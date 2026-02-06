module:    dswank
synopsis:  swank support for opendylan. swank is the protocol of SLIME
author:    Andreas Bogk and Hannes Mehnert
copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

// Why is this not called DIMED or DANK?  Missed opportunity.

define thread variable *server* = #f;
define thread variable *project* = #f;
define thread variable *library* = #f;
define thread variable *module* = #f;

define constant $emacs-commands = make(<table>);

define macro emacs-command-definer
    { define emacs-command ?:name (?args:*) ?:body end }
 => {
      define function ?name (?args);
        ?body
      end;
      $emacs-commands[":" ## ?#"name"] := ?name
    }
end macro;

define emacs-command emacs-rex (command, package, thread-id, request-id)
  block ()
    let function = $swank-functions[command.head];
    *module* := package;
    let result = apply(function, command.tail);
    list(#":return", list(#":ok", result), request-id)
  exception (err :: <error>)
    list(#":return", list(#":abort", format-to-string("%=", err)), request-id)
  end
end;

// Map symbols of the form #"swank:xxx" to functions.
define constant $swank-functions = make(<table>);

define macro swank-function-definer
    { define swank-function ?:name (?args:*) ?:body end }
 => {
      define function ?name (?args)
        ?body
      end;
      $swank-functions["swank:" ## ?#"name"] := ?name
    }
end macro;

define swank-function connection-info ()
  list(#":pid", 23,             // TODO
       #":style", #":fd-handler",
       #":lisp-implementation", list(#":type", "dylan",
                                     #":name", release-info/release-product-name(),
                                     #":version", release-info/release-version()),
       #":version", "2011-02-13", // TODO
       #":package", #(#":name", "opendylan",
                      #":prompt", "opendylan"))
end;

define swank-function create-repl (x)
  list("opendylan", "opendylan")
end;

define swank-function quit-lisp ()
  exit-application(0);
end;

define swank-function list-all-package-names (t)
  // `t` is always #t.  Get rid of it?
  let libraries = #();
  local
    method collect-project
        (dir :: <pathname>, filename :: <string>, type :: <file-type>)
      if (type == #"file")
        if (last(filename) ~== '~')
          libraries := pair(filename, libraries);
        end;
      end;
    end method;
  let regs = registry-projects/find-registries(as(<string>,
                                                  build-system/target-platform-name()));
  let reg-paths = map(registry-projects/registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
  libraries
end;

define swank-function set-package (package-name)
  run-compiler(*server*, concatenate("open ", package-name));
  *project* := ep/find-project(package-name);
  list(package-name, package-name) // #(name, prompt-string)
end;

define swank-function default-directory ()
  as(<string>, working-directory())
end;

define swank-function set-default-directory (new-directory)
  working-directory-setter(expand-pathname(new-directory));
  new-directory
end;

define swank-function compile-file-for-emacs (filename, #rest foo)
  block (done)
    for (proj in ep/open-projects())
      for (source in proj.ep/project-sources)
        if (source.sr/source-record-location = filename)
          *project* := proj;
          done();
        end;
      end;
    end;
  end;
  if (*project*)
    run-compiler(*server*, concatenate("build ", *project*.ep/project-name));
    let notes = compiler-notes-for-emacs();
    //result is output-pathname, notes, success/failure of compilation
    list("pathname", notes, "T");
  else
    error("A project has not been set");
  end
end;

define swank-function compiler-notes-for-emacs ()
  let warnings = ep/project-warnings(*project*);
  let res = make(<stretchy-vector>);
  for (w in warnings)
    let message = ep/compiler-warning-full-message(*project*, w);
    let short-message = ep/compiler-warning-short-message(*project*, w);
    let location = get-location-as-sexp(#f, w).tail.head;
    let severity = if (instance?(w, ep/<compiler-error-object>))
                     #":error"
                   elseif (instance?(w, ep/<serious-compiler-warning-object>))
                     #":warning"
                   else
                     #":note"
                   end;
    let warning = list(#":message", message,
                       #":severity", severity,
                       #":location", location,
                       #":references", #(),
                       #":short-message", short-message);
    res := add!(res, warning);
  end;
  res
end;

define swank-function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name);
  ep/environment-object-description(*project*, env, *module*)
end;

define function get-environment-object (symbols)
  let env = split(symbols, ":");
  let symbol-name = env[0];
  let library = #f;
  let module = #f;
  let project = #f;
  if (env.size == 3)
    project := *project*;
    library := ep/find-library(project, env[2]);
    module := ep/find-module(project, env[1], library: library);
  end;
  local method check-and-set-module (p, lib)
          unless(module)
            module := ep/find-module(p, *module*, library: lib);
            if (module)
              library := lib;
            end;
          end;
        end;
  for (p in ep/open-projects())
    unless (project)
      check-and-set-module(p, ep/project-library(p));
      ep/do-project-used-libraries(curry(check-and-set-module, p), p, p);
      if (library)
        project := p;
      end;
    end;
  end;
  if (env.size == 1)
    *project* := project;
    *library* := library;
    *module* := module;
  end;
  ep/find-environment-object(project, symbol-name,
                             library: library,
                             module: module)
end function;

define function get-location-as-sexp (search, env-obj)
  let location = ep/environment-object-source-location(*project*, env-obj);
  if (location)
    let source-name
      = ep/print-environment-object-location(*project*,
                                             env-obj,
                                             absolute-path?: #t);
    let (name, lineno)
      = sr/source-line-location(location.sr/source-location-source-record,
                                location.sr/source-location-start-line);
    let column = location.sr/source-location-start-column;
    let snipp = snippet(env-obj) | search;
    list(snipp | name,
         list(#":location",
              list(#":file", as(<string>, source-name)),
              list(#":line", lineno, column),
              if (search)
                list (#"snippet", search)
              else
                #()
              end));
  else
    list(#"unknown", #(#":error", "No error location available"))
  end
end function;

define swank-function find-definitions-for-emacs (symbol-name)
  let env-obj = get-environment-object(symbol-name);
  list(get-location-as-sexp(#f, env-obj))
end;

define swank-function listener-eval (arg)
  run-compiler(*server*, arg);
  list(#":values", "Done.")
end;

define function get-name (o :: ep/<environment-object>) => (res :: <string>)
  block ()
    ep/environment-object-primitive-name
      (*project*,
       ep/environment-object-home-name(*project*, o))
  exception (c :: <condition>)
    ep/environment-object-display-name(*project*, o, #f, qualify-name?: #f)
  end
end function;

define method snippet (o :: <object>) => (res :: <string>)
  get-name(o)
end method;

define method snippet (function :: ep/<dylan-function-object>) => (res :: <string>)
  let required = ep/function-parameters(*project*, function);
  concatenate("M ", next-method(), " (",
              if (empty?(required))
                ""
              else
                reduce1(method (a, b)
                          concatenate(a, ", ", b)
                        end,
                        map(compose(get-name, ep/parameter-type),
                            required))
              end,
              ")")
end method;

define method snippet (generic :: ep/<generic-function-object>) => (res :: <string>)
  concatenate("G ", generic.get-name)
end method;

define method snippet (domain :: ep/<domain-object>) => (res :: <string>)
  let specializers = ep/domain-specializers(*project*, domain);
  concatenate("D ", domain.get-name, " (",
              if (empty?(specializers))
                ""
              else
                reduce1(method(a, b) concatenate(a, ", ", b) end,
                        map(get-name, specializers))
              end, ")")
end method;

define method snippet (class :: ep/<class-object>) => (res :: <string>)
  let supers = make(<stretchy-vector>);
  ep/do-direct-superclasses(compose(curry(add!, supers), get-name), *project*, class);
  concatenate("C ", class.get-name, " (",
              if (empty?(supers))
                ""
              else
                reduce1(method(a, b) concatenate(a, ", ", b) end,
                        supers)
              end, ")")
end method;

define method snippet (zlot :: ep/<slot-object>) => (res :: <string>)
  concatenate("S ", zlot.get-name, " :: ", ep/slot-type(*project*, zlot).get-name)
end method;

define swank-function xref (kind :: <symbol>, name :: <string>)
  let function = $xref-functions[kind];
  let env-obj = get-environment-object(name);
  let result = function(env-obj);
  map(curry(get-location-as-sexp, name), result)
end;

define swank-function xrefs (kinds, name :: <string>)
  error("swank:xrefs not yet implemented (%s, %s)", kinds, name);
end;

define constant $xref-functions = make(<table>);

define macro xref-function-definer
    { define xref-function ?:name (?args:*) ?:body end }
 => {
      define function ?name (?args); ?body end;
      $xref-functions[":" ## ?#"name"] := ?name
    }
end macro;

define xref-function calls (env-obj)
  ep/source-form-clients(*project*, env-obj)
end;

define xref-function references (env-obj)
  ep/source-form-clients(*project*, env-obj)
end;

define xref-function sets (env-obj)
  // TODO: returns all references, needs to find actual setters
  ep/source-form-clients(*project*, env-obj)
end;

define xref-function binds (env-obj)
  // TODO: returns all references, needs to find actual setters
  ep/source-form-clients(*project*, env-obj)
end;

define xref-function macroexpands (env-obj)
  ep/macro-call-source-forms(*project*, env-obj)
end;

define xref-function specializes (function)
  let generic
    = select (function by instance?)
        ep/<generic-function-object> => function;
        ep/<method-object>           => ep/method-generic-function(*project*, function);
        otherwise                    => #f;
      end;
  // TODO: Why is one explicitly returning a vector and the other a list?
  if (generic)
    concatenate-as(<vector>,
                   vector(generic),
                   ep/generic-function-object-methods(*project*, generic))
  else
    #()
  end
end;

define xref-function callers (env-obj)
  ep/source-form-clients(*project*, env-obj)
end;

define xref-function callees (env-obj)
  // TODO: filter for function definitions
  ep/source-form-used-definitions(*project*, env-obj)
end;

define swank-function operator-arglist (symbol, package)
  let env-obj = get-environment-object(symbol);
  if (env-obj)
    concatenate(ep/print-function-parameters(*project*, env-obj, *module*),
                " => ",
                ep/print-function-values(*project*, env-obj, *module*))
  else
    #"nil"
  end
end;

define function get-names (env-objs)
  let module = if (instance?(*module*, <string>))
                 ep/find-module(*project*, *module*)
               else
                 *module*
               end;
  sort!(map(rcurry(curry(ep/environment-object-display-name, *project*),
                   module, qualify-names?: #t),
            env-objs))
end function;

define swank-function dylan-subclasses (symbols)
  let env-obj = get-environment-object(symbols);
  get-names(ep/class-direct-subclasses(*project*, env-obj))
end;

define swank-function dylan-superclasses (symbol)
  let env-obj = get-environment-object(symbol);
  get-names(ep/class-direct-superclasses(*project*, env-obj))
end;

define function write-to-emacs (stream, s-expression)
  let newstream = make(<string-stream>, direction: #"output");
  print-s-expression(newstream, s-expression);
  let s-expression = stream-contents(newstream);
  //format-err("write: %s\n", s-expression);
  let len = integer-to-string(s-expression.size, base: 16, size: 6);
  format(stream, "%s%s", len, s-expression);
end function;

define thread variable *dswank-stream* = #f;

// Send debug output to the REPL. Of course it can also be found in the *dime-events*
// buffer.
define function debug-to-repl (format-string :: <string>, #rest format-args)
  let text = apply(format-to-string,
                   concatenate("*** DEBUG: ", format-string),
                   format-args);
  write-to-emacs(*dswank-stream*, list(#":write-string", text));
end function;

define function main (args)
  start-sockets();
  let tmpfile = #f;
  let port = 4005;
  unless (args.size >= 1 & args[0] = "--listen")
    let line = read-line(*standard-input*);
    let sexp = read-lisp(make(<string-stream>,
                              direction: #"input", contents: line));
    tmpfile := block (ret)
                 for (call in sexp.tail)
                   if (call.head == #"funcall")
                     if (call.tail.head.tail.head = "swank:start-server")
                       ret(call[2])
                     end;
                   end;
                 end;
                 error("error parsing swank startup command");
               end;
  end unless;
  local method open ()
          block ()
            let socket = make(<server-socket>, port: port);
            if (tmpfile)
              with-open-file (file = tmpfile, direction: #"output")
                write(file, integer-to-string(port));
              end;
            end;
            socket
          exception (err :: <error>)
            port := port + 1;
            open()
          end
        end;
  let socket = open();
  let stream = accept(socket);
  dynamic-bind (*dswank-stream* = stream)
    *server* := start-compiler(stream);
    let greeting = concatenate("Welcome to dswank - the ",
                               release-info/release-full-name(),
                               " DIME interface\n");
    write-to-emacs(stream, list(#":write-string", greeting));
    while (#t)
      let length = string-to-integer(read(stream, 6), base: 16);
      let line = read(stream, length);
      debug-to-repl("read: %s", line);
      let expr = read-lisp(make(<string-stream>,
                                direction: #"input", contents: line));
      let function = $emacs-commands[expr.head];
      let result = apply(function, expr.tail);
      write-to-emacs(stream, result);
    end;
  end;
end function main;

main(application-arguments());
