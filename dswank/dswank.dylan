module:    dswank
synopsis:  swank support for opendylan. swank is the protocol of SLIME
author:    Andreas Bogk and Hannes Mehnert
copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

// Why is this not called DIMED or DANK?  Missed opportunity.

// The user must open a project with dime-repl-set-project.  Always read this through
// current-repl-project().  There is one active project at a time, even if more than one
// has been opened in the compiler.
//
// TODO(cgay): Maybe we can use ep/current-project instead?
define thread variable *project* :: false-or(ep/<project-object>) = #f;

define function current-repl-project () => (project :: ep/<project-object>)
  *project*
    | error("No project has been set, try dime-repl-set-project.")
end function;

// TODO(cgay): Not yet sure under what circumstances this variable can be expected to be
// non-false. Why is this necessary at all? I would think *project* suffices and every
// call to a swank-function should include the current buffer's filename and the value of
// the Module: header so that the module and library can be found.
define thread variable *module-name* :: false-or(<string>) = #f;

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

define emacs-command emacs-rex (command, module-name, thread-id, request-id)
  block ()
    debug-to-repl("emacs-rex(%=, %=, %=, %=)", command, module-name, thread-id, request-id);
    let function = $swank-functions[command.head];
    *module-name* := module-name;
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

define swank-function set-project (project-name)
  run-compiler-command(concatenate("open ", project-name));
  *project* := (ep/find-project(project-name)
                  | error("project %= not found", project-name));
  list(project-name, project-name) // #(name, prompt-string)
end;

define swank-function default-directory ()
  as(<string>, working-directory())
end;

define swank-function set-default-directory (new-directory)
  working-directory-setter(expand-pathname(new-directory));
  new-directory
end;

define swank-function compile-file-for-emacs (filename, #rest foo)
  let project = find-project-for-file(filename)
                  | error("no project found for %=", filename);
  run-compiler-command(concatenate("build ", ep/project-name(project)));
  let notes = compiler-notes-for-emacs();
  //result is output-pathname, notes, success/failure of compilation
  list("pathname", notes, "T")
end;

define function find-project-for-file
    (filename :: <string>) => (project :: false-or(ep/<project-object>))
  block (return)
    for (proj in ep/open-projects())
      for (source in proj.ep/project-sources)
        if (source.sr/source-record-location = filename)
          return(proj);
        end;
      end;
    end;
    #f
  end
end function;

define swank-function compiler-notes-for-emacs ()
  let project = current-repl-project();
  let warnings = ep/project-warnings(project);
  let res = make(<stretchy-vector>);
  for (w in warnings)
    let message = ep/compiler-warning-full-message(project, w);
    let short-message = ep/compiler-warning-short-message(project, w);
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

define swank-function describe-symbol (dylan-name)
  let project = current-repl-project();
  let (object, module) = find-object(dylan-name);
  ep/environment-object-description(project, object, module)
end;

define function find-object
    (dylan-name :: <string>)
 => (object :: false-or(ep/<environment-object>),
     module :: false-or(ep/<module-object>),
     library :: false-or(ep/<library-object>))
  block (return)
    // First check the current project, then check all open projects, so that if
    // dylan-name is unqualified and exists in two modules the current project wins.
    if (*project*)
      let (obj, mod, lib) = find-object-in-project(*project*, dylan-name);
      if (obj)
        return(obj, mod, lib)
      end;
    end;
    for (project in ep/open-projects())
      if (project ~== *project*)
        let (obj, mod, lib) = find-object-in-project(project, dylan-name);
        if (obj)
          return(obj, mod, lib);
        end;
      end;
    end;
  end
end function;

define function find-object-in-project
    (project :: ep/<project-object>, dylan-name :: <string>)
 => (object :: false-or(ep/<environment-object>),
     module :: false-or(ep/<module-object>),
     library :: false-or(ep/<library-object>))
  block (return)
    let (object-name, module-name, library-name)
      = apply(values, split(dylan-name, ':'));
    local
      method check-module (proj, lib, mod)
        if (~module-name
              | ep/environment-object-basic-name(proj, mod) = module-name)
          let obj = ep/find-environment-object(proj, object-name,
                                               library: lib, module: mod);
          if (obj)
            return(obj, mod, lib);
          end;
        end;
      end,
      method check-library (proj, lib)
        // Find object-name[:module-name] in lib...
        let module = module-name & ep/find-module(project, module-name, library: lib);
        if (module)
          check-module(proj, lib, module)
        else
          for (module in ep/library-modules(proj, lib, imported?: #t))
            check-module(proj, lib, module);
          end;
        end;
      end;
    let library = library-name & ep/find-library(project, library-name);
    if (library)
      // A library was explicitly specified so that's the only one we check.
      check-library(project, library)
    else
      let library = ep/project-library(project);
      check-library(project, library)
        | ep/do-project-used-libraries(method (proj, lib)
                                         let (obj, mod, lib) = check-library(proj, lib);
                                         if (obj)
                                           return(obj, mod, lib);
                                         end;
                                       end,
                                       project, project)
    end
  end block
end function;

define function get-location-as-sexp (search, env-obj)
  let project = current-repl-project();
  let location = ep/environment-object-source-location(project, env-obj);
  if (location)
    let source-name
      = ep/print-environment-object-location(project, env-obj, absolute-path?: #t);
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

define swank-function find-definitions-for-emacs (dylan-name)
  let env-obj = find-object(dylan-name);
  list(get-location-as-sexp(#f, env-obj))
end;

define swank-function listener-eval (arg)
  run-compiler-command(arg);
  list(#":values", "Done.")
end;

define function get-name (o :: ep/<environment-object>) => (res :: <string>)
  let project = current-repl-project();
  block ()
    ep/environment-object-primitive-name
      (project, ep/environment-object-home-name(project, o))
  exception (c :: <condition>)
    ep/environment-object-display-name(project, o, #f, qualify-name?: #f)
  end
end function;

define method snippet (o :: <object>) => (res :: <string>)
  get-name(o)
end method;

define method snippet (function :: ep/<dylan-function-object>) => (res :: <string>)
  let project = current-repl-project();
  let required = ep/function-parameters(project, function);
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
  let project = current-repl-project();
  let specializers = ep/domain-specializers(project, domain);
  concatenate("D ", domain.get-name, " (",
              if (empty?(specializers))
                ""
              else
                reduce1(method(a, b) concatenate(a, ", ", b) end,
                        map(get-name, specializers))
              end, ")")
end method;

define method snippet (class :: ep/<class-object>) => (res :: <string>)
  let project = current-repl-project();
  let supers = make(<stretchy-vector>);
  ep/do-direct-superclasses(compose(curry(add!, supers), get-name), project, class);
  concatenate("C ", class.get-name, " (",
              if (empty?(supers))
                ""
              else
                reduce1(method(a, b) concatenate(a, ", ", b) end,
                        supers)
              end, ")")
end method;

define method snippet (zlot :: ep/<slot-object>) => (res :: <string>)
  let project = current-repl-project();
  concatenate("S ", zlot.get-name, " :: ", ep/slot-type(project, zlot).get-name)
end method;

define swank-function xref (kind :: <symbol>, name :: <string>)
  let function = $xref-functions[kind];
  let env-obj = find-object(name);
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
  ep/source-form-clients(current-repl-project(), env-obj)
end;

define xref-function references (env-obj)
  ep/source-form-clients(current-repl-project(), env-obj)
end;

define xref-function sets (env-obj)
  // TODO: returns all references, needs to find actual setters
  ep/source-form-clients(current-repl-project(), env-obj)
end;

define xref-function binds (env-obj)
  // TODO: returns all references, needs to find actual setters
  ep/source-form-clients(current-repl-project(), env-obj)
end;

define xref-function macroexpands (env-obj)
  ep/macro-call-source-forms(current-repl-project(), env-obj)
end;

define xref-function specializes (function)
  let project = current-repl-project();
  let generic
    = select (function by instance?)
        ep/<generic-function-object> => function;
        ep/<method-object>           => ep/method-generic-function(project, function);
        otherwise                    => #f;
      end;
  // TODO: Why is one explicitly returning a vector and the other a list?
  if (generic)
    concatenate-as(<vector>,
                   vector(generic),
                   ep/generic-function-object-methods(project, generic))
  else
    #()
  end
end;

define xref-function callers (env-obj)
  ep/source-form-clients(current-repl-project(), env-obj)
end;

define xref-function callees (env-obj)
  // TODO: filter for function definitions
  ep/source-form-used-definitions(current-repl-project(), env-obj)
end;

define swank-function operator-arglist (dylan-name, module-name)
  // TODO(cgay): module-name is unused.
  let project = current-repl-project();
  let (object, module) = find-object(dylan-name);
  if (object)
    concatenate(ep/print-function-parameters(project, object, module),
                " => ",
                ep/print-function-values(project, object, module))
  else
    #"nil"
  end
end;

define function get-names (env-objs)
  let project = current-repl-project();
  let module = ep/find-module(project, *module-name*);
  sort!(map(rcurry(curry(ep/environment-object-display-name, project),
                   module, qualify-names?: #t),
            env-objs))
end function;

define swank-function dylan-subclasses (dylan-name)
  let env-obj = find-object(dylan-name);
  get-names(ep/class-direct-subclasses(current-repl-project(), env-obj))
end;

define swank-function dylan-superclasses (dylan-name)
  let env-obj = find-object(dylan-name);
  get-names(ep/class-direct-superclasses(current-repl-project(), env-obj))
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
                   concatenate("*** DEBUG: ", format-string, "\n"),
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
    start-compiler(stream);
    write-to-emacs(stream, list(#":write-string",
                                concatenate("Welcome to dswank - the ",
                                            release-info/release-full-name(),
                                            " DIME interface\n")));
    while (#t)
      let length = string-to-integer(read(stream, 6), base: 16);
      let line = read(stream, length);
      let expr = read-lisp(make(<string-stream>,
                                direction: #"input", contents: line));
      let function = $emacs-commands[expr.head];
      let result = apply(function, expr.tail);
      write-to-emacs(stream, result);
    end;
  end;
end function main;

main(application-arguments());
