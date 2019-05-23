Module: dylan-mode-test
Synopsis: This file can be used to test dylan-mode indentation, and to some extent
          the highlighting code.  It's not meant to be compiled.  Pressing the Tab
          key on any line in this file should not cause any changes; if it does it's
          a bug.

// *** Pressing Tab on the previous line (i.e., at the end of the header) should indent
//     to just below "a bug."

// Lines marked with *** currently have known problems, i.e., they change when tabbed.
// Note that in some cases adding this comment may itself have impact on the indentation
// of subsequent lines.

// TODO(cgay): add some code that is indented with non-default settings.  All code below
// assumes default settings.

define library foo
  use lib1;
  use lib2,
    import: {
      mod1,                     // ***
      mod2                      // ***
    },                          // ***
    rename: {
      mod1 => mod3,             // ***
      mod2 => mod1              // ***
    };
end;

define module foo
  use mod1;
  use mod2, import: {
              mod1,             // ***
              mod2              // ***
            },                  // ***
            rename: {
              mod1 => mod3,     // ***
              mod2 => mod1      // ***
            };                  // ***
  create
    foo,
    bar;
  create foo,
         bar;                   // ***
end;

define method arglist-on-same-line (#key foo = 1, bar = 2,
                                         baz = 3) // ***
 => (#rest values)              // *** (works if no comment on previous line)
  test();                       // ***
  test();
end;


define function foo
    ()  // *** this comment causes next line to break
 => ()
  block ()
    foo();
  end;
end;

define function arglist-on-multiple-lines
    (arg1 :: <string>, #rest args,
     #key key1 :: false-or(<x>) = #f,
          key2 = 2,
          key3 = 3)             // ***
 => ()                          // *** (works when arglist is just ())
  foo();                        // ***
  bar();
  let v = really-long-function-name-that-goes-on-for-miles(
            arg1, arg2,         // ***
            arg3, arg4);        // ***
  let v = fn(arg1, arg2,
             arg3, arg4);
  let (super, extra, long, variable, list)
    = fn();
  let v = #[a,
            b,
            c];
  let long-variable-name = #[
    a,                          // ***
    b                           // ***
  ];
  local
    method foo () => ()
      foo(bar,
          test(1, 2,
               3, 4),
          baz);
    end,
    method bar () 8 end,
    method baz (x)
      x
    end;
  local method foo (x)
          x
        end,
        method bar (x) x end;   // ***
  // test
  if (this("a" "b"))
    x;
    y;
    z
  elseif (f())
    that
  end;
  select (x)
    'x' => test;
           test;                // ***
    'y' =>
      test;
      test;
    otherwise =>
      test;
  end;
  case
    foo => test;
           test;                // ***
    bar =>
      test;
      test;
  end;
end function foo;

// Below is just an idea. I would like "slot" to line up so it's easier to see
// the names of the slots and which ones are constant/virtual/each-subclass.
// OTOH maybe syntax highlighting is good enough.
define open abstract class <flight> (<object>)
  constant slot itinerary-variations :: <sequence>,
             init-keyword: itinerary-variations:,
             init-value: 21;
           slot carrier, required-init-keyword: carrier:;  // ***
end;

define macro foo-definer
    { define foo ?class:name = ?expr:expression }  // ***
 => { define class "<" ## ?class ## ">" (<object>) // ***
        slot one = ?expr;
        slot two = 2;
      end
    }                           // ***

    { stuff }
 => { define function foo       // ***
          ()
       => (test)
      end;

 clauses:                       // ***
    { stuff }
 => { other stuff }             // ***
    
end;
