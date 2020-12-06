Module: dylan-mode-test
Synopsis: This file can be used to test dylan-mode indentation, and to some extent
          the highlighting code.  It's not meant to be compiled.  Pressing the Tab
          key on any line in this file should not cause any changes; if it does it's
          a bug.

// *** Pressing Tab on the previous line (i.e., at the end of the header) should indent
//     to just below "a bug."

// Notes for developers:
// * It may be useful to customize the dylan group during development to make it easier
//   to see the effects of your changes. e.g., setting dylan-continuation-indent to 8.
// * Lines marked with "// ***" currently have known problems, i.e., they change when
//   tabbed. Note that in some cases adding this comment may itself have impact on the
//   indentation of subsequent lines, which is also a bug.

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

define method arglist-on-same-line (foo, bar, #rest foo)
 => (#rest values)
  test();
  test();
  block ()
    foo();
  end;
end;

define function arglist-on-multiple-lines
    (arg1 :: <string>, #rest args,
     #key key1 :: false-or(<x>) = #f,
          key2 = 2,             // *** key2, key3 should colorize
          key3 = 3)
 => ()
  foo();
  bar();
  block (return)
    let v = really-long-function-name-that-goes-on-for-miles(
              arg1, arg2,         // ***
              arg3, arg4);        // ***
    let v = fn(arg1, arg2,
               arg3, arg4);
    if (thing)
      let (super, extra, long, variable, list)
        = fn();
    else
      other-thing()
    end;
    let v = #[a,
              b,
              c];
    let foo = #[
      a,                          // ***
      b                           // ***
    ];
    let foo = #[a,
                b];
  end block;
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

define test foo (option: bar)
  assert-true(#t);              // ***
  assert-false(#f);
end;

define inline function test-highlight-inline () end;
define may-inline function test-highlight-may-inline () end;
define not-inline function test-highlight-not-inline () end;
define inline-only function test-highlight-inline-only () end;
define default-inline function test-highlight-default-inline () end;

define function foo ()
  with-open-file (stream = "/tmp/foo") end;
  without-interrupts () body() end;
  printing-object (o, s) end;
  doing-this () end;
end function;
