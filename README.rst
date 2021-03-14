Dylan support for GNU Emacs
===========================

* `dylan` -- The `dylan-mode` major mode to edit Dylan code.
* `dylan-opt` -- The `dylan-opt-mode` minor mode to show compiler optimizations.
* `dylan-lid` -- The `dylan-lid-mode` major mode to edit LID files.

* `dime` -- Interactive development environment (derived from `SLIME
  <https://common-lisp.net/project/slime/>`).
* `dime-repl` -- Read-eval-print loop.
* `dime-browse` -- Class browser.
* `dime-note-tree` -- Compiler note browser.

Setting up Dime
===============

Dime relies on a backend, `dswank`. To configure Dime and `dswank`,
add these lines to your .emacs file, changing `YYYY.nn` as appropriate
for your installed release of Open Dylan::

  (dime-setup '(dime-dylan dime-repl dime-compiler-notes-tree))
  (setq dime-dylan-implementations
        '((opendylan ("/opt/opendylan-YYYY.nn/bin/dswank")
                     :env ("OPEN_DYLAN_USER_REGISTRIES=/opt/opendylan-YYYY.nn/sources/registry"))))

You will also want to add your own source registries to the
`OPEN_DYLAN_USER_REGISTRIES` environment variable. Registry paths are
separated by semicolons on Windows and colons elsewhere.
