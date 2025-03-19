# Dylan support for GNU Emacs

This repository contains two Emacs packages installable from
[MELPA](https://melpa.org/).

The `dylan` package (Dylan editing modes):

* `dylan.el` -- The `dylan-mode` major mode to edit Dylan code.
* `dylan-opt.el` -- The `dylan-opt-mode` minor mode to show compiler
  optimizations.
* `dylan-lid.el` -- The `dylan-lid-mode` major mode to edit LID files.

The `dime` package (Dylan interaction mode, an IDE derived from
[SLIME](https://common-lisp.net/project/slime/)):

* `dime.el` -- Interactive development environment.
* `dime-repl.el` -- Read-eval-print loop.
* `dime-browse.el` -- Class browser.
* `dime-note-tree.el` -- Compiler note browser.

## Setting up Dime

Dime relies on a backend, `dswank`. To configure Dime and `dswank`,
add these lines to your .emacs file, changing `YYYY.nn` as appropriate
for your installed release of Open Dylan::

```lisp
(dime-setup '(dime-repl dime-note-tree))
(setq dime-dylan-implementations
      '((opendylan ("/opt/opendylan-YYYY.nn/bin/dswank")
                    :env ("OPEN_DYLAN_USER_REGISTRIES=/opt/opendylan-YYYY.nn/sources/registry"))))
```

You will also want to add your own source registries to the
`OPEN_DYLAN_USER_REGISTRIES` environment variable. Registry paths are
separated by semicolons on Windows and colons elsewhere.
