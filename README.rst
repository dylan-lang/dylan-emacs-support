dylan-mode
==========

dylan-mode is a major mode for editing Dylan code in emacs.  To enable
auto-loading of dylan-mode upon loading a .dylan file, add these lines
to your .emacs file::

  (add-to-list 'load-path "/path/to/dylan-mode")
  (autoload 'dylan-mode "dylan-mode" "Dylan-mode" t)
  (add-to-list 'auto-mode-alist '("\\.dylan\\'" . dylan-mode))


DIME/dswank
===========

DIME/dswank is part of the release since opendylan-2011.1.  It
provides interactive development support in emacs.  DIME is a fork of
`SLIME <http://common-lisp.net/project/slime/>`_ and stands for Dylan
Interaction Mode for Emacs.

To enable DIME/dswank add these lines to your .emacs file, changing
YYYY.nn as appropriate for your installed release of Open Dylan::

  (setq inferior-dylan-program "/opt/opendylan-YYYY.nn/bin/dswank") ; your dswank binary
  (require 'dime)
  (dime-setup '(dime-dylan dime-repl dime-compiler-notes-tree))

You will also need to set OPEN_DYLAN_USER_REGISTRIES to your source registries
so that the compiler can find library dependencies::

  export OPEN_DYLAN_USER_REGISTRIES=/path/to/opendylan/sources/registry:/other/registry

In Windows this can be set via the System control panel, and
registries must be separated with semicolons instead of colon.


etags.regex
===========

This file may be used to create an emacs TAGS file for Dylan code.  See the
comment in the file for usage.

dtags
=====

This script uses the `Gema <http://gema.sourceforge.net>`_ macro
processor to generate an emacs TAGS file.


License
=======

This code is distributed under the GNU GPL.

It originates from different sources:
 dylan-mode.el is from CMU
 dylan-optimization-coloring.el from Harlequin
 dylan-dime.el, dylan-common.el from Dylan Hackers
 dime.el, dime-repl.el, dime-compiler-notes-tree.el from SLIME


