Dylan-mode and dime
-------------------

dswank is part of the release since opendylan-2011.1

Set OPEN_DYLAN_USER_REGISTRIES environment variable to point to your registry[ies]:

  OPEN_DYLAN_USER_REGISTRIES=/path/to/opendylan/sources/registry

To enable auto-loading of dylan-mode upon loading a .dylan file, add these
lines to your .emacs file::

  (setq load-path (cons "/path/to/dylan-mode" load-path))
  (autoload 'dylan-mode "dylan-mode" "Dylan-mode" t)
  (setq auto-mode-alist (cons '("\\.dylan\\'" . dylan-mode) auto-mode-alist))
  (setq inferior-dylan-program "/opt/opendylan-2011.1/bin/dswank") ; your dswank binary
  (require 'dime)
  (dime-setup '(dime-dylan dime-repl dime-compiler-notes-tree))

dime is a fork of slime.

License
-------

This code is distributed under the GNU GPL.

It originates from different sources:
 dylan-mode.el is from CMU
 dylan-optimization-coloring.el from Harlequin
 dylan-dime.el from Dylan Hackers
 dime.el, dime-repl.el, dime-compiler-notes-tree.el from SLIME


Enscript Support
----------------

The file dylan.st adds Dylan support to Enscript.  This file is now included
with Enscript (circa version 1.6.5).
