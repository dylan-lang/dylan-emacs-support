Dylan-mode elisp files
----------------------

To enable auto-loading of dylan-mode upon loading a .dylan file, add these
lines to your .emacs file::

  (setq load-path (cons "/usr/lib/dylan/elisp" load-path))
  (autoload 'dylan-mode "dylan-mode" "Dylan-mode" t)
  (setq auto-mode-alist (cons '("\\.dylan\\'" . dylan-mode) auto-mode-alist))


Dylan-slime interface
---------------------

dswank is part of the release since opendylan-2011.1

Get slime from

  http://opendylan.org/~hannes/slime.tar.gz (tarball from February 2011)

Set OPEN_DYLAN_USER_REGISTRIES environment variable to point to your registry/ies:

  OPEN_DYLAN_USER_REGISTRIES=/path/to/opendylan/sources/registry

Add the following lines to your .emacs file::

  (add-to-list 'load-path "/path/to/slime/")  ; your SLIME directory
  (setq inferior-lisp-program "/opt/opendylan-2011.1/bin/dswank") ; your dswank binary
  (require 'slime)
  (slime-setup '(slime-dylan slime-repl))


Enscript Support
----------------

The file dylan.st adds Dylan support to Enscript.  This file is now included
with Enscript (circa version 1.6.5).
