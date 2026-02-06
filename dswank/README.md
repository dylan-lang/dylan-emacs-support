# Notes on Hacking dswank

* Build with `deft update; deft build dswank`

* When dswank runs the compiler it needs the Jam build scripts. When dswank is built as
  part of an Open Dylan release it finds the scripts in
  `INSTALL-DIR/share/opendylan/build-scripts`.  When building and running your own dswank
  you must copy those files to your `_build` directory.  For example, `cp -rp
  /opt/opendylan/share /path/to/dylan-emacs-support/_build/`.

* Configure DIME so that it uses your dswank executable and so it can find whatever
  project you're using for testing DIME (I usually use dswank itself). Example:

  ```elisp
  (require 'dime)
  (dime-setup '(dime-repl dime-note-tree))
  (setq dime-dylan-implementations
        `((opendylan ("/home/me/dylan/workspaces/dylan-emacs-support/_build/bin/dswank")
                     ;; The registry setting should be found dynamically but for now
                     ;; must be set specifically for the code I'm working on at the
                     ;; moment.
                     :env ("OPEN_DYLAN_USER_REGISTRIES=/home/me/dylan/workspaces/dylan-emacs-support/registry"))))
  ```

* The easiest way to add debug output to the dswank Dylan code is to call
  `debug-to-repl`. The output shows up in the `*dime-repl nil*` buffer.
