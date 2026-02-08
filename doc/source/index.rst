*******************
dylan-emacs-support
*******************

This repository contains two Emacs packages installable from `MELPA
<https://melpa.org/>`_.

The `dylan <https://melpa.org/#/dylan>`_ package (Dylan editing modes):

* :file:`dylan.el` -- The ``dylan-mode`` major mode to edit Dylan code.
* :file:`dylan-opt.el` -- The ``dylan-opt-mode`` minor mode to show compiler optimizations.
* :file:`dylan-lid.el` -- The ``dylan-lid-mode`` major mode to edit LID files.

**TODO:**  document the above modes!

The `dime <https://melpa.org/#/dime>`_ package (Dylan Interactor Mode for Emacs), an IDE
derived from `SLIME <https://common-lisp.net/project/slime/>`_:

* :file:`dime.el` -- Interactive development environment.
* :file:`dime-repl.el` -- Read-eval-print loop.
* :file:`dime-browse.el` -- Class browser.
* :file:`dime-note-tree.el` -- Compiler note browser.
* ``dswank`` -- Server process that controls the Open Dylan compiler.

See :doc:`dime`.

.. note:: The LSP server provided by the `lsp-dylan
          <https://package.opendylan.org/lsp-dylan>`_ repository is an alternative that
          provides similar functionality.  Both ``DIME`` and ``lsp-dylan`` use the same
          interfaces to the Open Dylan compiler.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   dime
