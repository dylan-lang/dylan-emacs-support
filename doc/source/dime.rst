DIME - Dylan Interactor Mode for Emacs
======================================

DIME and its back-end, "dswank", create a link between the Dylan compiler and Emacs
so that editor commands can leverage everything the compiler knows about your source
code.  It allows you to view cross references, locate definitions, view argument lists,
compile your code, browse class hierarchies, and more.  This section gives a brief
introduction to using DIME.

The first thing you need in order to use DIME is the Emacs Lisp code for ``dylan-mode``,
which can be downloaded from https://github.com/dylan-lang/dylan-emacs-support.

..note:: You can download the ``dylan-emacs-support`` repository and build ``dswank``
         yourself to get the bleeding edge version, but the most recent stable version is
         distributed with Open Dylan so ``dswank`` should be available in your ``PATH``
         already and :file:`dime.el` and related files are available in the Open Dylan
         distribution in the :file:`sources/app` directory.

Next set up your Emacs init file.  Adjust the pathnames to match your Open Dylan
installation location or the directory where you put the `dylan-emacs-support
<https://github.com/dylan-lang/dylan-emacs-support>`_ repository.

.. code-block:: emacs-lisp

   (add-to-list 'load-path "/path/to/dylan-emacs-support")
   (require 'dime)
   (dime-setup '(dime-repl dime-note-tree))
   (setq dime-dylan-implementations
         '((opendylan ("/opt/opendylan/bin/dswank")
                      :env ("OPEN_DYLAN_USER_REGISTRIES=/path/to/your/registry"
                            "OPEN_DYLAN_USER_ROOT=/path/to/your/_build")
                      )))

Setting `OPEN_DYLAN_USER_REGISTRIES
<https://opendylan.org/getting-started-cli/source-registries.html#open-dylan-user-registries>`_
is important because that's how Open Dylan and DIME find your project sources.

Similarly, ``OPEN_DYLAN_USER_ROOT`` tells the compiler where to find your build products,
including the all-important compiler database that DIME queries.

.. note:: In the future we hope to have better support for `Deft
          <https://package.opendylan.org/deft/index.html>`_ workspaces, without the need
          to set any environment variables.  See `bug 76
          <https://github.com/dylan-lang/dylan-emacs-support/issues/76>`_.

For this tutorial we will use a "dime-test" project created with :program:`deft`.  Create
the project with ::

  $ cd /tmp
  $ deft new application dime-test

which creates a library named "dime-test" and a corresponding executable library and test
suite, as well as downloading dependencies and creating registry files.  See the `deft
new application <https://package.opendylan.org/deft/index.html#deft-new-application>`_
command for more info.

Do an initial build to create the compiler database::

  $ cd dime-test       # Created by deft new application
  $ deft build -a

Make sure DIME is configured correctly for this new project.  Here we'll **add** our new
project registry, ``/tmp/dime-test/registry``, to the list of user registries:

.. code:: emacs-lisp

   (setq dime-dylan-implementations
         '((opendylan ("/opt/opendylan/bin/dswank")
                      :env ("OPEN_DYLAN_USER_REGISTRIES=/path/to/your/registry:/tmp/dime-test/registry"
                            "OPEN_DYLAN_USER_ROOT=/path/to/your/_build")
                      )))

**Start Emacs and DIME:**  ::

  $ export PATH=/opt/opendylan/bin:$PATH
  $ emacs src/app/main.dylan
  M-x dime <Enter>

You should now have a buffer called ``*dime-repl nil*`` that looks like this::

  Welcome to dswank - the Open Dylan Version 2026.1 DIME interface
  opendylan>

This is the Open Dylan compiler interactive shell.  You can issue commands directly here
if you like, but mostly you'll issue DIME commands from your Dylan source buffers.

**Change projects:** Switch back to the :file:`main.dylan` buffer and type ``C-c M-p
dime-test-app`` to tell DIME to open the ``dime-test-app`` project.  If DIME doesn't let
you enter "dime-test-app" as the project name that means it couldn't find the registry
entry.  Make sure ``OPEN_DYLAN_USER_REGISTRIES`` (see above) is set correctly.

.. hint:: When entering the project name press tab to see a complete list of available
          projects and in the ``*dime-repl nil*`` buffer run the "show registries"
          command to see the active registries in the order they will be searched.

**Compile:** To build the project, type ``C-c C-k`` in the :file:`main.dylan` buffer.
You should see "Compilation finished: 1 note" (or similar) in the Emacs message line.

**Edit definition:** There's not much code in :file:`main.dylan` except for a ``main``
function.  Move the cursor onto the call to "format-out" and type ``M-.``.  It should
jump to the ``format-out`` definition in the ``io-internals`` module.

**Compiler warnings:** Switch back to the :file:`main.dylan` buffer using ``M-,`` and
make a change that causes a compiler warning, such as changing ``format-out`` to
``xformat-out``.  Recompile with ``C-c C-k`` and you should see something like
"Compilation finished: 1 warning, 1 note".  You can jump to the first warning using the
standard for Emacs: ``C-x ```.

**Argument lists:** Note that when you type an open parenthesis, or comma, or space after
a function name dime will display the **argument list** and return values in the Emacs
minibuffer.  e.g., try typing ``+(``.

**Cross references:** To list cross references (e.g., who calls function F?) move the
cursor over the name you want to look up and type ``C-c C-w C-c`` ('c' for call).  DIME
will display a list of callers in a ``*dime-xref*`` buffer.  ``C-M-.`` will take you to
the next caller.  Use it repeatedly to move to each caller definition in turn.  Move the
cursor to a particular caller in the ``*dime-xref*`` buffer and press <Enter> to jump to
that caller.

That should be enough to give you the flavor of DIME.  Following is a table of useful
commands, and you can of course find many more using the standard Emacs tools such as
``C-h b`` and ``M-x apropos``.

    +-------------------+------------------------------------------+
    | Keyboard shortcut | Effect                                   |
    +===================+==========================================+
    | M-x dime          | start dime                               |
    +-------------------+------------------------------------------+
    | , change-project  | change project (in the repl buffer)      |
    +-------------------+------------------------------------------+
    | C-c M-p           | change project (in Dylan source buffers) |
    +-------------------+------------------------------------------+
    | M-.               | jump to definition                       |
    +-------------------+------------------------------------------+
    | M-,               | jump backwards (return from definition)  |
    +-------------------+------------------------------------------+
    | C-c C-k           | compile project                          |
    +-------------------+------------------------------------------+
    | C-c C-w C-a       | who specializes? (or who defines?)       |
    +-------------------+------------------------------------------+
    | C-c C-w C-r       | who references?                          |
    +-------------------+------------------------------------------+
    | C-c C-w C-b       | who binds?                               |
    +-------------------+------------------------------------------+
    | C-c C-w C-c       | who calls?                               |
    +-------------------+------------------------------------------+
