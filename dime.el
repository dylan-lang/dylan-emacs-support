;;; dime.el --- Superior Dylan Interaction Mode for Emacs
;;
;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;     For a detailed list of contributors, see the manual.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;;;; Commentary
;;
;; This file contains extensions for programming in Open Dylan. The
;; main features are:
;;
;;   A socket-based communication/RPC interface between Emacs and
;;   Dylan, enabling introspection and remote development.
;;
;;   The `dime-mode' minor-mode complementing `dylan-mode'. This new
;;   mode includes many commands for interacting with the Open Dylan
;;   process.
;;
;;   A Dylan debugger written in Emacs Lisp. The debugger pops up
;;   an Emacs buffer similar to the Emacs/Elisp debugger.
;;
;;   A Open Dylan inspector to interactively look at run-time data.
;;
;;   Trapping compiler messages and creating annotations in the source
;;   file on the appropriate forms.
;;
;; DIME requires Emacs 24.
;;
;; In order to run DIME, a supporting Dylan server called Swank is
;; required. Swank is distributed with dime.el and will automatically
;; be started in a normal installation.


;;;; Dependencies and setup

;; check that we have at least Emacs 24... full DIME support is only
;; available in 24.3, but dime-compat lets dylan-mode work in 24.1.
(when (< emacs-major-version 24)
  (error "Dime requires an Emacs version of 24, or above"))

(eval-when-compile
  (require 'cl))
(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(require 'easymenu)
(require 'etags)
(require 'arc-mode)
(require 'apropos)
(require 'outline)
(require 'compile)
(require 'gud)

(require 'dime-compat)
(require 'dylan-mode)
(require 'dylanlid-mode)
(require 'dylan-optimization-coloring)

(defvar dime-buffer-project nil)
(defvar dime-buffer-connection nil)

(eval-and-compile
  (defvar dime-path
    (let ((path (or (locate-library "dime") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Dime package.
This is used to load the supporting Open Dylan library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))

(defvar dime-dylan-modes '(dylan-mode))
(defvar dime-setup-contribs nil)

;;;###autoload
(defun dime-setup (&optional contribs)
  "Setup Emacs so that dylan-mode buffers always use DIME.
CONTRIBS is a list of contrib packages to load."
  (when (member 'dylan-mode dime-dylan-modes)
    (add-hook 'dylan-mode-hook 'dime-dylan-mode-hook))
  (setq dime-setup-contribs contribs)
  (dime-setup-contribs))

(defun dime-setup-contribs ()
  "Load and initialize contribs."
  (dolist (c dime-setup-contribs)
    (require c)
    (let ((init (intern (format "%s-init" c))))
      (when (fboundp init)
        (funcall init)))))

(defun dime-dylan-mode-hook ()
  (dime-mode 1))

(eval-and-compile
  (defun dime-changelog-date (&optional interactivep)
    "Return the datestring of the latest entry in the ChangeLog file.
Return nil if the ChangeLog file cannot be found."
    (interactive "p")
    (let ((changelog (concat dime-path "ChangeLog"))
          (date nil))
      (when (file-exists-p changelog)
        (with-temp-buffer
          (insert-file-contents-literally changelog nil 0 100)
          (goto-char (point-min))
          (setq date (symbol-name (read (current-buffer))))))
      (when interactivep
        (message "Dime ChangeLog dates %s." date))
      date)))

(defvar dime-protocol-version "2011-02-13")


;;;; Customize groups
;;
;;;;; dime

(defgroup dime nil
  "Interaction with the Superior Dylan Environment."
  :prefix "dime-"
  :group 'applications)

;;;;; dime-ui

(defgroup dime-ui nil
  "Interaction with the Superior Dylan Environment."
  :prefix "dime-"
  :group 'dime)

(defcustom dime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'dime-ui)

(defcustom dime-kill-without-query-p nil
  "If non-nil, kill DIME processes without query when quitting Emacs.
This applies to the *inferior-dylan* buffer and the network connections."
  :type 'boolean
  :group 'dime-ui)

;;;;; dime-dylan

(defgroup dime-dylan nil
  "Dylan server configuration."
  :prefix "dime-"
  :group 'dime)

(defcustom dime-backend "swank-loader.dylan"
  "The name of the Dylan file that loads the Swank server.
This name is interpreted relative to the directory containing
dime.el, but could also be set to an absolute filename."
  :type 'string
  :group 'dime-dylan)

(defcustom dime-connected-hook nil
  "List of functions to call when DIME connects to Dylan."
  :type 'hook
  :group 'dime-dylan)

(defcustom dime-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Dylan can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'dime-dylan)

(defcustom dime-dylan-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'dime-dylan)

(defcustom dime-port 4005
  "Port to use as the default for `dime-connect'."
  :type 'integer
  :group 'dime-dylan)

(defvar dime-connect-host-history (list dime-dylan-host))
(defvar dime-connect-port-history (list (prin1-to-string dime-port)))

(defvar dime-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems.
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun dime-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `dime-net-valid-coding-systems'
of nil."
  (let ((probe (assq name dime-net-valid-coding-systems)))
    (when (and probe
               (ignore-errors (check-coding-system (car probe))))
      probe)))

(defcustom dime-net-coding-system
  (car (cl-find-if 'dime-find-coding-system
                   dime-net-valid-coding-systems :key 'car))
  "Coding system used for network connections.
See also `dime-net-valid-coding-systems'."
  :type (cons 'choice
              (mapcar (lambda (x)
                        (list 'const (car x)))
                      dime-net-valid-coding-systems))
  :group 'dime-dylan)

;;;;; dime-mode

(defgroup dime-mode nil
  "Settings for dime-mode Dylan source buffers."
  :prefix "dime-"
  :group 'dime)

(defcustom dime-find-definitions-function 'dime-find-definitions-rpc
  "Function to find definitions for a name.
The function is called with the definition name, a string, as its
argument."
  :type 'function
  :group 'dime-mode
  :options '(dime-find-definitions-rpc
             dime-etags-definitions
             (lambda (name)
               (append (dime-find-definitions-rpc name)
                       (dime-etags-definitions name)))
             (lambda (name)
               (or (dime-find-definitions-rpc name)
                   (and tags-table-list
                        (dime-etags-definitions name))))))

(defcustom dime-complete-symbol-function 'dime-simple-complete-symbol
  "*Function to perform symbol completion."
  :group 'dime-mode
  :type '(choice (const :tag "Simple" dime-simple-complete-symbol)
                 (const :tag "Compound" dime-complete-symbol*)
                 (const :tag "Fuzzy" dime-fuzzy-complete-symbol)))

;;;;; dime-mode-faces

(defgroup dime-mode-faces nil
  "Faces in dime-mode source code buffers."
  :prefix "dime-"
  :group 'dime-mode)

(defface dime-error-face
  `((((class color) (background light))
     (:underline "red"))
    (((class color) (background dark))
     (:underline "red"))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'dime-mode-faces)

(defface dime-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'dime-mode-faces)

(defface dime-style-warning-face
  `((((class color) (background light))
     (:underline "brown"))
    (((class color) (background dark))
     (:underline "gold"))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'dime-mode-faces)

(defface dime-note-face
  `((((class color) (background light))
     (:underline "brown4"))
    (((class color) (background dark))
     (:underline "light goldenrod"))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'dime-mode-faces)

(defun dime-face-inheritance-possible-p ()
  "Return true if the :inherit face attribute is supported."
  (assq :inherit custom-face-attributes))

(defface dime-highlight-face
  (if (dime-face-inheritance-possible-p)
      '((t (:inherit highlight :underline nil)))
    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for compiler notes while selected."
  :group 'dime-mode-faces)

;;;;; sldb

(defgroup dime-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'dime)

(defmacro define-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(loop for face in faces
                  collect `(define-sldb-face ,@face))))

(defmacro define-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
      ,(format "Face for %s." description)
      :group 'dime-debugger)))

(define-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class")
  (section        "the labels of major sections in the debugger buffer")
  (frame-label    "backtrace frame numbers")
  (restart-type   "restart names."
                  (if (dime-face-inheritance-possible-p)
                      '(:inherit font-lock-keyword-face)))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable"
   '(:foreground "lime green"))
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names")
  (local-value    "local variable values")
  (catch-tag      "catch tags"))


;;;; Minor modes

;;;;; dime-mode

(defvar dime-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `dime-mode-map' as it's parent.
This is a hack so that we can reinitilize the real dime-mode-map
more easily. See `dime-init-keymaps'.")

(define-minor-mode dime-mode
  "\\<dime-mode-map>\
DIME: The Superior Dylan Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[dime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[dime-compile-file]	- Compile (but not load) the current buffer's file.
\\[dime-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[dime-next-note]	- Goto the next form with a compiler note.
\\[dime-previous-note]	- Goto the previous form with a compiler note.
\\[dime-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[dime-edit-definition]	- Edit the definition of the function called at point.
\\[dime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Documentation commands:
\\[dime-describe-symbol]	- Describe symbol.
\\[dime-apropos]	- Apropos search.
\\[dime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[dime-eval-defun]	- Evaluate top-level from containing point.
\\[dime-eval-last-expression]	- Evaluate sexp before point.
\\[dime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{dime-mode-map}"
  nil
  nil
  dime-mode-indirect-map
  (dime-setup-command-hooks))



;;;;;; Modeline

(add-to-list 'minor-mode-alist
             `(dime-mode '(:eval (dime-modeline-string))))

(defun dime-modeline-string ()
  "Return the string to display in the modeline.
\"Dime\" only appears if we aren't connected.  If connected,
include project-name, connection-name, and possibly some state
information."
  (let ((conn (dime-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `dime-connection' which may query the user.
    (if (not conn)
        (and dime-mode " Dime")
        (let ((local (eq conn dime-buffer-connection))
              (pkg   (dime-current-project)))
          (concat " "
                  (if local "{" "[")
                  (if pkg pkg "?")
                  " "
                  ;; ignore errors for closed connections
                  (ignore-errors (dime-connection-name conn))
                  (dime-modeline-state-string conn)
                  (if local "}" "]"))))))

(defun dime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
         (format " %s" (process-status conn)))
        ((let ((pending (length (dime-rex-continuations conn)))
               (sldbs (length (sldb-buffers conn))))
           (cond ((and (zerop sldbs) (zerop pending)) nil)
                 ((zerop sldbs) (format " %s" pending))
                 (t (format " %s/%s" pending sldbs)))))))


;;;;; Key bindings

(defvar dime-parent-map nil
  "Parent keymap for shared between all Dime related modes.")

(defvar dime-parent-bindings
  '(("\M-."      dime-edit-definition)
    ("\M-,"      dime-pop-find-definition-stack)
    ("\M-_"      dime-edit-uses)    ; for German layout
    ("\M-?"      dime-edit-uses)    ; for USian layout
    ("\C-x4." 	 dime-edit-definition-other-window)
    ("\C-x5." 	 dime-edit-definition-other-frame)
    ("\C-x\C-e"  dime-eval-last-expression)
    ("\C-\M-x"   dime-eval-defun)
    ;; Include PREFIX keys...
    ("\C-c"	 dime-prefix-map)))

(defvar dime-prefix-map nil
  "Keymap for commands prefixed with `dime-prefix-key'.")

(defvar dime-prefix-bindings
  '(("\C-r"  dime-eval-region)
    (":"     dime-interactive-eval)
    ("\C-e"  dime-interactive-eval)
    ("E"     dime-edit-value)
    ("\C-l"  dime-load-file)
    ("\C-b"  dime-interrupt)
    ("\M-d"  dime-disassemble-symbol)
    ("\C-t"  dime-toggle-trace-fdefinition)
    ("I"     dime-inspect)
    ("\C-xt" dime-list-threads)
    ("\C-xn" dime-cycle-connections)
    ("\C-xc" dime-list-connections)
    ("<"     dime-list-callers)
    (">"     dime-list-callees)
    ;; Include DOC keys...
    ("\C-d"  dime-doc-map)
    ;; Include XREF WHO-FOO keys...
    ("\C-w"  dime-who-map)
    ))

(defvar dime-editing-map nil
  "These keys are useful for buffers where the user can insert and
edit s-exprs, e.g. for source buffers and the REPL.")

(defvar dime-editing-keys
  `(;; Arglist display & completion
    ("\M-\t"      dime-complete-symbol)
    (" "          dime-space)
    ;; Evaluating
    ;;("\C-x\M-e" dime-eval-last-expression-display-output :inferior t)
    ("\C-c\C-p"   dime-pprint-eval-last-expression)
    ;; Macroexpand
    ("\C-c\C-m"   dime-expand-1)
    ("\C-c\M-m"   dime-macroexpand-all)
    ;; Misc
    ("\C-c\C-u"   dime-undefine-function)
    (,(kbd "C-M-.")   dime-next-location)
    (,(kbd "C-M-,")   dime-previous-location)
    ;; Obsolete, redundant bindings
    ("\C-c\C-i" dime-complete-symbol)
    ;;("\M-*" pop-tag-mark) ; almost to clever
    ))

(defvar dime-mode-map nil
  "Keymap for dime-mode.")

(defvar dime-keys
  '( ;; Compiler notes
    ("\M-p"       dime-previous-note)
    ("\M-n"       dime-next-note)
    ("\C-c\M-c"   dime-remove-notes)
    ("\C-c\C-k"   dime-compile-and-load-file)
    ("\C-c\M-k"   dime-compile-file)
    ("\C-c\C-c"   dime-compile-defun)))

(defun dime-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar dime-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar dime-doc-bindings
  '((?a dime-apropos)
    (?z dime-apropos-all)
    (?p dime-apropos-project)
    (?d dime-describe-symbol)
    (?f dime-describe-function)))

(defvar dime-who-map nil
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar dime-who-bindings
  '((?c dime-who-calls)
    (?w dime-calls-who)
    (?r dime-who-references)
    (?b dime-who-binds)
    (?s dime-who-sets)
    (?m dime-who-macroexpands)
    (?a dime-who-specializes)))

(defun dime-init-keymaps ()
  "(Re)initialize the keymaps for `dime-mode'."
  (interactive)
  (dime-init-keymap 'dime-doc-map t t dime-doc-bindings)
  (dime-init-keymap 'dime-who-map t t dime-who-bindings)
  (dime-init-keymap 'dime-prefix-map t nil dime-prefix-bindings)
  (dime-init-keymap 'dime-parent-map nil nil dime-parent-bindings)
  (dime-init-keymap 'dime-editing-map nil nil dime-editing-keys)
  (set-keymap-parent dime-editing-map dime-parent-map)
  (dime-init-keymap 'dime-mode-map nil nil dime-keys)
  (set-keymap-parent dime-mode-map dime-editing-map)
  (set-keymap-parent dime-mode-indirect-map dime-mode-map))

(defun dime-init-keymap (keymap-name prefixp bothp bindings)
  (set keymap-name (make-sparse-keymap))
  (when prefixp (define-prefix-command keymap-name))
  (dime-bind-keys (eval keymap-name) bothp bindings))

(defun dime-bind-keys (keymap bothp bindings)
  "Add BINDINGS to KEYMAP.
If BOTHP is true also add bindings with control modifier."
  (loop for (key command) in bindings do
        (cond (bothp
               (define-key keymap `[,key] command)
               (unless (equal key ?h)     ; But don't bind C-h
                 (define-key keymap `[(control ,key)] command)))
              (t (define-key keymap key command)))))

(dime-init-keymaps)

(define-minor-mode dime-editing-mode
  "Minor mode which makes dime-editing-map available.
\\{dime-editing-map}"
  nil
  nil
  dime-editing-map)


;;;; Setup initial `dime-mode' hooks

(defvar-local dime-pre-command-actions nil
   "List of functions to execute before the next Emacs command.
This list of flushed between commands.")

(defun dime-pre-command-hook ()
  "Execute all functions in `dime-pre-command-actions', then NIL it."
  (dolist (undo-fn dime-pre-command-actions)
    (funcall undo-fn))
  (setq dime-pre-command-actions nil))

(defun dime-post-command-hook ()
  (when (null pre-command-hook) ; sometimes this is lost
    (add-hook 'pre-command-hook 'dime-pre-command-hook)))

(defun dime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `dime-pre-command-hook'."
  (add-hook 'pre-command-hook 'dime-pre-command-hook nil t)
  (add-hook 'post-command-hook 'dime-post-command-hook nil t))


;;;; Framework'ey bits
;;;
;;; This section contains some standard DIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(cl-defmacro when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (declare (indent 1))
  (let ((operator (cl-gensym "op-"))
	(operands (cl-gensym "rand-"))
	(tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "ELISP destructure-case failed: %S" ,tmp))))))))

(defmacro dime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  (declare (indent 1))
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(cl-defmacro with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2))
  (cl-flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (cl-gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot))
						 ,struct-var)))))
		      slots)
	   . ,body)))))

;;;;; Very-commonly-used functions

(defvar dime-message-function 'message)

;; Interface
(defun dime-buffer-name (type &optional hidden)
  (assert (keywordp type))
  (concat (if hidden " " "")
          (format "*dime-%s*" (substring (symbol-name type) 1))))

;; Interface
(defun dime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply dime-message-function format args))

(defun dime-display-warning (message &rest args)
  (display-warning '(dime warning) (apply #'format message args)))

(defvar dime-background-message-function 'dime-display-oneliner)

;; Interface
(defun dime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `dime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply dime-background-message-function format-string format-args))

(defun dime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (dime-oneliner msg)))))

(defun dime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (frame-width)))))

;; Interface
(defun dime-set-truncate-lines ()
  "Apply `dime-truncate-lines' to the current buffer."
  (when dime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun dime-read-project-name (prompt &optional initial-value)
  "Read a project name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (dime-bogus-completion-alist
                             (dime-eval
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

;; Interface
(defun dime-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (thing-at-point 'dime-symbol)))
         (dime-read-from-minibuffer prompt (thing-at-point 'dime-symbol)))
        (t (thing-at-point 'dime-symbol))))

;; Interface
(defmacro dime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun dime-add-face (face string)
  (declare (indent 1))
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

;; Interface
(defsubst dime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (dime-propertize-region props (apply #'insert args)))

(defmacro dime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (declare (indent 1))
  (let ((start (cl-gensym)) (l (cl-gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (dime-indent-rigidly ,start (point) ,l)))))

(defun dime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun dime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (dime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun dime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun dime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun dime-rcurry (fun &rest args)
  "Like `dime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Temporary popup buffers

(defvar dime-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

;; Interface
(cl-defmacro dime-with-popup-buffer ((name &key project connection select mode)
                                    &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PROJECT is the value `dime-buffer-project'.
CONNECTION is the value for `dime-buffer-connection',
 if nil, no explicit connection is associated with
 the buffer.  If t, the current connection is taken.
MODE is the name of a major mode which will be enabled.
"
  (declare (indent 1))
  `(let* ((vars% (list ,(if (eq project t) '(dime-current-project) project)
                       ,(if (eq connection t) '(dime-connection) connection)))
          (standard-output (dime-make-popup-buffer ,name vars% ,mode)))
     (with-current-buffer standard-output
       (prog1 (progn ,@body)
         (assert (eq (current-buffer) standard-output))
         (setq buffer-read-only t)
         (set-window-point (dime-display-popup-buffer ,(or select nil))
                           (point))))))

(defun dime-make-popup-buffer (name buffer-vars mode)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `dime-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (when mode
      (funcall mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (dime-init-popup-buffer buffer-vars)
    (current-buffer)))

(defun dime-init-popup-buffer (buffer-vars)
  (dime-popup-buffer-mode 1)
  (multiple-value-setq (dime-buffer-project dime-buffer-connection)
    buffer-vars))

(defun dime-display-popup-buffer (select)
  "Display the current buffer.
Save the selected-window in a buffer-local variable, so that we
can restore it later."
  (let ((selected-window (selected-window))
        (old-windows))
    (walk-windows (lambda (w) (push (cons w (window-buffer w)) old-windows))
                  nil t)
    (let ((new-window (display-buffer (current-buffer))))
      (unless dime-popup-restore-data
        (set (make-local-variable 'dime-popup-restore-data)
             (list new-window
                   selected-window
                   (cdr (cl-find new-window old-windows :key #'car)))))
      (when select
        (select-window new-window))
      new-window)))

(defun dime-close-popup-window ()
  (when dime-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
        dime-popup-restore-data
      (kill-local-variable 'dime-popup-restore-data)
      (bury-buffer)
      (when (eq popup-window (selected-window))
        (cond ((and (not old-buffer) (not (one-window-p)))
               (delete-window popup-window))
              ((and old-buffer (buffer-live-p old-buffer))
               (set-window-buffer popup-window old-buffer))))
      (when (window-live-p selected-window)
        (select-window selected-window)))))

(defmacro dime-save-local-variables (vars &rest body)
  (declare (indent 1))
  (let ((vals (make-symbol "vals")))
  `(let ((,vals (mapcar (lambda (var)
                          (if (local-variable-p var (current-buffer))
                              (cons var (eval var))))
                        ',vars)))
     (prog1 (progn . ,body)
       (mapc (lambda (var+val)
               (when (consp var+val)
                 (set (make-local-variable (car var+val)) (cdr var+val))))
             ,vals)))))

(define-minor-mode dime-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil
  nil
  '(("q" . dime-popup-buffer-quit-function)
    ;;("\C-c\C-z" . dime-switch-to-output-buffer)
    ("\M-." . dime-edit-definition)))

(add-to-list 'minor-mode-alist
             `(dime-popup-buffer-mode
               '(:eval (unless dime-mode
                         (dime-modeline-string)))))

(set-keymap-parent dime-popup-buffer-mode-map dime-parent-map)

(defvar-local dime-popup-buffer-quit-function 'dime-popup-buffer-quit
  "The function that is used to quit a temporary popup buffer.")

(defun dime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `dime-popup-buffer-quit-function'."
  (interactive)
  (funcall dime-popup-buffer-quit-function kill-buffer-p))

;; Interface
(defun dime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
Restore the window configuration unless it was changed since we
last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (dime-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Dylan should be translated using
;;; these functions. This way users who run Emacs and Dylan on separate
;;; machines have a chance to integrate file operations somehow.

(defvar dime-to-dylan-filename-function #'convert-standard-filename
  "Function to translate Emacs filenames to CL namestrings.")
(defvar dime-from-dylan-filename-function #'identity
  "Function to translate CL namestrings to Emacs filenames.")

(defun dime-to-dylan-filename (filename)
  "Translate the string FILENAME to a Dylan filename."
  (funcall dime-to-dylan-filename-function filename))

(defun dime-from-dylan-filename (filename)
  "Translate the Dylan filename FILENAME to an Emacs filename."
  (funcall dime-from-dylan-filename-function filename))


;;;; Starting DIME
;;;
;;; This section covers starting an inferior-dylan, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-dylan, but we use this variable for backward
;; compatibility.
(defvar inferior-dylan-program "dylan-compiler"
  "*Program name for invoking an inferior Dylan with for Inferior Dylan mode.")

(defvar dime-dylan-implementations nil
  "*A list of known Dylan implementations.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Dylan process.
For KEYWORD-ARGS see `dime-start'.

Here's an example:
 ((cmucl (\"/opt/cmucl/bin/dylan\" \"-quiet\") :init dime-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defvar dime-default-dylan nil
  "*The name of the default Dylan implementation.
See `dime-dylan-implementations'")

;; dummy definitions for the compiler
(defvar dime-net-processes)
(defvar dime-default-connection)

;;;###autoload
(defun dime (&optional command coding-system)
  "Start an inferior^_superior Dylan and connect to its Swank server."
  (interactive)

  (let ((inferior-dylan-program (or command inferior-dylan-program))
        (dime-net-coding-system (or coding-system dime-net-coding-system)))
    (dime-start* (cond ((and command (symbolp command))
                         (dime-dylan-options command))
                        (t (dime-read-interactive-args))))))

(defvar dime-inferior-dylan-program-history '()
  "History list of command strings.  Used by `dime'.")

(defun dime-read-interactive-args ()
  "Return the list of args which should be passed to `dime-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `dime-dylan-implementations' is nil, use
  `inferior-dylan-program' as fallback.

- If the table `dime-dylan-implementations' is non-nil use the
  implementation with name `dime-default-dylan' or if that's nil
  the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  dylans.

- If the prefix-arg is positive, read the command to start the
  process."
  (let ((table dime-dylan-implementations))
    (cond ((not current-prefix-arg) (dime-dylan-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read
                       "Dylan name: " (mapcar (lambda (x)
                                               (list (symbol-name (car x))))
                                             table)
                       nil t)))
             (dime-lookup-dylan-implementation table (intern key))))
          (t
           (destructuring-bind (program &rest program-args)
               (split-string (read-string
                              "Run dylan: " inferior-dylan-program
                              'dime-inferior-dylan-program-history))
             (let ((coding-system
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set dime-coding-system: "
                                            dime-net-coding-system)
                      dime-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

(defun dime-dylan-options (&optional name)
  (let ((table dime-dylan-implementations))
    (assert (or (not name) table))
    (cond (table (dime-lookup-dylan-implementation dime-dylan-implementations
                                                   (or name dime-default-dylan
                                                       (car (car table)))))
          (t (destructuring-bind (program &rest args)
                 (split-string inferior-dylan-program)
               (list :program program :program-args args))))))

(defun dime-lookup-dylan-implementation (table name)
  (let ((arguments (rest (assoc name table))))
    (unless arguments
      (error "Could not find dylan implementation with the name '%S'" name))
    (when (and (= (length arguments) 1)
               (functionp (first arguments)))
      (setf arguments (funcall (first arguments))))
    (destructuring-bind ((prog &rest args) &rest keys) arguments
      (list* :name name :program prog :program-args args keys))))

(cl-defun dime-start (&key (program inferior-dylan-program) program-args
                           directory
                           (coding-system dime-net-coding-system)
                           (init 'dime-init-command)
                           name
                           (buffer "*inferior-dylan*")
                           init-function
                           env)
  "Start a Dylan process and connect to it.
This function is intended for programmatic use if `dime' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `dime-init-command'.
CODING-SYSTEM a symbol for the coding system. The default is
  dime-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Dylan implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (dime-check-coding-system coding-system)
    (when (dime-bytecode-stale-p)
      (dime-urge-bytecode-recompile))
    (let ((proc (dime-maybe-start-dylan program program-args env
                                        directory buffer)))
      (when (processp proc)
        (dime-inferior-connect proc args)
        (pop-to-buffer (process-buffer proc))))))

(defun dime-start* (options)
  (apply #'dime-start options))

(defun dime-connect (host port &optional coding-system)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer
                      "Host: " (first dime-connect-host-history)
                      nil nil '(dime-connect-host-history . 1))
                     (string-to-number (read-from-minibuffer
                      "Port: " (first dime-connect-port-history)
                      nil nil '(dime-connect-port-history . 1)))))
  (when (and (called-interactively-p 'any) dime-net-processes
             (y-or-n-p "Close old connections first? "))
    (dime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ((coding-system (or coding-system dime-net-coding-system)))
    (dime-check-coding-system coding-system)
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (dime-net-connect host port coding-system))
           (dime-dispatching-connection process))
      (dime-setup-connection process))))

;; FIXME: seems redundant
(defun dime-start-and-init (options fun)
  (let* ((rest (plist-get options :init-function))
         (init (cond (rest `(lambda () (funcall ',rest) (funcall ',fun)))
                     (t fun))))
    (dime-start* (plist-put (cl-copy-list options) :init-function init))))

;;;;; Start inferior dylan
;;;
;;; Here is the protocol for starting DIME:
;;;
;;;   0. Emacs recompiles/reloads dime.elc if it exists and is stale.
;;;   1. Emacs starts an inferior Dylan process.
;;;   2. Emacs tells Dylan (via stdio) to load and start Swank.
;;;   3. Dylan recompiles the Swank if needed.
;;;   4. Dylan starts the Swank server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Swank needs recompilation.

(defvar dime-connect-retry-timer nil
  "Timer object while waiting for an inferior-dylan to start.")

;;; Recompiling bytecode:

(defun dime-bytecode-stale-p ()
  "Return true if dime.elc is older than dime.el."
  (when-let (libfile (locate-library "dime"))
    (let* ((basename (file-name-sans-extension libfile))
           (sourcefile (concat basename ".el"))
           (bytefile (concat basename ".elc")))
      (and (file-exists-p bytefile)
           (file-newer-than-file-p sourcefile bytefile)))))

(defun dime-recompile-bytecode ()
  "Recompile and reload dime."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "dime"))
                            ".el")))
    (byte-compile-file sourcefile t)))

(defun dime-urge-bytecode-recompile ()
  "Urge the user to recompile dime.elc.
Return true if we have been given permission to continue."
  (cond ((y-or-n-p "dime.elc is older than source.  Recompile first? ")
         (dime-recompile-bytecode))
        (t)))

(defun dime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (dime-connect-retry-timer
         (dime-cancel-connect-retry-timer)
         (message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Dylan and loading Swank:

(defun dime-maybe-start-dylan (program program-args env directory buffer)
  "Return a new or existing inferior dylan process."
  (cond ((not (comint-check-proc buffer))
         (dime-start-dylan program program-args env directory buffer))
        ((dime-reinitialize-inferior-dylan-p program program-args env buffer)
         (when-let (conn (cl-find (get-buffer-process buffer) dime-net-processes
                                  :key #'dime-inferior-process))
           (dime-net-close conn))
         (get-buffer-process buffer))
        (t (dime-start-dylan program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun dime-reinitialize-inferior-dylan-p (program program-args env buffer)
  (let ((args (dime-inferior-dylan-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
         (equal (plist-get args :program-args) program-args)
         (equal (plist-get args :env) env)
         (not (y-or-n-p "Create an additional *inferior-dylan*? ")))))

(defvar dime-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun dime-start-dylan (program program-args env directory buffer)
  "Does the same as `inferior-dylan' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (if (not (file-exists-p program))
        (message "please specify either 'inferior-dylan-program' or 'dime-dylan-implementations' in your .emacs!")
      (comint-mode)
      (let ((process-environment (append env process-environment))
            (process-connection-type nil))
        (comint-exec (current-buffer) "inferior-dylan" program nil program-args))
      (lisp-mode-variables t)
      (let ((proc (get-buffer-process (current-buffer))))
        (dime-set-query-on-exit-flag proc)
        (run-hooks 'dime-inferior-process-start-hook)
        proc))))

(defun dime-inferior-connect (process args)
  "Start a Swank server in the inferior Dylan and connect."
  (dime-delete-swank-port-file 'quiet)
  (dime-start-swank-server process args)
  (dime-read-port-and-connect process))

(defvar dime-inferior-dylan-args nil
  "A buffer local variable in the inferior proccess.
See `dime-start'.")

(defun dime-start-swank-server (process args)
  "Start a Swank server on the inferior dylan."
  (destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'dime-inferior-dylan-args)
      (setq dime-inferior-dylan-args args)
      (let ((str (funcall init (dime-swank-port-file) coding-system)))
        (goto-char (process-mark process))
        (insert-before-markers str)
        (process-send-string process str)))))

(defun dime-inferior-dylan-args (process)
  "Return the initial process arguments.
See `dime-start'."
  (with-current-buffer (process-buffer process)
    dime-inferior-dylan-args))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun dime-init-command (port-filename coding-system)
  "Return a string to initialize Dylan."
  (let ((loader (if (file-name-absolute-p dime-backend)
                    dime-backend
                  (concat dime-path dime-backend)))
        (encoding (dime-coding-system-cl-name coding-system)))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(dime-to-dylan-filename (expand-file-name loader))
                     :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,(dime-to-dylan-filename port-filename)
                        :coding-system ,encoding)))))

(defun dime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (concat (file-name-as-directory temporary-file-directory)
          (format "dime.%S" (emacs-pid))))

(defun dime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (dime-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
                         (dime-swank-port-file)))))))

(defun dime-read-port-and-connect (inferior-process)
  (dime-attempt-connection inferior-process nil 1))

(defun dime-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (dime-cancel-connect-retry-timer)
  (let ((file (dime-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x dime-abort-connection'.)" file))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (let ((port (dime-read-swank-port))
                 (args (dime-inferior-dylan-args process)))
             (dime-delete-swank-port-file 'message)
             (let ((c (dime-connect dime-dylan-host port
                                    (plist-get args :coding-system))))
               (dime-set-inferior-process c process))))
          ((and retries (zerop retries))
           (message "Gave up connecting to Swank after %d attempts." attempt))
          ((eq (process-status process) 'exit)
           (message "Failed to connect to Swank: inferior process exited."))
          (t
           (when (and (file-exists-p file)
                      (zerop (nth 7 (file-attributes file))))
             (message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (unless dime-connect-retry-timer
             (setq dime-connect-retry-timer
                   (run-with-timer
                    0.3 nil
                    #'dime-timer-call #'dime-attempt-connection
                    process (and retries (1- retries))
                    (1+ attempt))))))))

(defun dime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun dime-cancel-connect-retry-timer ()
  (when dime-connect-retry-timer
    (cancel-timer dime-connect-retry-timer)
    (setq dime-connect-retry-timer nil)))

(defun dime-read-swank-port ()
  "Read the Swank server port number from the `dime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (dime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (assert (integerp port))
        port))))

(defun dime-toggle-debug-on-swank-error ()
  (interactive)
  (if (dime-eval `(swank:toggle-debug-on-swank-error))
      (message "Debug on SWANK error enabled.")
      (message "Debug on SWANK error disabled.")))

;;; Words of encouragement

(defun dime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar dime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (dime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun dime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length dime-words-of-encouragement))
             dime-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each DIME protocol message beings with a 3-byte length header
;;; followed by an S-expression as text. The sexp must be readable
;;; both by Emacs and by Open Dylan, so if it contains any embedded
;;; code fragments they should be sent as strings.
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.dylan.

(defvar dime-net-processes nil
  "List of processes (sockets) connected to Dylans.")

(defvar dime-net-process-close-hooks '()
  "List of functions called when a dime network connection closes.
The functions are called with the process as their argument.")

(defun dime-secret ()
  "Find the magic secret from the user's home directory.
Return nil if the file doesn't exist or is empty; otherwise the
first line of the file."
  (condition-case err
      (with-temp-buffer
	(insert-file-contents "~/.dime-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defun dime-net-connect (host port coding-system)
  "Establish a connection with a Dylan."
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "DIME Dylan" nil host port))
         (buffer (dime-make-net-buffer " *dime-connection*")))
    (push proc dime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'dime-net-filter)
    (set-process-sentinel proc 'dime-net-sentinel)
    (dime-set-query-on-exit-flag proc)
    (dime-check-coding-system coding-system)
    (set-process-coding-system proc coding-system coding-system)
    (when-let (secret (dime-secret))
      (dime-net-send secret proc))
    proc))

(defun dime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun dime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `dime-kill-without-query-p'."
  (when dime-kill-without-query-p
    (set-process-query-on-exit-flag process nil)))

;;;;; Coding system madness

(defun dime-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (let ((props (dime-find-coding-system coding-system)))
    (unless props
      (error "Invalid dime-net-coding-system: %s. %s"
             coding-system (mapcar #'car dime-net-valid-coding-systems)))
    (when (second props)
      (assert enable-multibyte-characters))
    t))

(defun dime-coding-system-mulibyte-p (coding-system)
  (second (dime-find-coding-system coding-system)))

(defun dime-coding-system-cl-name (coding-system)
  (third (dime-find-coding-system coding-system)))

;;; Interface
(defun dime-net-send (sexp proc)
  "Send a SEXP to Dylan over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Dylan."
  (let* ((msg (concat (dime-prin1-to-string sexp) "\n"))
         (string (concat (dime-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (dime-log-event sexp)
    (cond ((dime-safe-encoding-p coding-system string)
           (process-send-string proc string))
          (t (error "Coding system %s not suitable for %S"
                    coding-system string)))))

(defun dime-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (or (let ((candidates (find-coding-systems-string string))
            (base (coding-system-base coding-system)))
        (or (equal candidates '(undecided))
            (memq base candidates)))
      (and (not (multibyte-string-p string))
           (not (dime-coding-system-mulibyte-p coding-system)))))

(defun dime-net-close (process &optional debug)
  (setq dime-net-processes (remove process dime-net-processes))
  (when (eq process dime-default-connection)
    (setq dime-default-connection nil))
  (cond (debug
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'dime-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun dime-net-sentinel (process message)
  (message "Dylan connection closed unexpectedly: %s" message)
  (dime-net-close process))

;;; Socket input is handled by `dime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun dime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (dime-process-available-input process))

(defun dime-process-available-input (process)
  "Process all complete messages that have arrived from Dylan."
  (with-current-buffer (process-buffer process)
    (while (dime-net-have-input-p)
      (let ((event (dime-net-read-or-lose process))
            (ok nil))
        (dime-log-event event)
        (unwind-protect
            (save-current-buffer
              (dime-dispatch-event event process)
              (setq ok t))
          (unless ok
            (dime-run-when-idle 'dime-process-available-input process)))))))

(defun dime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (dime-net-decode-length))))

(defun dime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 0 nil function args))

(defun dime-net-read-or-lose (process)
  (condition-case error
      (dime-net-read)
    (error
     (debug 'error error)
     (dime-net-close process t)
     (error "net-read error: %S" error))))

(defun dime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (dime-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun dime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun dime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun dime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Dylan networking concept.
;;;
;;; Emacs has a connection to each Dylan process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Dylans simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Dylan output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Dylan process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `dime-dispatching-connection' if dynamically bound, or
;;;   `dime-buffer-connection' if this is set buffer-local, or
;;;   `dime-default-connection' otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `dime-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `dime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Dylan that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `dime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Dylan handles commands in
;;; Dylan-mode source buffers, and dime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar dime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Dylan; it
overrides `dime-buffer-connection' and `dime-default-connection'.")

(defvar-local dime-buffer-connection nil
  "Network connection to use in the current buffer.
This overrides `dime-default-connection'.")

(defvar dime-default-connection nil
  "Network connection to use by default.
Used for all Dylan communication, except when overridden by
`dime-dispatching-connection' or `dime-buffer-connection'.")

(defun dime-current-connection ()
  "Return the connection to use for Dylan interaction.
Return nil if there's no connection."
  (or dime-dispatching-connection
      dime-buffer-connection
      dime-default-connection))

(defun dime-connection ()
  "Return the connection to use for Dylan interaction.
Signal an error if there's no connection."
  (let ((conn (dime-current-connection)))
    (cond ((and (not conn) dime-net-processes)
           (or (dime-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (dime-auto-connect)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

;; FIXME: should be called auto-start
(defcustom dime-auto-connect 'never
  "Controls auto connection when information from dylan process is needed.
This doesn't mean it will connect right after Dime is loaded."
  :group 'dime-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun dime-auto-connect ()
  (cond ((or (eq dime-auto-connect 'always)
             (and (eq dime-auto-connect 'ask)
                  (y-or-n-p "No connection.  Start Dime? ")))
         (save-window-excursion
           (dime)
           (while (not (dime-current-connection))
             (sleep-for 1))
           (dime-connection)))
        (t nil)))

(defcustom dime-auto-select-connection 'ask
  "Controls auto selection after the default connection was closed."
  :group 'dime-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun dime-auto-select-connection ()
  (let* ((c0 (car dime-net-processes))
         (c (cond ((eq dime-auto-select-connection 'always) c0)
                  ((and (eq dime-auto-select-connection 'ask)
                        (y-or-n-p
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (dime-connection-name c0))))
                   c0))))
    (when c
      (dime-select-connection c)
      (message "Switching to connection: %s" (dime-connection-name c))
      c)))

(defun dime-select-connection (process)
  "Make PROCESS the default connection."
  (setq dime-default-connection process))

(defun dime-cycle-connections ()
  "Change current dime connection, cycling through all connections."
  (interactive)
  (let* ((tail (or (cdr (member (dime-current-connection)
                                dime-net-processes))
                   dime-net-processes))
         (p (car tail)))
    (dime-select-connection p)
    (message "Dylan: %s %s" (dime-connection-name p) (process-contact p))))

(cl-defmacro dime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `dime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  (declare (indent 1))
  `(with-current-buffer
       (process-buffer (or ,process (dime-connection)
                           (error "No connection")))
     ,@body))

;;; Connection-local variables:

(defmacro dime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `dime-connection'."
  (declare (indent 2))
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (defvar-local ,real-var ,@initial-value-and-doc)
       ;; Accessor
       (defun ,varname (&optional process)
         (dime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(dime-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(dime-def-connection-var dime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(dime-def-connection-var dime-dylan-features '()
  "The symbol-names of Dylan's *FEATURES*.
This is automatically synchronized from Dylan.")

(dime-def-connection-var dime-dylan-modules '()
  "The strings of Dylan's *MODULES*.")

(dime-def-connection-var dime-pid nil
  "The process id of the Dylan process.")

(dime-def-connection-var dime-dylan-version nil
  "The version of the Dylan process.")

(dime-def-connection-var dime-dylan-implementation-type nil
  "The implementation type of the Dylan process.")

(dime-def-connection-var dime-dylan-implementation-version nil
  "The implementation version of the Dylan process.")

(dime-def-connection-var dime-dylan-implementation-name nil
  "The short name for the Dylan implementation.")

(dime-def-connection-var dime-dylan-implementation-program nil
  "The argv[0] of the process running the Dylan implementation.")

(dime-def-connection-var dime-connection-name nil
  "The short name for connection.")

(dime-def-connection-var dime-inferior-process nil
  "The inferior process for the connection if any.")

(dime-def-connection-var dime-communication-style nil
  "The communication style.")

(dime-def-connection-var dime-machine-instance nil
  "The name of the (remote) machine running the Dylan process.")

;;;;; Connection setup

(defvar dime-connection-counter 0
  "The number of DIME connections made. For generating serial numbers.")

;;; Interface
(defun dime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((dime-dispatching-connection process))
    (dime-init-connection-state process)
    (dime-select-connection process)
    process))

(defun dime-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal dime-net-processes (list proc))
    (setq dime-connection-counter 0))
  (dime-with-connection-buffer ()
    (setq dime-buffer-connection proc))
  (setf (dime-connection-number proc) (incf dime-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (let ((dime-current-thread t))
    (dime-eval-async '(swank:connection-info)
                      (dime-curry #'dime-set-connection-info proc))))

(defun dime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Dylan."
  (let ((dime-dispatching-connection connection)
        (dime-current-thread t))
    (destructuring-bind (&key pid style lisp-implementation machine
                              features project version modules
                              &allow-other-keys) info
      (dime-check-version version connection)
      (setf (dime-dylan-version) version)
      (setf (dime-pid) pid
            (dime-communication-style) style
            (dime-dylan-features) features
            (dime-dylan-modules) modules)
      (destructuring-bind (&key type name version program) lisp-implementation
        (setf (dime-dylan-implementation-type) type
              (dime-dylan-implementation-version) version
              (dime-dylan-implementation-name) name
              (dime-dylan-implementation-program) program
              (dime-connection-name) (dime-generate-connection-name name)))
      (destructuring-bind (&key instance type version) machine
        (setf (dime-machine-instance) instance)))
    (let ((args (when-let (p (dime-inferior-process))
                  (dime-inferior-dylan-args p))))
      (when-let (name (plist-get args ':name))
        (unless (string= (dime-dylan-implementation-name) name)
          (setf (dime-connection-name)
                (dime-generate-connection-name (symbol-name name)))))
      (dime-load-contribs)
      (run-hooks 'dime-connected-hook)
      (when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    (message "Connected. %s" (dime-random-words-of-encouragement))))

(defun dime-check-version (version conn)
  (or (equal version dime-protocol-version)
      (equal dime-protocol-version 'ignore)
      (y-or-n-p
       (format "Versions differ: %s (dime) vs. %s (swank). Continue? "
               dime-protocol-version version))
      (dime-net-close conn)
      (top-level)))

(defun dime-generate-connection-name (dylan-name)
  (loop for i from 1
        for name = dylan-name then (format "%s<%d>" dylan-name i)
        while (cl-find name dime-net-processes
                       :key #'dime-connection-name :test #'equal)
        finally (return name)))

(defun dime-connection-close-hook (process)
  (when (eq process dime-default-connection)
    (when dime-net-processes
      (dime-select-connection (car dime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (dime-connection-number)
               (dime-connection-name)))))

(add-hook 'dime-net-process-close-hooks 'dime-connection-close-hook)

;;;;; Commands on connections

(defun dime-disconnect ()
  "Close the current connection."
  (interactive)
  (dime-net-close (dime-connection)))

(defun dime-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'dime-net-close dime-net-processes))

(defun dime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (cadr (process-contact connection)))

(defun dime-process (&optional connection)
  "Return the Dylan process for CONNECTION (default `dime-connection').
Return nil if there's no process object for the connection."
  (let ((proc (dime-inferior-process connection)))
    (if (and proc
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable.
(defun dime-set-inferior-process (connection process)
  (setf (dime-inferior-process connection) process))

(defun dime-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (dime-connection))))
    (ecase (dime-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar dime-inhibit-pipelining t
  "*If true, don't send background requests if Dylan is already busy.")

(defun dime-background-activities-enabled-p ()
  (and (let ((con (dime-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (dime-busy-p))
           (not dime-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Dylan
;;; to apply a named Dylan function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`dime-eval-async') for
;;; most things. Reserve synchronous evaluations (`dime-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Dylan project". RPC requests
;;; always say what project the user is making them from and the Dylan
;;; side binds that project to *BUFFER-PROJECT* to use as it sees
;;; fit. The current project is defined as the buffer-local value of
;;; `dime-buffer-project' if set, and otherwise the project found in
;;;; a local lid file.
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Dylan process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `dime-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(defvar-local dime-current-thread t
   "The id of the current thread on the Dylan side.
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread.")

(defvar dime-buffer-project nil
  "The Dylan project associated with the current buffer.
This is set only in buffers bound to specific projects.")

;;; `dime-rex' is the RPC primitive which is used to implement both
;;; `dime-eval' and `dime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(cl-defmacro dime-rex ((&rest saved-vars)
                       (sexp &optional
                             (project dylan-buffer-module)
                             (thread 'dime-current-thread))
                       &rest continuations)
  "(dime-rex (VAR ...) (SEXP &optional PROJECT THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Dylan.

PROJECT is evaluated and Dylan binds *BUFFER-PROJECT* to this project.
The default value is dylan-buffer-module.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 1))
  (let ((result (cl-gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (dime-dispatch-event
        (list :emacs-rex ,sexp ,project ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

;;; Interface
(defun dime-current-project ()
  "Return the Open Dylan project in the current context.
If `dime-buffer-project' has a value then return that, otherwise
search for a lid file."
  (or dime-buffer-project
      (save-restriction
        (widen)
        (dime-find-buffer-project))))

(defvar dime-find-buffer-project-function 'dylan-find-buffer-library
  "*Function to use for `dime-find-buffer-project'.
The result should be the project-name (a string)
or nil if nothing suitable can be found.")

(defun dime-find-buffer-project ()
  "Figure out which Dylan project the current buffer is associated with."
  (funcall dime-find-buffer-project-function))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar dime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun dime-eval (sexp &optional project)
  "Evaluate EXPR on the superior Dylan and return the result."
  (let* ((tag (cl-gensym (format "dime-result-%d-"
                                 (1+ (dime-continuation-counter)))))
	 (dime-stack-eval-tags (cons tag dime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (dime-rex (tag sexp)
           (sexp (or project dylan-buffer-module))
         ((:ok value)
          (unless (member tag dime-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort condition)
          (throw tag (list #'error "Synchronous Dylan Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (dime-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Dylan connection closed unexpectedly"))
           (accept-process-output nil 0.01)))))))

(defun dime-eval-async (sexp &optional cont project)
  "Evaluate EXPR on the superior Dylan and call CONT with the result."
  (declare (indent 1))
  (dime-rex (cont (buffer (current-buffer)))
      (sexp (or project dylan-buffer-module))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; dime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :dime-eval-async)

;;; These functions can be handy too:

(defun dime-connected-p ()
  "Return true if the Swank connection is open."
  (not (null dime-net-processes)))

(defun dime-check-connected ()
  "Signal an error if we are not connected to Dylan."
  (unless (dime-connected-p)
    (error "Not connected. Use `%s' to start a Dylan."
           (substitute-command-keys "\\[dime]"))))

(defun dime-busy-p (&optional conn)
  "True if Dylan has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (dime-connection)))))
    (cl-remove-if (lambda (id)
                    (memq id debugged))
                  (dime-rex-continuations)
                  :key #'car)))

(defun dime-sync ()
  "Block until the most recent request has finished."
  (when (dime-rex-continuations)
    (let ((tag (caar (dime-rex-continuations))))
      (while (cl-find tag (dime-rex-continuations) :key #'car)
        (accept-process-output nil 0.1)))))

(defun dime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (dime-eval "PONG")))

;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Dylan.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Dylan don't.

(dime-def-connection-var dime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(dime-def-connection-var dime-continuation-counter 0
  "Continuation serial number counter.")

(defvar dime-event-hooks)

(defun dime-dispatch-event (event &optional process)
  (let ((dime-dispatching-connection (or process (dime-connection))))
    (or (run-hook-with-args-until-success 'dime-event-hooks event)
        (destructure-case event
          ((:emacs-rex form project thread continuation)
           (when (and (dime-use-sigint-for-interrupt) (dime-busy-p))
             (dime-display-oneliner "; pipelined request... %S" form))
           (let ((id (incf (dime-continuation-counter))))
             (dime-send `(:emacs-rex ,form ,project ,thread ,id))
             (push (cons id continuation) (dime-rex-continuations))))
          ((:return value id)
           (let ((rec (assq id (dime-rex-continuations))))
             (cond (rec (setf (dime-rex-continuations)
                              (remove rec (dime-rex-continuations)))
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (dime-send `(:emacs-interrupt ,thread)))
          ((:channel-send id msg)
           (dime-channel-send (or (dime-find-channel id)
                                   (error "Invalid channel id: %S %S" id msg))
                               msg))
          ((:emacs-channel-send id msg)
           (dime-send `(:emacs-channel-send ,id ,msg)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (dime-read-from-minibuffer-for-swank thread tag prompt initial-value))
          ((:y-or-n-p thread tag question)
           (dime-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (dime-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (dime-dylan-features) features))
          ((:eval-no-wait form)
           (dime-check-eval-in-emacs-enabled)
           (eval (read form)))
          ((:eval thread tag form-string)
           (dime-check-eval-in-emacs-enabled)
           (dime-eval-for-dylan thread tag form-string))
          ((:emacs-return thread tag value)
           (dime-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (dime-ed what))
          ((:inspect what wait-thread wait-tag)
           (let ((hook (when (and wait-thread wait-tag)
                         (lexical-let ((thread wait-thread)
                                       (tag wait-tag))
                           (lambda ()
                             (dime-send `(:emacs-return ,thread ,tag nil)))))))
             (dime-open-inspector what nil hook)))
          ((:background-message message)
           (dime-background-message "%s" message))
          ((:debug-condition thread message)
           (assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (dime-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (dime-with-popup-buffer ((dime-buffer-name :error))
             (princ (format "Invalid protocol message:\n%s\n\n%S"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (dime-rex-continuations)
                 (cl-remove id (dime-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))))))

(defun dime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (dime-net-send sexp (dime-connection)))

(defun dime-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (dime-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers))
  (dime-with-connection-buffer ()
    (erase-buffer)))

(defun dime-send-sigint ()
  (interactive)
  (signal-process (dime-pid) 'SIGINT))

;;;;; Channels

;;; A channel implements a set of operations.  Those operations can be
;;; invoked by sending messages to the channel.  Channels are used for
;;; protocols which can't be expressed naturally with RPCs, e.g. for
;;; streaming data over the wire.
;;;
;;; A channel can be "remote" or "local".  Remote channels are
;;; represented by integers.  Local channels are structures.  Messages
;;; sent to a closed (remote) channel are ignored.

(dime-def-connection-var dime-channels '()
  "Alist of the form (ID . CHANNEL).")

(dime-def-connection-var dime-channels-counter 0
  "Channel serial number counter.")

(defstruct (dime-channel (:conc-name dime-channel.)
                          (:constructor
                           dime-make-channel% (operations name id plist)))
  operations name id plist)

(defun dime-make-channel (operations &optional name)
  (let* ((id (incf (dime-channels-counter)))
         (ch (dime-make-channel% operations name id nil)))
    (push (cons id ch) (dime-channels))
    ch))

(defun dime-find-channel (id)
  (cdr (assq id (dime-channels))))

(defun dime-channel-send (channel message)
  (apply (or (gethash (car message) (dime-channel.operations channel))
             (error "Unsupported operation: %S %S" message channel))
         channel (cdr message)))

;;;;; Event logging to *dime-events*
;;;
;;; The *dime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar dime-log-events t
  "*Log protocol events to the *dime-events* buffer.")

(defvar dime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *dime-events*.")

(defvar dime-event-buffer-name (dime-buffer-name :events)
  "The name of the dime event buffer.")

(defun dime-log-event (event)
  "Record the fact that EVENT occurred."
  (when dime-log-events
    (with-current-buffer (dime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (dime-pprint-event event (current-buffer)))
      (when outline-minor-mode
        (hide-entry))
      (goto-char (point-max)))))

(defun dime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun dime-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer dime-event-buffer-name)
      (let ((buffer (get-buffer-create dime-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when dime-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;;; Cleanup after a quit

(defun dime-restart-inferior-dylan ()
  "Kill and restart the Dylan subprocess."
  (interactive)
  (assert (dime-inferior-process) () "No inferior dylan process")
  (dime-quit-dylan-internal (dime-connection) 'dime-restart-sentinel t))

(defun dime-restart-sentinel (process message)
  "Restart the inferior dylan process.
Also rearrange windows."
  (assert (process-status process) 'closed)
  (let* ((proc (dime-inferior-process process))
         (args (dime-inferior-dylan-args proc))
         (buffer (buffer-name (process-buffer proc)))
         (buffer-window (get-buffer-window buffer))
         (new-proc (dime-start-dylan (plist-get args :program)
                                     (plist-get args :program-args)
                                     (plist-get args :env)
                                     nil
                                     buffer)))
    (when (processp new-proc)
      (dime-net-close process)
      (dime-inferior-connect new-proc args)
      (switch-to-buffer buffer)
      (goto-char (point-max)))))

;; FIXME: move to dime-repl
(defun dime-kill-all-buffers ()
  "Kill all the dime related buffers.
This is only used by the repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) dime-event-buffer-name)
              (string-match "^\\*inferior-dylan*" (buffer-name buf))
              (string-match "^\\*dime-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf))
              (string-match "^\\*DIME.*\\*$" (buffer-name buf)))
      (kill-buffer buf))))


;;;; Compilation and the creation of compiler-note annotations

(defvar dime-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defvar dime-before-compile-functions nil
  "A list of function called before compiling a buffer or region.
The function receive two arguments: the beginning and the end of the
region that will be compiled.")

;; FIXME: remove some of the options
(defcustom dime-compilation-finished-hook 'dime-maybe-show-compilation-log
  "Hook called with a list of compiler notes after a compilation."
  :group 'dime-mode
  :type 'hook
  :options '(dime-maybe-show-compilation-log
             dime-create-compilation-log
             dime-show-compilation-log
             dime-maybe-list-compiler-notes
             dime-list-compiler-notes
             dime-maybe-show-xrefs-for-notes
             dime-goto-first-note))

;; FIXME: I doubt that anybody uses this directly and it seems to be
;; only an ugly way to pass arguments.
(defvar dime-compilation-policy nil
  "When non-nil compile with these optimization settings.")

(defun dime-compute-policy (arg)
  "Return the policy for the prefix argument ARG."
  (cl-flet ((between (min n max)
           (if (< n min)
               min
               (if (> n max) max n))))
    (let ((n (prefix-numeric-value arg)))
      (cond ((not arg)   dime-compilation-policy)
            ((plusp n)   `((cl:debug . ,(between 0 n 3))))
            ((eq arg '-) `((cl:speed . 3)))
            (t           `((cl:speed . ,(between 0 (abs n) 3))))))))

(defstruct (dime-compilation-result
             (:type list)
             (:conc-name dime-compilation-result.)
             (:constructor nil)
             (:copier nil))
  tag notes successp duration loadp faslfile)

(defvar dime-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun dime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (dime-compilation-result.notes dime-last-compilation-result))

(defun dime-compile-and-load-file (&optional policy)
  "Compile and load the buffer's file and highlight compiler notes.

With (positive) prefix argument the file is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`dime-next-note' and `dime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive "P")
  (dime-compile-file t (dime-compute-policy policy)))

;;; FIXME: This should become a DEFCUSTOM
(defvar dime-compile-file-options '()
  "Plist of additional options that C-c C-k should pass to Dylan.
Currently only :fasl-directory is supported.")

(defun dime-compile-file (&optional load policy)
  "Compile current buffer's file and highlight resulting compiler notes.

See `dime-compile-and-load-file' for further details."
  (interactive)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (check-parens)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (run-hook-with-args 'dime-before-compile-functions (point-min) (point-max))
  (let ((file (dime-to-dylan-filename (buffer-file-name)))
        (options (dime-simplify-plist `(,@dime-compile-file-options
                                         :policy ,policy))))
    (dime-eval-async
        `(swank:compile-file-for-emacs ,file ,(if load t nil)
                                       . ,(dime-hack-quotes options))
      #'dime-compilation-finished)
    (message "Compiling %s..." file)))

(defun dime-hack-quotes (arglist)
  ;; eval is the wrong primitive, we really want funcall
  (loop for arg in arglist collect `(quote ,arg)))

(defun dime-simplify-plist (plist)
  (loop for (key val) on plist by #'cddr
        append (cond ((null val) '())
                     (t (list key val)))))

(defun dime-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form.

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  (interactive "P")
  (let ((dime-compilation-policy (dime-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (dime-compile-region (region-beginning) (region-end))
        (apply #'dime-compile-region (dime-region-for-defun-at-point)))))

(defun dime-compile-region (start end)
  "Compile the region."
  (interactive "r")
  (dime-flash-region start end)
  (run-hook-with-args 'dime-before-compile-functions start end)
  (dime-compile-string (buffer-substring-no-properties start end) start))

(defun dime-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun dime-compile-string (string start-offset)
  (let* ((line (save-excursion
                 (goto-char start-offset)
                 (list (line-number-at-pos) (1+ (current-column)))))
         (position `((:position ,start-offset) (:line ,@line))))
    (dime-eval-async
      `(swank:compile-string-for-emacs
        ,string
        ,(buffer-name)
        ',position
        ,(if (buffer-file-name) (dime-to-dylan-filename (buffer-file-name)))
        ',dime-compilation-policy)
      #'dime-compilation-finished)))

(defun dime-compilation-finished (result)
  (with-struct (dime-compilation-result. notes duration successp
                                          loadp faslfile) result
    (setf dime-last-compilation-result result)
    (dime-show-note-counts notes duration (cond ((not loadp) successp)
                                                 (t (and faslfile successp))))
    (when dime-highlight-compiler-notes
      (dime-highlight-notes notes))
    (run-hook-with-args 'dime-compilation-finished-hook notes)
    (when (and loadp faslfile
               (or successp
                   (y-or-n-p "Compilation failed.  Load fasl file anyway? ")))
      (dime-eval-async `(swank:load-file ,faslfile)))))

(defun dime-show-note-counts (notes secs successp)
  (message (concat
            (cond (successp "Compilation finished")
                  (t (dime-add-face 'font-lock-warning-face
                       "Compilation failed")))
            (if (null notes) ". (No warnings)" ": ")
            (mapconcat
             (lambda (messages)
               (destructuring-bind (sev . notes) messages
                 (let ((len (length notes)))
                   (format "%d %s%s" len (dime-severity-label sev)
                           (if (= len 1) "" "s")))))
             (sort (dime-alistify notes #'dime-note.severity #'eq)
                   (lambda (x y) (dime-severity< (car y) (car x))))
             "  ")
            (if secs (format "  [%.2f secs]" secs)))))

(defun dime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (dime-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (dime-remove-old-overlays)
        (mapc #'dime-overlay-note (dime-merge-notes-for-display notes))))))

(defvar dime-note-overlays '()
  "List of overlays created by `dime-make-note-overlay'")

(defun dime-remove-old-overlays ()
  "Delete the existing note overlays."
  (mapc #'delete-overlay dime-note-overlays)
  (setq dime-note-overlays '()))

(defun dime-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (cl-remove-if-not (lambda (%buffer)
                      (with-current-buffer %buffer
                        (funcall predicate)))
                    (buffer-list)))

;;;;; Recompilation.

;; FIXME: This whole idea is questionable since it depends so
;; crucially on precise source-locs.

(defun dime-recompile-location (location)
  (save-excursion
    (dime-goto-source-location location)
    (dime-compile-defun)))

(defun dime-recompile-locations (locations cont)
  (dime-eval-async
   `(swank:compile-multiple-strings-for-emacs
     ',(loop for loc in locations collect
             (save-excursion
               (dime-goto-source-location loc)
               (destructuring-bind (start end)
                   (dime-region-for-defun-at-point)
                 (list (buffer-substring-no-properties start end)
                       (buffer-name)
                       (dime-current-project)
                       start
                       (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                         nil)))))
     ',dime-compilation-policy)
   cont))


;;;;; Merging together compiler notes in the same location.

(defun dime-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'dime-merge-notes
          (dime-group-similar 'dime-notes-in-same-location-p notes)))

(defun dime-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (cl-reduce #'dime-most-severe notes
                                  :key #'dime-note.severity))
         (new-message (mapconcat #'dime-note.message notes "\n")))
    (let ((new-note (cl-copy-list (car notes))))
      (setf (getf new-note :message) new-message)
      (setf (getf new-note :severity) new-severity)
      new-note)))

(defun dime-notes-in-same-location-p (a b)
  (equal (dime-note.location a) (dime-note.location b)))


;;;;; Compiler notes list

(defun dime-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun dime-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (cl-getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (cons (format "%s: %s"
                            (cl-getf note :severity)
                            (dime-one-line-ify (cl-getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
              (setf xrefs (acons fn (list node) xrefs))))))
    xrefs))

(defun dime-maybe-show-xrefs-for-notes (notes)
  "Show the compiler notes NOTES if they come from more than one file."
  (let ((xrefs (dime-xrefs-for-notes notes)))
    (when (dime-length> xrefs 1)          ; >1 file
      (dime-show-xrefs
       xrefs 'definition "Compiler notes" (dime-current-project)))))

(defun dime-note-has-location-p (note)
  (not (eq ':error (car (dime-note.location note)))))

(defun dime-redefinition-note-p (note)
  (eq (dime-note.severity note) :redefinition))

(defun dime-create-compilation-log (notes)
  "Create a buffer for `next-error' to use."
  (with-current-buffer (get-buffer-create (dime-buffer-name :compilation))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (dime-insert-compilation-log notes)
    (compilation-mode)))

(defun dime-maybe-show-compilation-log (notes)
  "Display the log on failed compilations or if NOTES is non-nil."
  (dime-create-compilation-log notes)
  (with-struct (dime-compilation-result. notes duration successp)
      dime-last-compilation-result
    (unless successp
      (with-current-buffer (dime-buffer-name :compilation)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Compilation " (if successp "succeeded." "failed."))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun dime-show-compilation-log (notes)
  "Create and display the compilation log buffer."
  (interactive (list (dime-compiler-notes)))
  (dime-with-popup-buffer ((dime-buffer-name :compilation)
                            :mode 'compilation-mode)
    (dime-insert-compilation-log notes)))

(defun dime-insert-compilation-log (notes)
  "Insert NOTES in format suitable for `compilation-mode'."
  (multiple-value-bind (grouped-notes canonicalized-locs-table)
      (dime-group-and-sort-notes notes)
    (with-temp-message "Preparing compilation log..."
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)) ; inefficient font-lock-hook
        (insert (format "cd %s\n%d compiler notes:\n\n"
                        default-directory (length notes)))
        (dolist (notes grouped-notes)
          (let ((loc (gethash (first notes) canonicalized-locs-table))
                (start (point)))
            (insert (dime-canonicalized-location-to-string loc) ":")
            (dime-insert-note-group notes)
            (insert "\n")
            (dime-make-note-overlay (first notes) start (1- (point))))))
      (set (make-local-variable 'compilation-skip-threshold) 0)
      (setq next-error-last-buffer (current-buffer)))))

(defun dime-insert-note-group (notes)
  "Insert a group of compiler messages."
  (insert "\n")
  (dolist (note notes)
    (insert "  " (dime-severity-label (dime-note.severity note)) ": ")
    (let ((start (point)))
      (insert (dime-note.message note))
      (let ((ctx (dime-note.source-context note)))
        (if ctx (insert "\n" ctx)))
      (dime-indent-block start 4))
    (insert "\n")))

(defun dime-indent-block (start column)
  "If the region back to START isn't a one-liner indent it."
  (when (< start (line-beginning-position))
    (save-excursion
      (goto-char start)
      (insert "\n"))
    (dime-indent-rigidly start (point) column)))

(defun dime-canonicalized-location (location)
  "Return a list (FILE LINE COLUMN) for dime-location LOCATION.
This is quite an expensive operation so use carefully."
  (save-excursion
    (dime-goto-location-buffer (dime-location.buffer location))
    (save-excursion
      (dime-goto-source-location location)
      (list (or (buffer-file-name) (buffer-name))
            (line-number-at-pos)
            (1+ (current-column))))))

(defun dime-canonicalized-location-to-string (loc)
  (if loc
      (destructuring-bind (filename line col) loc
        (format "%s:%d:%d"
                (cond ((not filename) "")
                      ((let ((rel (file-relative-name filename)))
                         (if (< (length rel) (length filename))
                             rel)))
                      (t filename))
                line col))
      (format "Unknown location")))

(defun dime-goto-note-in-compilation-log (note)
  "Find `note' in the compilation log and display it."
  (with-current-buffer (get-buffer (dime-buffer-name :compilation))
    (let ((origin (point))
          (foundp nil))
      (goto-char (point-min))
      (let ((overlay))
        (while (and (setq overlay (dime-find-next-note))
                    (not foundp))
          (let ((other-note (overlay-get overlay 'dime-note)))
            (when (dime-notes-in-same-location-p note other-note)
              (dime-show-buffer-position (overlay-start overlay) 'top)
              (setq foundp t)))))
      (unless foundp
        (goto-char origin)))))

(defun dime-group-and-sort-notes (notes)
  "First sort, then group NOTES according to their canonicalized locs."
  (let ((locs (make-hash-table :test #'eq)))
    (mapc (lambda (note)
            (let ((loc (dime-note.location note)))
              (when (dime-location-p loc)
                (puthash note (dime-canonicalized-location loc) locs))))
          notes)
    (values (dime-group-similar
             (lambda (n1 n2)
               (equal (gethash n1 locs nil) (gethash n2 locs t)))
             (let* ((bottom most-negative-fixnum)
                    (+default+ (list "" bottom bottom)))
               (sort notes
                     (lambda (n1 n2)
                       (destructuring-bind (filename1 line1 col1)
                           (gethash n1 locs +default+)
                         (destructuring-bind (filename2 line2 col2)
                             (gethash n2 locs +default+)
                           (cond ((string-lessp filename1 filename2) t)
                                 ((string-lessp filename2 filename1) nil)
                                 ((< line1 line2) t)
                                 ((> line1 line2) nil)
                                 (t (< col1 col2)))))))))
            locs)))

(defun dime-note.severity (note)
  (plist-get note :severity))

(defun dime-note.message (note)
  (plist-get note :message))

(defun dime-note.source-context (note)
  (plist-get note :source-context))

(defun dime-note.location (note)
  (plist-get note :location))

(defun dime-severity-label (severity)
  (cl-subseq (symbol-name severity) 1))


;;;;; Adding a single compiler note

(defun dime-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (dime-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (overlay (dime-note-at-point)))
        (if overlay
            (dime-merge-note-into-overlay overlay severity message)
            (dime-create-note-overlay note start end severity message))))))

(defun dime-make-note-overlay (note start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'dime-note note)
    (push overlay dime-note-overlays)
    overlay))

(defun dime-create-note-overlay (note start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (dime-make-note-overlay note start end)))
    (cl-flet ((putp (name value) (overlay-put overlay name value)))
      (putp 'face (dime-severity-face severity))
      (putp 'severity severity)
      (putp 'mouse-face 'highlight)
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `dime-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
(defun dime-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (cl-flet ((putp (name value) (overlay-put overlay name value))
	 (getp (name)       (overlay-get overlay name)))
    (putp 'severity (dime-most-severe severity (getp 'severity)))
    (putp 'face (dime-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun dime-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (dime-note.location note)))
    (when location
      (destructure-case location
        ((:error _) _ nil)                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               ((eq (dime-note.severity note) :read-error)
                (dime-choose-overlay-for-read-error location))
               ((equal pos '(:eof))
                (values (1- (point-max)) (point-max)))
               (t
                (dime-choose-overlay-for-sexp location))))))))

(defun dime-choose-overlay-for-read-error (location)
  (let ((pos (dime-location-offset location)))
    (save-excursion
      (goto-char pos)
      (cond ((thing-at-point 'dime-symbol)
             ;; project not found, &c.
             (values (dime-symbol-start-pos) (dime-symbol-end-pos)))
            (t
             (values pos (1+ pos)))))))

(defun dime-choose-overlay-for-sexp (location)
  (dime-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (dime-forward-sexp))
    (if (dime-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (ignore-errors (forward-sexp 1))
                     (point))))))

(defun dime-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defvar dime-severity-face-plist
  '(:error         dime-error-face
    :read-error    dime-error-face
    :warning       dime-warning-face
    :redefinition  dime-style-warning-face
    :style-warning dime-style-warning-face
    :note          dime-note-face))

(defun dime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (or (plist-get dime-severity-face-plist severity)
      (error "No face for: %S" severity)))

(defvar dime-severity-order
  '(:note :style-warning :redefinition :warning :error :read-error))

(defun dime-severity< (sev1 sev2)
  "Return true if SEV1 is less severe than SEV2."
  (< (cl-position sev1 dime-severity-order)
     (cl-position sev2 dime-severity-order)))

(defun dime-most-severe (sev1 sev2)
  "Return the most servere of two conditions."
  (if (dime-severity< sev1 sev2) sev2 sev1))

;; XXX: unused function
(defun dime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (dime-forward-source-path source-path))

(defun dime-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors
    (dime-forward-sexp)
    (beginning-of-defun))
  (when-let (source-path (cdr source-path))
    (down-list 1)
    (dime-forward-source-path source-path)))

(defun dime-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (loop for (count . more) on source-path
                do (progn
                     (dime-forward-sexp count)
                     (when more (down-list 1))))
          ;; Align at beginning
          (dime-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))


;; FIXME: really fix this mess
;; FIXME: the check shouln't be done here anyway but by M-. itself.

(defun dime-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
      (file-name-as-directory "/")))

(defun dime-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (dime-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.dylan\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.dylan\")

        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.dylan\"
"
  (let ((target-dirs (split-string (file-name-directory target-filename) "/" t))
        (buffer-dirs (split-string (file-name-directory buffer-filename) "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (loop with target-suffix-dirs = nil
          with buffer-dirs* = (reverse buffer-dirs)
          with target-dirs* = (reverse target-dirs)
          for target-dir in target-dirs*
          do (cl-flet ((concat-dirs (dirs)
                      (apply #'concat (mapcar #'file-name-as-directory dirs))))
               (let ((pos (cl-position target-dir buffer-dirs* :test #'equal)))
                 (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                     (push target-dir target-suffix-dirs)
                     (let* ((target-suffix (concat-dirs target-suffix-dirs)) ; PUSH reversed for us!
                            (buffer-root   (concat-dirs (reverse (nthcdr pos buffer-dirs*)))))
                       (return (concat (dime-filesystem-toplevel-directory)
                                       buffer-root
                                       target-suffix
                                       (file-name-nondirectory target-filename))))))))))

(defun dime-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (cl-flet ((insert-dir (dirname)
           (insert (file-name-as-directory dirname)))
         (insert-dir/propzd (dirname)
           (dime-insert-propertized '(face highlight) dirname)
           (insert "/")))  ; Not exactly portable (to VMS...)
    (let ((base-dirs (split-string base-dirname "/" t))
          (contrast-dirs (split-string contrast-dirname "/" t)))
      (with-temp-buffer
        (loop initially (insert (dime-filesystem-toplevel-directory))
              for base-dir in base-dirs do
              (let ((pos (cl-position base-dir contrast-dirs :test #'equal)))
                (if (not pos)
                    (insert-dir/propzd base-dir)
                    (progn (insert-dir base-dir)
                           (setq contrast-dirs (nthcdr (1+ pos) contrast-dirs))))))
        (buffer-substring (point-min) (point-max))))))

(defvar dime-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`dime-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Dylan image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (the one corresponding to
the Dylan image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun dime-maybe-warn-for-different-source-root (target-filename buffer-filename)
  (let ((guessed-target (dime-file-name-merge-source-root target-filename
                                                           buffer-filename)))
    (when (and guessed-target
               (not (equal guessed-target target-filename))
               (file-exists-p guessed-target))
      (dime-message "Attention: This is `%s'."
                     (concat (dime-highlight-differences-in-dirname
                              (file-name-directory target-filename)
                              (file-name-directory guessed-target))
                             (file-name-nondirectory target-filename))))))

(defun dime-check-location-filename-sanity (filename)
  (when dime-warn-when-possibly-tricked-by-M-.
    (cl-flet ((file-truename-safe (filename) (and filename (file-truename filename))))
      (let ((target-filename (file-truename-safe filename))
            (buffer-filename (file-truename-safe (buffer-file-name))))
        (when (and target-filename
                   buffer-filename)
          (dime-maybe-warn-for-different-source-root
           target-filename buffer-filename))))))

(defun dime-check-location-buffer-name-sanity (buffer-name)
  (dime-check-location-filename-sanity
   (buffer-file-name (get-buffer buffer-name))))



(defun dime-goto-location-buffer (buffer)
  (destructure-case buffer
    ((:file filename)
     (let ((filename (dime-from-dylan-filename filename)))
       (dime-check-location-filename-sanity filename)
       (set-buffer (or (get-file-buffer filename)
                       (let ((find-file-suppress-same-file-warnings t))
                         (find-file-noselect filename))))))
    ((:buffer buffer-name)
     (dime-check-location-buffer-name-sanity buffer-name)
     (set-buffer buffer-name))
    ((:source-form string)
     (set-buffer (get-buffer-create (dime-buffer-name :source)))
     (erase-buffer)
     (dylan-mode)
     (insert string)
     (goto-char (point-min)))
    ((:zip file entry)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))))))

(defun dime-goto-location-position (position)
  (destructure-case position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (dime-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-char (point-min))
     (beginning-of-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (goto-char (point-min))
       (when (or
              (re-search-forward
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_"
                       (regexp-quote name)) nil t)
              (re-search-forward
               (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t))
         (goto-char (match-beginning 0)))))
    ((:method name specializers &rest qualifiers)
     (dime-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (dime-forward-positioned-source-path source-path))
           (t
            (dime-forward-source-path source-path))))
    ((:eof)
     (goto-char (point-max)))))

(defun dime-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (case (coding-system-eol-type buffer-file-coding-system)
    ((1)
     (save-excursion
       (do ((pos (+ (point) n))
            (count 0 (1+ count)))
           ((>= (point) pos) (1- count))
         (forward-line)
         (decf pos))))
    (t 0)))

(defun dime-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat (lambda (el)
                                    (if (eql (aref el 0) ?\()
                                        (let ((spec (read el)))
                                          (if (eq (car spec) 'EQL)
                                              (concat ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                                      (format "%s" (second spec)) ")")
                                            (error "don't understand specializer: %s,%s" el (car spec))))
                                      (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                                  (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;	(dime-goto-location-position `(:function-name ,name))
        )))

(defun dime-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[)\n \t]"))
          (case-fold-search t))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun dime-search-edit-path (edit-path)
  "Move to EDIT-PATH starting at the current toplevel form."
  (when edit-path
    (unless (and (= (current-column) 0)
                 (looking-at "("))
      (beginning-of-defun))
    (dime-forward-source-path edit-path)))

(defun dime-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>)

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>)
             | (:method <name string> <specializer strings> . <qualifiers strings>)"
  (destructure-case location
    ((:location buffer position hints)
     (dime-goto-location-buffer buffer)
     (let ((pos (dime-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (if noerror
         (dime-message "%s" message)
       (error "%s" message)))))

(defun dime-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (dime-goto-location-position (dime-location.position location))
    (let ((hints (dime-location.hints location)))
      (when-let (snippet (cl-getf hints :snippet))
        (dime-isearch snippet))
      (when-let (snippet (cl-getf hints :edit-path))
        (dime-search-edit-path snippet))
      (when-let (fname (cl-getf hints :call-site))
        (dime-search-call-site fname))
      (when (cl-getf hints :align)
        (dime-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun dime-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (dime-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (dime-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun dime-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (loop for i from 1 to (length string)
          while (funcall search-fn (substring string 0 i) nil t)
          for match-data = (match-data)
          do (case search-fn
               (search-forward  (goto-char (match-beginning 0)))
               (search-backward (goto-char (1+ (match-end 0)))))
          finally (return (if (null match-data)
                              nil
                            ;; Finish based on the last successful match
                            (store-match-data match-data)
                            (goto-char (match-beginning 0))
                            (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

(defun dime-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (dime-find-next-note)))
    (if note
        (dime-show-note note)
      (goto-char here)
      (message "No next note."))))

(defun dime-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (dime-find-previous-note)))
    (if note
        (dime-show-note note)
      (goto-char here)
      (message "No previous note."))))

(defun dime-goto-first-note (&rest ignore)
  "Go to the first note in the buffer."
  (let ((point (point)))
    (goto-char (point-min))
    (cond ((dime-find-next-note)
           (dime-show-note (dime-note-at-point)))
          (t (goto-char point)))))

(defun dime-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (dime-remove-old-overlays))

(defun dime-show-note (overlay)
  "Present the details of a compiler note to the user."
  (dime-temporarily-highlight-note overlay)
  (if (get-buffer-window (dime-buffer-name :compilation) t)
      (dime-goto-note-in-compilation-log (overlay-get overlay 'dime-note))
      (let ((message (get-char-property (point) 'help-echo)))
        (dime-message "%s" (if (zerop (length message)) "\"\"" message)))))

;; FIXME: could probably use flash region
(defun dime-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone with a timer."
  (run-with-timer 0.2 nil
                  #'overlay-put overlay 'face (overlay-get overlay 'face))
  (overlay-put overlay 'face 'dime-highlight-face))


;;;;; Overlay lookup operations

(defun dime-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (cl-find (point) (dime-note-overlays-at-point)
           :key 'overlay-start))

(defun dime-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'dime-note))

(defun dime-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (cl-remove-if-not 'dime-note-overlay-p (overlays-at (point))))

(defun dime-find-next-note ()
  "Go to the next position with the `dime-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (dime-search-property 'dime-note nil #'dime-note-at-point))

(defun dime-find-previous-note ()
  "Go to the next position with the `dime-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (dime-search-property 'dime-note t #'dime-note-at-point))


;;;; Arglist Display

(defun dime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (dime-background-activities-enabled-p)
    (dime-echo-arglist)))

(put 'dime-space 'delete-selection t) ; for delete-section-mode & CUA

(defvar dime-echo-arglist-function 'dime-show-arglist)

(defun dime-echo-arglist ()
  "Display the arglist of the current form in the echo area."
  (funcall dime-echo-arglist-function))

(defun dime-show-arglist ()
  (let ((op (dime-operator-before-point)))
    (when op
      (dime-eval-async `(swank:operator-arglist ,op ,dylan-buffer-module)
			(lambda (arglist)
			  (when arglist
			    (dime-message "%s" arglist)))))))

(defun dime-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (thing-at-point 'dime-symbol))))


;;;; Completion

(defvar dime-completions-buffer-name "*Completions*")

(defvar-local dime-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed.")

(defvar-local dime-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `dime-complete-saved-window-configuration'.")

(defun dime-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or dime-complete-saved-window-configuration
              (get-buffer-window dime-completions-buffer-name))
    (setq dime-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun dime-complete-delay-restoration ()
  (add-hook 'pre-command-hook
            'dime-complete-maybe-restore-window-configuration
            nil t))

(defun dime-complete-forget-window-configuration ()
  (setq dime-complete-saved-window-configuration nil)
  (setq dime-completions-window nil))

(defun dime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'dime-complete-maybe-restore-window-configuration)
  (when (and dime-complete-saved-window-configuration
             (dime-completion-window-active-p))
    ;; XEmacs does not allow us to restore a window configuration from
    ;; pre-command-hook, so we do it asynchronously.
    (dime-run-when-idle
     (lambda ()
       (save-excursion
         (set-window-configuration
          dime-complete-saved-window-configuration))
       (setq dime-complete-saved-window-configuration nil)
       (when (buffer-live-p dime-completions-buffer-name)
         (kill-buffer dime-completions-buffer-name))))))

(defun dime-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'dime-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((cl-find last-command-event "()\"'`,# \r\n:")
             (dime-complete-restore-window-configuration))
            ((not (dime-completion-window-active-p))
             (dime-complete-forget-window-configuration))
            (t
             (dime-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in dime-complete-restore-window-configuration: %S" err))))

(defun dime-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p dime-completions-window)
       (equal (buffer-name (window-buffer dime-completions-window))
              dime-completions-buffer-name)))

(defun dime-display-completion-list (completions base)
  (let ((savedp (dime-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer dime-completions-buffer-name
      (display-completion-list completions)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-position offset)
          (set-syntax-table lisp-mode-syntax-table))))
    (when savedp
      (setq dime-completions-window
            (get-buffer-window dime-completions-buffer-name)))))

(defun dime-display-or-scroll-completions (completions base)
  (cond ((and (eq last-command this-command)
              (dime-completion-window-active-p))
         (dime-scroll-completions))
        (t
         (dime-display-completion-list completions base)))
  (dime-complete-delay-restoration))

(defun dime-scroll-completions ()
  (let ((window dime-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun dime-complete-symbol ()
  "Complete the symbol at point.

Completion is performed by `dime-complete-symbol-function'."
  (interactive)
  (funcall dime-complete-symbol-function))

(defun dime-simple-complete-symbol ()
  "Complete the symbol at point.
Perform completion more similar to Emacs' complete-symbol."
  (or (dime-maybe-complete-as-filename)
      (let* ((end (point))
             (beg (dime-symbol-start-pos))
             (prefix (buffer-substring-no-properties beg end))
             (result (dime-simple-completions prefix)))
        (destructuring-bind (completions partial) result
          (if (null completions)
              (progn (dime-minibuffer-respecting-message
                      "Can't find completion for \"%s\"" prefix)
                     (ding)
                     (dime-complete-restore-window-configuration))
            (insert-and-inherit (substring partial (length prefix)))
            (cond ((dime-length= completions 1)
                   (dime-minibuffer-respecting-message "Sole completion")
                   (dime-complete-restore-window-configuration))
                  ;; Incomplete
                  (t
                   (dime-minibuffer-respecting-message
                    "Complete but not unique")
                   (dime-display-or-scroll-completions completions
                                                        partial))))))))

(defun dime-maybe-complete-as-filename ()
  "If point is at a string starting with \", complete it as filename.
Return nil if point is not at filename."
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" (max (point-min)
                                                                  (- (point) 1000)) t))
    (let ((comint-completion-addsuffix '("/" . "\"")))
      (comint-replace-by-expanded-filename)
      t)))

(defun dime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defun dime-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'dime-repl-prompt)
      (dylan-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (dime-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (dime-echo-arglist))))))

(defvar dime-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'dime-complete-symbol)
    (define-key map "\M-\t" 'dime-complete-symbol)
    map)
  "Minibuffer keymap used for reading CL expressions.")

(defvar dime-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defun dime-minibuffer-setup-hook ()
  (cons (lexical-let ((project (dime-current-project))
                      (connection (dime-connection)))
          (lambda ()
            (setq dime-buffer-project project)
            (setq dime-buffer-connection connection)
            (set-syntax-table lisp-mode-syntax-table)))
        minibuffer-setup-hook))

(defun dime-read-from-minibuffer (prompt &optional initial-value history)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook (dime-minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value dime-minibuffer-map
			  nil 'dime-minibuffer-history)))

(defun dime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun dime-simple-completions (prefix)
  (let ((dime-current-thread t))
    (dime-eval
     `(swank:simple-completions ,prefix ',(dime-current-project)))))


;;;; Edit definition

(defun dime-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (ring-insert find-tag-marker-ring (point-marker)))

(defun dime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(defstruct (dime-xref (:conc-name dime-xref.) (:type list))
  dspec location)

(defstruct (dime-location (:conc-name dime-location.) (:type list)
                           (:constructor nil)
                           (:copier nil))
  tag buffer position hints)

(defun dime-location-p (o) (and (consp o) (eq (car o) :location)))

(defun dime-xref-has-location-p (xref)
  (dime-location-p (dime-xref.location xref)))

(defun make-dime-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

(defun make-dime-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

;;; The hooks are tried in order until one succeeds, otherwise the
;;; default implementation involving `dime-find-definitions-function'
;;; is used. The hooks are called with the same arguments as
;;; `dime-edit-definition'.
(defvar dime-edit-definition-hooks)

(defun dime-edit-definition (name &optional where)
  "Lookup the definition of the name at point.
If there's no name at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (dime-read-symbol-name "Edit Definition of: " t)))
  (or (run-hook-with-args-until-success 'dime-edit-definition-hooks
                                        name where)
      (dime-edit-definition-cont (dime-find-definitions name)
                                  name where)))

(defun dime-edit-definition-cont (xrefs name where)
  (destructuring-bind (1loc file-alist) (dime-analyze-xrefs xrefs)
    (cond ((null xrefs)
           (error "No known definition for: %s (in %s)"
                  name dylan-buffer-module))
          (1loc
           (dime-push-definition-stack)
           (dime-pop-to-location (dime-xref.location (car xrefs)) where))
          ((dime-length= xrefs 1)      ; ((:error "..."))
           (error "%s" (cadr (dime-xref.location (car xrefs)))))
          (t
           (dime-push-definition-stack)
           (dime-show-xrefs file-alist 'definition name
                             dylan-buffer-module)))))

(defvar dime-edit-uses-xrefs
  '(:calls :macroexpands :binds :references :sets :specializes))

;;; FIXME. TODO: Would be nice to group the symbols (in each
;;;              type-group) by their home-project.
(defun dime-edit-uses (symbol)
  "Lookup all the uses of SYMBOL."
  (interactive (list (dime-read-symbol-name "Edit Uses of: ")))
  (dime-xrefs dime-edit-uses-xrefs
               symbol
               (lambda (xrefs type symbol project)
                 (cond
                  ((null xrefs)
                   (message "No xref information found for %s." symbol))
                  ((and (dime-length= xrefs 1)          ; one group
                        (dime-length= (cdar  xrefs) 1)) ; one ref in group
                   (destructuring-bind (_ (_ loc)) (first xrefs)
                     (dime-push-definition-stack)
                     (dime-pop-to-location loc)))
                  (t
                   (dime-push-definition-stack)
                   (dime-show-xref-buffer xrefs type symbol project))))))

(defun dime-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (dime-xref.location (car xrefs))))
               (and (dime-location-p loc)
                    (cl-every (lambda (x) (equal (dime-xref.location x) loc))
                              (cdr xrefs)))))
        (dime-alistify xrefs #'dime-xref-group #'equal)))

(defun dime-xref-group (xref)
  (cond ((dime-xref-has-location-p xref)
         (destructure-case (dime-location.buffer (dime-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer
                  (format "%S" buffer) ; "#<buffer foo.dylan>"
                (format "%s (previously existing buffer)" bufname))))
           ((:source-form _) "(S-Exp)")
           ((:zip zip entry) entry)))
        (t
         "(No location)")))

(defun dime-pop-to-location (location &optional where)
  (dime-goto-source-location location)
  (ecase where
    ((nil)     (switch-to-buffer (current-buffer)))
    (window    (pop-to-buffer (current-buffer) t))
    (frame     (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))))

(defun dime-postprocess-xref (original-xref)
  "Process (for normalization purposes) an Xref comming directly
from SWANK before the rest of Dime sees it. In particular,
convert ETAGS based xrefs to actual file+position based
locations."
  (if (not (dime-xref-has-location-p original-xref))
      (list original-xref)
      (let ((loc (dime-xref.location original-xref)))
        (destructure-case (dime-location.buffer loc)
          ((:etags-file tags-file)
           (destructure-case (dime-location.position loc)
             ((:tag &rest tags)
              (visit-tags-table tags-file)
              (mapcar (lambda (xref)
                          (let ((old-dspec (dime-xref.dspec original-xref))
                                (new-dspec (dime-xref.dspec xref)))
                            (setf (dime-xref.dspec xref)
                                  (format "%s: %s" old-dspec new-dspec))
                            xref))
                      (cl-mapcan #'dime-etags-definitions tags)))))
          (t
           (list original-xref))))))

(defun dime-postprocess-xrefs (xrefs)
  (cl-mapcan #'dime-postprocess-xref xrefs))

(defun dime-find-definitions (name)
  "Find definitions for NAME."
  (dime-postprocess-xrefs (funcall dime-find-definitions-function name)))

(defun dime-find-definitions-rpc (name)
  (dime-eval `(swank:find-definitions-for-emacs ,name)))

(defun dime-edit-definition-other-window (name)
  "Like `dime-edit-definition' but switch to the other window."
  (interactive (list (dime-read-symbol-name "Symbol: " t)))
  (dime-edit-definition name 'window))

(defun dime-edit-definition-other-frame (name)
  "Like `dime-edit-definition' but switch to the other window."
  (interactive (list (dime-read-symbol-name "Symbol: " t)))
  (dime-edit-definition name 'frame))

(defun dime-edit-definition-with-etags (name)
  (interactive (list (dime-read-symbol-name "Symbol: " t)))
  (let ((xrefs (dime-etags-definitions name)))
    (cond (xrefs
           (message "Using tag file...")
           (dime-edit-definition-cont xrefs name nil))
          (t
           (error "No known definition for: %s" name)))))

(defun dime-etags-to-locations (name)
  "Search for definitions matching `name' in the currently active
tags table. Return a possibly empty list of dime-locations."
  (let ((locs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t) ; hint==t if we are in a filename line
                (push `(:location (:file ,(expand-file-name (file-of-tag)))
                                  (:line ,line)
                                  (:snippet ,hint))
                       locs))))))
      (nreverse locs))))

(defun dime-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (mapcar (lambda (loc)
              (make-dime-xref :dspec (second (dime-location.hints loc))
                               :location loc))
          (dime-etags-to-locations name)))

;;;;; first-change-hook

(defun dime-first-change-hook ()
  "Notify Dylan that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (dime-background-activities-enabled-p))
        (let ((filename (dime-to-dylan-filename (buffer-file-name))))
           (dime-eval-async `(swank:buffer-first-change ,filename)))))))

(defun dime-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'dime-first-change-hook))

; disable for now, not sure whether this is useful for us in
; the future, thus only commenting out -- hannes (Jan 2012)
;(add-hook 'dime-mode-hook 'dime-setup-first-change-hook)


;;;; Eval for Dylan

(defun dime-eval-for-dylan (thread tag form-string)
  (let ((ok nil)
        (value nil)
        (error nil)
        (c (dime-connection)))
    (unwind-protect
        (condition-case err
            (progn
              (dime-check-eval-in-emacs-enabled)
              (setq value (eval (read form-string)))
              (dime-check-eval-in-emacs-result value)
              (setq ok t))
          ((debug error)
           (setq error err)))
      (let ((result (cond (ok `(:ok ,value))
                          (error `(:error ,(symbol-name (car error))
                                          . ,(mapcar #'prin1-to-string
                                                     (cdr error))))
                          (t `(:abort)))))
        (dime-dispatch-event `(:emacs-return ,thread ,tag ,result) c)))))

(defun dime-check-eval-in-emacs-result (x)
  "Raise an error if X can't be marshaled."
  (or (stringp x)
      (memq x '(nil t))
      (integerp x)
      (keywordp x)
      (and (consp x)
           (let ((l x))
             (while (consp l)
               (dime-check-eval-in-emacs-result (car x))
               (setq l (cdr l)))
             (dime-check-eval-in-emacs-result l)))
      (error "Non-serializable return value: %S" x)))

(defun dime-check-eval-in-emacs-enabled ()
  "Raise an error if `dime-enable-evaluate-in-emacs' isn't true."
  (unless dime-enable-evaluate-in-emacs
    (error (concat "dime-eval-in-emacs disabled for security."
                   "Set dime-enable-evaluate-in-emacs true to enable it."))))


;;;; `ED'

(defvar dime-ed-frame nil
  "The frame used by `dime-ed'.")

(defcustom dime-ed-use-dedicated-frame t
  "*When non-nil, `dime-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'dime-mode)

(defun dime-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil.

This is for use in the implementation of COMMON-DYLAN:ED."
  (when dime-ed-use-dedicated-frame
    (unless (and dime-ed-frame (frame-live-p dime-ed-frame))
      (setq dime-ed-frame (make-frame)))
    (select-frame dime-ed-frame))
  (when what
    (destructure-case what
      ((:filename file &key line column position)
       (find-file (dime-from-dylan-filename file))
       (when line (dime-goto-line line))
       (when column (move-to-column column))
       (when position (goto-char position)))
      ((:function-name name)
       (dime-edit-definition name)))))

(defun dime-goto-line (line-number)
  "Move to line LINE-NUMBER (1-based).
This is similar to `goto-line' but without pushing the mark and
the display stuff that we neither need nor want."
  (assert (= (buffer-size) (- (point-max) (point-min))) ()
          "dime-goto-line in narrowed buffer")
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun dime-y-or-n-p (thread tag question)
  (dime-dispatch-event `(:emacs-return ,thread ,tag ,(y-or-n-p question))))

(defun dime-read-from-minibuffer-for-swank (thread tag prompt initial-value)
  (let ((answer (condition-case nil
                    (dime-read-from-minibuffer prompt initial-value)
                  (quit nil))))
    (dime-dispatch-event `(:emacs-return ,thread ,tag ,answer))))

;;;; Interactive evaluation.

(defun dime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (dime-read-from-minibuffer "Dime Eval: ")))
  (case current-prefix-arg
    ((nil)
     (dime-eval-with-transcript `(swank:interactive-eval ,string)))
    ((-)
     (dime-eval-save string))
    (t
     (dime-eval-print string))))

(defvar dime-transcript-start-hook nil
  "Hook run before start an evalution.")
(defvar dime-transcript-stop-hook nil
  "Hook run after finishing a evalution.")

(defun dime-display-eval-result (value)
  (dime-message "%s" value))

(defun dime-eval-with-transcript (form)
  "Eval FROM in Dylan.  Display output, if any."
  (run-hooks 'dime-transcript-start-hook)
  (dime-rex () (form)
    ((:ok value)
     (run-hooks 'dime-transcript-stop-hook)
     (dime-display-eval-result value))
    ((:abort condition)
     (run-hooks 'dime-transcript-stop-hook)
     (message "Evaluation aborted on %s." condition))))

(defun dime-eval-print (string)
  "Eval STRING in Dylan; insert any output and the result at point."
  (dime-eval-async `(swank:eval-and-grab-output ,string)
                    (lambda (result)
                      (destructuring-bind (output value) result
                        (push-mark)
                        (insert output value)))))

(defun dime-eval-save (string)
  "Evaluate STRING in Dylan and save the result in the kill ring."
  (dime-eval-async `(swank:eval-and-grab-output ,string)
    (lambda (result)
      (destructuring-bind (output value) result
        (let ((string (concat output value)))
          (kill-new string)
          (message "Evaluation finished; pushed result to kill ring."))))))

(defun dime-eval-describe (form)
  "Evaluate FORM in Dylan and display the result in a new buffer."
  (dime-eval-async form (dime-rcurry #'dime-show-description
                                       dylan-buffer-module)))

(defvar dime-description-autofocus nil
  "If non-nil select description windows on display.")

(defun dime-show-description (string project)
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (dime-buffer-name :description)))
    (dime-with-popup-buffer (bufname :project project
                                      :connection t
                                      :select dime-description-autofocus)
      (princ string)
      (goto-char (point-min)))))

(defun dime-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun dime-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (dime-interactive-eval (dime-last-expression)))

(defun dime-eval-defun ()
  "Evaluate the current toplevel form.
Use `dime-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (dime-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (dime-re-evaluate-defvar form))
          (t
           (dime-interactive-eval form)))))

(defun dime-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (dime-eval-with-transcript
   `(swank:interactive-eval-region
     ,(buffer-substring-no-properties start end))))

(defun dime-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (dime-eval-region (point-min) (point-max)))

(defun dime-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.

First make the variable unbound, then evaluate the entire form."
  (interactive (list (dime-last-expression)))
  (dime-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun dime-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (dime-eval-describe `(swank:pprint-eval ,(dime-last-expression))))

(defun dime-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (dime-last-expression)))
  (insert "\n")
  (dime-eval-print string))

;;;; Edit Dylan value
;;;
(defun dime-edit-value (form-string)
  "\\<dime-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Dylan when committed with \\[dime-edit-value-commit]."
  (interactive
   (list (dime-read-from-minibuffer "Edit value (evaluated): "
				     (dime-sexp-at-point))))
  (dime-eval-async `(swank:value-for-editing ,form-string)
                    (lexical-let ((form-string form-string)
                                  (project (dime-current-project)))
                      (lambda (result)
                        (dime-edit-value-callback form-string result
                                                   project)))))

(defvar-local dime-edit-form-string nil
  "The form being edited by `dime-edit-value'.")

(define-minor-mode dime-edit-value-mode
  "Mode for editing a Dylan value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . dime-edit-value-commit)))

(defun dime-edit-value-callback (form-string current-value project)
  (let* ((name (generate-new-buffer-name (format "*Edit %s*" form-string)))
         (buffer (dime-with-popup-buffer (name :project project
                                                :connection t
                                                :select t
                                                :mode 'dylan-mode)
                   (dime-popup-buffer-mode -1) ; don't want binding of 'q'
                   (dime-mode 1)
                   (dime-edit-value-mode 1)
                   (setq dime-edit-form-string form-string)
                   (insert current-value)
                   (current-buffer))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (message "Type C-c C-c when done"))))

(defun dime-edit-value-commit ()
  "Commit the edited value to the Dylan image.
\\(See `dime-edit-value'.)"
  (interactive)
  (if (null dime-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (dime-eval-async `(swank:commit-edited-value ,dime-edit-form-string
                                                      ,value)
                          (lambda (_)
                            (with-current-buffer buffer
                              (dime-popup-buffer-quit t))))))))

;;;; Tracing

(defun dime-untrace-all ()
  "Untrace all functions."
  (interactive)
  (dime-eval `(swank:untrace-all)))

(defun dime-toggle-trace-fdefinition (spec)
  "Toggle trace."
  (interactive (list (dime-read-from-minibuffer
                      "(Un)trace: " (thing-at-point 'dime-symbol))))
  (message "%s" (dime-eval `(swank:swank-toggle-trace ,spec))))



(defun dime-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (dime-read-symbol-name "Disassemble: ")))
  (dime-eval-describe `(swank:disassemble-form ,(concat "'" symbol-name))))

(defun dime-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (dime-read-symbol-name "fmakunbound: " t)))
  (dime-eval-async `(swank:undefine-function ,symbol-name)
                    (lambda (result) (message "%s" result))))

(defun dime-load-file (filename)
  "Load the Dylan file FILENAME."
  (interactive (list
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (let ((dylan-filename (dime-to-dylan-filename (expand-file-name filename))))
    (dime-eval-with-transcript `(swank:load-file ,dylan-filename))))

(defvar dime-change-directory-hooks nil
  "Hook run by `dime-change-directory'.
The functions are called with the new (absolute) directory.")

(defun dime-change-directory (directory)
  "Make DIRECTORY become Dylan's current directory.
Return whatever swank:set-default-directory returns."
  (let ((dir (expand-file-name directory)))
    (prog1 (dime-eval `(swank:set-default-directory
                         ,(dime-to-dylan-filename dir)))
      (dime-with-connection-buffer nil (cd-absolute dir))
      (run-hook-with-args 'dime-change-directory-hooks dir))))

(defun dime-cd (directory)
  "Make DIRECTORY become Dylan's current directory.
Return whatever swank:set-default-directory returns."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s" (dime-change-directory directory)))

(defun dime-pwd ()
  "Show Dylan's default directory."
  (interactive)
  (message "Directory %s" (dime-eval `(swank:default-directory))))


;;;; Profiling

(defun dime-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (dime-read-from-minibuffer
                      "(Un)Profile: "
                      (thing-at-point 'dime-symbol))))
  (dime-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
                    (lambda (r) (message "%s" r))))

(defun dime-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (dime-eval-async '(swank:unprofile-all)
                    (lambda (r) (message "%s" r))))

(defun dime-profile-report ()
  "Print profile report."
  (interactive)
  (dime-eval-with-transcript '(swank:profile-report)))

(defun dime-profile-reset ()
  "Reset profile counters."
  (interactive)
  (dime-eval-async (dime-eval `(swank:profile-reset))
                    (lambda (r) (message "%s" r))))

(defun dime-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (dime-eval-async `(swank:profiled-functions)
                    (lambda (r) (message "%s" r))))

(defun dime-profile-project (project callers methods)
  "Profile all functions in PROJECT.
If CALLER is non-nil names have counts of the most common calling
functions recorded.
If METHODS is non-nil, profile all methods of all generic function
having names in the given project."
  (interactive (list (dime-read-project-name "Project: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (dime-eval-async `(swank:profile-project ,project ,callers ,methods)
                    (lambda (r) (message "%s" r))))

(defun dime-profile-by-substring (substring &optional project)
  "Profile all functions which names contain SUBSTRING.
If PROJECT is NIL, then search in all projects."
  (interactive (list
                (dime-read-from-minibuffer
                 "Profile by matching substring: "
                 (thing-at-point 'dime-symbol))
                (dime-read-project-name "Project (RET for all projects): ")))
  (let ((project (unless (equal project "") project)))
    (dime-eval-async `(swank:profile-by-substring ,substring ,project)
                      (lambda (r) (message "%s" r)) )))

;;;; Documentation

(defun dime-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (dime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (dime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun dime-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (dime-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (dime-eval-describe
   `(swank:documentation-symbol ,symbol-name)))

(defun dime-describe-function (symbol-name)
  (interactive (list (dime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (dime-eval-describe `(swank:describe-function ,symbol-name)))

(defun dime-apropos-summary (string case-sensitive-p project only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if project (format " in project %S" project) "")
          (if only-external-p " (external symbols only)" "")))

(defun dime-apropos (string &optional only-external-p project
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "DIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (dime-read-project-name "Project: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "DIME Apropos: ") t nil nil)))
  (let ((buffer-project (or project dylan-buffer-module)))
    (dime-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p
                                    ,case-sensitive-p ',project)
     (dime-rcurry #'dime-show-apropos string buffer-project
                   (dime-apropos-summary string case-sensitive-p
                                          project only-external-p)))))

(defun dime-apropos-all ()
  "Shortcut for (dime-apropos <string> nil nil)"
  (interactive)
  (dime-apropos (read-string "DIME Apropos: ") nil nil))

(defun dime-apropos-project (project &optional internal)
  "Show apropos listing for symbols in PROJECT.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (dime-read-project-name "Project: ")))
                       (if (string= pkg "") (dime-current-project) pkg))
                     current-prefix-arg))
  (dime-apropos "" (not internal) project))

(defun dime-show-apropos (plists string project summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (dime-with-popup-buffer ((dime-buffer-name :apropos)
                             :project project :connection t
                             :mode 'apropos-mode)
      (setq header-line-format summary)
      (dime-set-truncate-lines)
      (dime-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(defun dime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (assert designator)
      (dime-insert-propertized `(face apropos-symbol) designator))
    (terpri)
    (loop for (prop namespace)
          in '((:variable "Variable")
               (:function "Function")
               (:generic-function "Generic Function")
               (:macro "Macro")
               (:special-operator "Special Operator")
               (:setf "Setf")
               (:type "Type")
               (:class "Class")
               (:alien-type "Alien type")
               (:alien-struct "Alien struct")
               (:alien-union "Alien type")
               (:alien-enum "Alien enum"))
          ;; Properties not listed here will not show up in the buffer
          do
          (let ((value (plist-get plist prop))
                (start (point)))
            (when value
              (insert "  " namespace ": ")
              (princ (etypecase value
                       (string value)
                       ((member :not-documented) "(not documented)")))
              (add-text-properties
               start (point)
               (list 'type prop 'action 'dime-call-describer
                     'button t 'apropos-label namespace
                     'item (plist-get plist :designator)))
              (terpri))))))

(defun dime-call-describer (arg)
  (let* ((pos (if (markerp arg) arg (point)))
         (type (get-text-property pos 'type))
         (item (get-text-property pos 'item)))
    (dime-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))

(defun dime-info ()
  "Open Dime manual"
  (interactive)
  (let ((file (expand-file-name "doc/dime.info" dime-path)))
    (if (file-exists-p file)
        (info file)
        (message "No dime.info, run `make dime.info' in %s"
                 (expand-file-name "doc/" dime-path)))))


;;;; XREF: cross-referencing

(defvar dime-xref-mode-map)

(define-derived-mode dime-xref-mode dylan-mode "Xref"
  "dime-xref-mode: Major mode for cross-referencing.
\\<dime-xref-mode-map>\
The most important commands:
\\[dime-xref-quit]	- Dismiss buffer.
\\[dime-show-xref]	- Display referenced source and keep xref window.
\\[dime-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{dime-xref-mode-map}
\\{dime-popup-buffer-mode-map}
"
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (dime-mode -1))

(dime-define-keys dime-xref-mode-map
  ((kbd "RET") 'dime-goto-xref)
  ((kbd "SPC") 'dime-goto-xref)
  ([mouse-1] 'dime-goto-xref)
  ("v" 'dime-show-xref)
  ("n" (lambda () (forward-line 1)))
  ("p" (lambda () (forward-line -1)))
  ("\C-c\C-c" 'dime-recompile-xref)
  ("\C-c\C-k" 'dime-recompile-all-xrefs)
  ("\M-," 'dime-xref-retract)
  ([remap next-line] 'dime-xref-next-line)
  ([remap previous-line] 'dime-xref-prev-line))

(defun dime-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (forward-line 1)))


;;;;; XREF results buffer and window management

(cl-defmacro dime-with-xref-buffer ((xref-type symbol &optional project)
                                   &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  (declare (indent 1))
  `(let ((xref-buffer-name% (dime-buffer-name :xref)))
     (dime-with-popup-buffer (xref-buffer-name%
                               :project ,project
                               :connection t
                               :select t
                               :mode 'dime-xref-mode)
       (dime-set-truncate-lines)
       ,@body)))

(defun dime-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (loop for (group . refs) in xref-alist do
        (dime-insert-propertized '(face bold) group "\n")
        (loop for (label location) in refs do
              (dime-insert-propertized
               (list 'dime-location location
                     'face 'font-lock-keyword-face
                     'mouse-face 'highlight)
               "  " (dime-one-line-ify label))
              (dime-insert-propertized '() "\n")))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defun dime-xref-next-line ()
  (interactive)
  (dime-xref-show-location (dime-search-property 'dime-location)))

(defun dime-xref-prev-line ()
  (interactive)
  (dime-xref-show-location (dime-search-property 'dime-location t)))

(defun dime-xref-show-location (loc)
  (ecase (car loc)
    (:location (dime-show-source-location loc t))
    (:error (message "%s" (cadr loc)))
    ((nil))))

(defvar dime-next-location-function nil
  "Function to call for going to the next location.")

(defvar dime-previous-location-function nil
  "Function to call for going to the previous location.")

(defvar dime-xref-last-buffer nil
  "The most recent XREF results buffer.
This is used by `dime-goto-next-xref'")

(defun dime-show-xref-buffer (xrefs type symbol project)
  (dime-with-xref-buffer (type symbol project)
    (dime-insert-xrefs xrefs)
    (setq dime-next-location-function 'dime-goto-next-xref)
    (setq dime-previous-location-function 'dime-goto-previous-xref)
    (setq dime-xref-last-buffer (current-buffer))
    (goto-char (point-min))))

(defun dime-show-xrefs (xrefs type symbol project)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
      (dime-show-xref-buffer xrefs type symbol project)))


;;;;; XREF commands

(defun dime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (dime-read-symbol-name "Who calls: " t)))
  (dime-xref :calls symbol))

(defun dime-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (dime-read-symbol-name "Who calls: " t)))
  (dime-xref :calls-who symbol))

(defun dime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (dime-read-symbol-name "Who references: " t)))
  (dime-xref :references symbol))

(defun dime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (dime-read-symbol-name "Who binds: " t)))
  (dime-xref :binds symbol))

(defun dime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (dime-read-symbol-name "Who sets: " t)))
  (dime-xref :sets symbol))

(defun dime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (dime-read-symbol-name "Who macroexpands: " t)))
  (dime-xref :macroexpands symbol))

(defun dime-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (dime-read-symbol-name "Who specializes: " t)))
  (dime-xref :specializes symbol))

(defun dime-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (dime-read-symbol-name "List callers: ")))
  (dime-xref :callers symbol-name))

(defun dime-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (dime-read-symbol-name "List callees: ")))
  (dime-xref :callees symbol-name))

(defun dime-xref (type symbol &optional continuation)
  "Make an XREF request to Dylan."
  (dime-eval-async
   `(swank:xref ',type ',symbol)
   (dime-rcurry (lambda (result type symbol project cont)
                   (dime-check-xref-implemented type result)
                   (let* ((xrefs (dime-postprocess-xrefs result))
                          (file-alist (cadr (dime-analyze-xrefs result))))
                     (funcall (or cont 'dime-show-xrefs)
                              file-alist type symbol project)))
                 type
                 symbol
                 dylan-buffer-module
                 continuation)))

(defun dime-check-xref-implemented (type xrefs)
  (when (eq xrefs :not-implemented)
    (error "%s is not implemented yet on %s."
           (dime-xref-type type)
           (dime-dylan-implementation-name))))

(defun dime-xref-type (type)
  (format "who-%s" (dime-cl-symbol-name type)))

(defun dime-xrefs (types symbol &optional continuation)
  "Make multiple XREF requests at once."
  (dime-eval-async
   `(swank:xrefs ',types ',symbol)
   (dime-rcurry (lambda (result types symbol project cont)
                   (funcall (or cont 'dime-show-xrefs)
                            (dime-map-alist #'dime-xref-type
                                             #'identity
                                             result)
                            types symbol project))
                 types
                 symbol
                 dylan-buffer-module
                 continuation)))


;;;;; XREF navigation

(defun dime-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'dime-location)
        (error "No reference at point."))))

(defun dime-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (dime-sexp-at-point))))

(defun dime-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
      (goto-char (point-min))
      (while (ignore-errors (dime-next-line/not-add-newlines) t)
        (when-let (loc (get-text-property (point) 'dime-location))
          (let* ((dspec (dime-xref-dspec-at-point))
                 (xref  (make-dime-xref :dspec dspec :location loc)))
            (push xref xrefs)))))
    (nreverse xrefs)))

(defun dime-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (dime-show-xref)
  (dime-popup-buffer-quit))

(defun dime-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (dime-xref-location-at-point)))
    (dime-show-source-location location)))

(defun dime-goto-next-xref (&optional backward)
  "Goto the next cross-reference location."
  (if (not (buffer-live-p dime-xref-last-buffer))
      (error "No XREF buffer alive.")
    (multiple-value-bind (location pos)
        (with-current-buffer dime-xref-last-buffer
          (values (dime-search-property 'dime-location backward)
                  (point)))
      (cond ((dime-location-p location)
             (dime-pop-to-location location)
             ;; We do this here because changing the location can take
             ;; a while when Emacs needs to read a file from disk.
             (with-current-buffer dime-xref-last-buffer
               (dime-show-buffer-position pos)
               (dime-highlight-line 0.35)))
            ((null location)
             (message (if backward "No previous xref" "No next xref.")))
            (t ; error location
             (dime-goto-next-xref backward))))))

(defun dime-goto-previous-xref ()
  "Goto the previous cross-reference location."
  (dime-goto-next-xref t))

(defun dime-search-property (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward
                            #'previous-single-char-property-change
                            #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn))
                      (eobp)
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

(defun dime-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null dime-next-location-function)
    (error "No context for finding locations."))
  (funcall dime-next-location-function))

(defun dime-previous-location ()
  "Go to the previous location, depending on context.
When displaying XREF information, this goes to the previous reference."
  (interactive)
  (when (null dime-previous-location-function)
    (error "No context for finding locations."))
  (funcall dime-previous-location-function))

(defun dime-recompile-xref (&optional raw-prefix-arg)
  (interactive "P")
  (let ((dime-compilation-policy (dime-compute-policy raw-prefix-arg)))
    (let ((location (dime-xref-location-at-point))
          (dspec    (dime-xref-dspec-at-point)))
      (dime-recompile-locations
       (list location)
       (dime-rcurry #'dime-xref-recompilation-cont
                     (list dspec) (current-buffer))))))

(defun dime-recompile-all-xrefs (&optional raw-prefix-arg)
  (interactive "P")
  (let ((dime-compilation-policy (dime-compute-policy raw-prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (dime-all-xrefs))
        (when (dime-xref-has-location-p xref)
          (push (dime-xref.dspec xref) dspecs)
          (push (dime-xref.location xref) locations)))
      (dime-recompile-locations
       locations
       (dime-rcurry #'dime-xref-recompilation-cont
                     dspecs (current-buffer))))))

(defun dime-xref-recompilation-cont (results dspecs buffer)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.

  ;; FIXME: Should probably throw out the whole recompilation cruft
  ;; anyway.  -- helmut
  (with-current-buffer buffer
    (dime-compilation-finished (dime-aggregate-compilation-results results))
    (save-excursion
      (dime-xref-insert-recompilation-flags
       dspecs (loop for r in results collect
                    (or (dime-compilation-result.successp r)
                        (and (dime-compilation-result.notes r)
                             :complained)))))))

(defun dime-aggregate-compilation-results (results)
  `(:compilation-result
    ,(cl-reduce #'append (mapcar #'dime-compilation-result.notes results))
    ,(cl-every #'dime-compilation-result.successp results)
    ,(cl-reduce #'+ (mapcar #'dime-compilation-result.duration results))))

(defun dime-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (dime-column-max)))
    (goto-char (point-min))
    (loop for dspec in dspecs
          for result in compilation-results
          do (save-excursion
               (loop for dspec-at-point = (progn (search-forward dspec)
                                                 (dime-xref-dspec-at-point))
                     until (equal dspec-at-point dspec))
               (end-of-line) ; skip old status information.
               (dotimes (i (- max-column (current-column)))
                 (insert " "))
               (insert " ")
               (insert (format "[%s]"
                               (case result
                                 ((t)   :success)
                                 ((nil) :failure)
                                 (t     result))))))))


;;;; Macroexpansion

(define-minor-mode dime-macroexpansion-minor-mode
    "DIME mode for macroexpansion"
    nil
  " Macroexpand"
  '(("g" . dime-macroexpand-again)))

(cl-flet ((remap (from to)
         (dolist (mapping (where-is-internal from dime-mode-map))
           (define-key dime-macroexpansion-minor-mode-map mapping to))))
  (remap 'dime-macroexpand-1 'dime-macroexpand-1-inplace)
  (remap 'dime-macroexpand-all 'dime-macroexpand-all-inplace)
  (remap 'dime-compiler-macroexpand-1 'dime-compiler-macroexpand-1-inplace)
  (remap 'dime-expand-1
         'dime-expand-1-inplace)
  (remap 'advertised-undo 'dime-macroexpand-undo)
  (remap 'undo 'dime-macroexpand-undo))

(defun dime-macroexpand-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defun dime-sexp-at-point-for-macroexpansion ()
  "`dime-sexp-at-point' with special cases for LOOP."
  (let ((string (dime-sexp-at-point-or-error))
        (bounds (bounds-of-thing-at-point 'sexp))
        (char-at-point (substring-no-properties (thing-at-point 'char))))
    ;; DIME-SEXP-AT-POINT(-OR-ERROR) uses (THING-AT-POINT 'SEXP)
    ;; which is quite a bit botched: it returns "'(FOO BAR BAZ)" even
    ;; when point is placed _at the opening parenthesis_, and hence
    ;; "(FOO BAR BAZ)" wouldn't get expanded. Likewise for ",(...)",
    ;; ",@(...)" (would return "@(...)"!!), and "\"(...)".
    ;; So we better fix this up here:
    (when (string= char-at-point "(")
      (let ((char0 (elt string 0)))
        (when (member char0 '(?\' ?\, ?\" ?\@))
          (setf string (substring string 1))
          (incf (car bounds)))))
    (list string (cons (set-marker (make-marker) (car bounds))
                       (set-marker (make-marker) (cdr bounds))))))

(defvar dime-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed.
This variable specifies both what was expanded and how.")

(defun dime-eval-macroexpand (expander &optional string)
  (let ((string (or string
                    (car (dime-sexp-at-point-for-macroexpansion)))))
    (setq dime-eval-macroexpand-expression `(,expander ,string))
    (dime-eval-async dime-eval-macroexpand-expression
                      #'dime-initialize-macroexpansion-buffer)))

(defun dime-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (dime-eval-async dime-eval-macroexpand-expression
                    (dime-rcurry #'dime-initialize-macroexpansion-buffer
                                  (current-buffer))))

(defun dime-initialize-macroexpansion-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (dime-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                              ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun dime-create-macroexpansion-buffer ()
  (let ((name (dime-buffer-name :macroexpansion)))
    (dime-with-popup-buffer (name :project t :connection t
                                   :mode 'dylan-mode)
      (dime-mode 1)
      (dime-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun dime-eval-macroexpand-inplace (expander)
  "Substitute the sexp at point with its macroexpansion.

NB: Does not affect dime-eval-macroexpand-expression"
  (interactive)
  (destructuring-bind (string bounds)
      (dime-sexp-at-point-for-macroexpansion)
    (lexical-let* ((start (car bounds))
                   (end (cdr bounds))
                   (point (point))
                   (project dylan-buffer-module)
                   (buffer (current-buffer)))
      (dime-eval-async
       `(,expander ,string)
       (lambda (expansion)
         (with-current-buffer buffer
           (let ((buffer-read-only nil))
             (goto-char start)
             (delete-region start end)
             (dime-insert-indented expansion)
             (goto-char point))))))))

(defun dime-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (dime-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun dime-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (dime-eval-macroexpand-inplace
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun dime-macroexpand-all ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (dime-eval-macroexpand 'swank:swank-macroexpand-all))

(defun dime-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (dime-eval-macroexpand-inplace 'swank:swank-macroexpand-all))

(defun dime-compiler-macroexpand-1 (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (dime-eval-macroexpand
   (if repeatedly
       'swank:swank-compiler-macroexpand
       'swank:swank-compiler-macroexpand-1)))

(defun dime-compiler-macroexpand-1-inplace (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (dime-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-compiler-macroexpand
       'swank:swank-compiler-macroexpand-1)))

(defun dime-expand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (dime-eval-macroexpand
   (if repeatedly
       'swank:swank-expand
       'swank:swank-expand-1)))

(defun dime-expand-1-inplace (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (dime-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-expand
       'swank:swank-expand-1)))

(defun dime-format-string-expand ()
  "Expand the format-string at point and display it."
  (interactive)
  (dime-eval-macroexpand 'swank:swank-format-string-expand
                          (dime-string-at-point-or-error)))


;;;; Subprocess control

(defun dime-interrupt ()
  "Interrupt Dylan."
  (interactive)
  (cond ((dime-use-sigint-for-interrupt) (dime-send-sigint))
        (t (dime-dispatch-event `(:emacs-interrupt ,dime-current-thread)))))

(defun dime-quit ()
  (error "Not implemented properly.  Use `dime-interrupt' instead."))

(defun dime-quit-dylan (&optional kill)
  "Quit dylan, kill the inferior process and associated buffers."
  (interactive "P")
  (dime-quit-dylan-internal (dime-connection) 'dime-quit-sentinel kill))

(defun dime-quit-dylan-internal (connection sentinel kill)
  (let ((dime-dispatching-connection connection))
    (dime-eval-async '(swank:quit-lisp))
    (let* ((process (dime-inferior-process connection)))
      (set-process-filter connection  nil)
      (set-process-sentinel connection sentinel)
      (when (and kill process)
        (sleep-for 0.2)
        (unless (memq (process-status process) '(exit signal))
          (kill-process process))))))

(defun dime-quit-sentinel (process message)
  (assert (process-status process) 'closed)
  (let* ((inferior (dime-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (dime-net-close process)
    (message "Connection closed.")))


;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defcustom sldb-initial-restart-limit 6
  "Maximum number of restarts to display initially."
  :group 'dime-debugger
  :type 'integer)


;;;;; Local variables in the debugger buffer

(defvar-local sldb-condition nil
  "A list (DESCRIPTION TYPE) describing the condition being debugged.")

(defvar-local sldb-restarts nil
  "List of (NAME DESCRIPTION) for each available restart.")

(defvar-local sldb-level nil
  "Current debug level (recursion depth) displayed in buffer.")

(defvar-local sldb-backtrace-start-marker nil
  "Marker placed at the first frame of the backtrace.")

(defvar-local sldb-restart-list-start-marker nil
  "Marker placed at the first restart in the restart list.")

(defvar-local sldb-continuations nil
  "List of ids for pending continuation.")

;;;;; SLDB macros

;; some macros that we need to define before the first use

;; FIXME: rename
(defmacro in-sldb-face (name string)
  "Return STRING propertised with face sldb-NAME-face."
  (declare (indent 1))
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (cl-gensym "string")))
    `(let ((,var ,string))
      (dime-add-face ',facename ,var)
      ,var)))


;;;;; sldb-mode

(defvar sldb-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Dylan mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLDB mode.")

(define-derived-mode sldb-mode fundamental-mode "sldb"
  "Superior dylan debugger mode.
In addition to ordinary DIME commands, the following are
available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts
   \\[sldb-invoke-restart-by-name]   - invoke restart by name

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details
   \\[sldb-cycle] - cycle between restarts & backtrace
   \\[sldb-beginning-of-backtrace]   - beginning of backtrace
   \\[sldb-end-of-backtrace]   - end of backtrace

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to native debugger
   \\[sldb-break-with-system-debugger]   - switch to system debugger (gdb)
   \\[dime-interactive-eval]   - eval
   \\[sldb-inspect-condition]   - inspect signalled condition

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table sldb-mode-syntax-table)
  (dime-set-truncate-lines)
  ;; Make original dime-connection "sticky" for SLDB commands in this buffer
  (setq dime-buffer-connection (dime-connection)))

(set-keymap-parent sldb-mode-map dime-parent-map)

(dime-define-keys sldb-mode-map

  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([return] 'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ([follow-link] 'mouse-face)
  ("\C-i" 'sldb-cycle)
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ("e"    'sldb-eval-in-frame)
  ("d"    'sldb-pprint-eval-in-frame)
  ("D"    'sldb-disassemble)
  ("i"    'sldb-inspect-in-frame)
  ("n"    'sldb-down)
  ("p"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("<"    'sldb-beginning-of-backtrace)
  (">"    'sldb-end-of-backtrace)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("I"    'sldb-invoke-restart-by-name)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("x"    'sldb-next)
  ("o"    'sldb-out)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("A"    'sldb-break-with-system-debugger)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'dime-interactive-eval)
  ("\C-c\C-c" 'sldb-recompile-frame-source))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sldb-invoke-restart ,number)))
    (define-key sldb-mode-map (number-to-string number) fname)))


;;;;; SLDB buffer creation & update

(defun sldb-buffers (&optional connection)
  "Return a list of all sldb buffers (belonging to CONNECTION.)"
  (if connection
      (dime-filter-buffers (lambda ()
                              (and (eq dime-buffer-connection connection)
                                   (eq major-mode 'sldb-mode))))
      (dime-filter-buffers (lambda () (eq major-mode 'sldb-mode)))))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (dime-connection))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (eq dime-buffer-connection connection)
                         (eq dime-current-thread thread))))
                (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.
The buffer is chosen more or less randomly."
  (car (sldb-buffers)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (dime-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (dime-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq dime-buffer-connection connection
                  dime-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the debugged continuations for CONNECTION."
  (lexical-let ((accu '()))
    (dolist (b (sldb-buffers))
      (with-current-buffer b
        (when (eq dime-buffer-connection connection)
          (setq accu (append sldb-continuations accu)))))
    accu))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION &optional PLIST) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (dime-save-local-variables (dime-popup-restore-data)
        (sldb-mode))
      (setq dime-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert "\n\n" (in-sldb-face section "Restarts:") "\n")
      (setq sldb-restart-list-start-marker (point-marker))
      (sldb-insert-restarts restarts 0 sldb-initial-restart-limit)
      (insert "\n" (in-sldb-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (save-excursion
        (if frames
            (sldb-insert-frames (sldb-prune-initial-frames frames) t)
          (insert "[No backtrace]")))
      (run-hooks 'sldb-hook)
      (set-syntax-table lisp-mode-syntax-table))
    (dime-display-popup-buffer t)
    (sldb-recenter-region (point-min) (point))
    (setq buffer-read-only t)
    (when (and dime-stack-eval-tags
               ;; (y-or-n-p "Enter recursive edit? ")
               )
      (message "Entering recursive edit..")
      (recursive-edit))))

(defun sldb-activate (thread level select)
  "Display the debugger buffer for THREAD.
If LEVEL isn't the same as in the buffer reinitialize the buffer."
  (or (let ((buffer (sldb-find-buffer thread)))
        (when buffer
          (with-current-buffer buffer
            (when (equal sldb-level level)
              (when select (pop-to-buffer (current-buffer)))
              t))))
      (sldb-reinitialize thread level)))

(defun sldb-reinitialize (thread level)
  (dime-rex (thread level)
      ('(swank:debugger-info-for-emacs 0 10)
       nil thread)
    ((:ok result)
     (apply #'sldb-setup thread level result))))

(defun sldb-exit (thread level &optional stepping)
  "Exit from the debug level LEVEL."
  (when-let (sldb (sldb-find-buffer thread))
    (with-current-buffer sldb
      (cond (stepping
             (setq sldb-level nil)
             (run-with-timer 0.4 nil 'sldb-close-step-buffer sldb))
            (t
             (dime-popup-buffer-quit t))))))

(defun sldb-close-step-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (not sldb-level)
        (dime-popup-buffer-quit t)))))


;;;;;; SLDB buffer insertion

(defun sldb-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (destructuring-bind (message type extras) condition
    (dime-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (in-sldb-face topline message)
                              "\n"
                              (in-sldb-face condition type))
    (sldb-dispatch-extras extras)))

(defvar sldb-extras-hooks)

(defun sldb-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (destructure-case extra
      ((:show-frame-source n)
       (sldb-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sldb-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sldb-insert-restarts (restarts start count)
  "Insert RESTARTS and add the needed text props
RESTARTS should be a list ((NAME DESCRIPTION) ...)."
  (let* ((len (length restarts))
         (end (if count (min (+ start count) len) len)))
    (loop for (name string) in (cl-subseq restarts start end)
          for number from start
          do (dime-insert-propertized
               `(,@nil restart ,number
                       sldb-default-action sldb-invoke-restart
                       mouse-face highlight)
               " " (in-sldb-face restart-number (number-to-string number))
               ": ["  (in-sldb-face restart-type name) "] "
               (in-sldb-face restart string))
             (insert "\n"))
    (when (< end len)
      (let ((pos (point)))
        (dime-insert-propertized
         (list 'sldb-default-action
               (dime-rcurry #'sldb-insert-more-restarts restarts pos end))
         " --more--\n")))))

(defun sldb-insert-more-restarts (restarts position start)
  (goto-char position)
  (let ((inhibit-read-only t))
    (delete-region position (1+ (line-end-position)))
    (sldb-insert-restarts restarts start nil)))

(defun sldb-frame.string (frame)
  (destructuring-bind (_ str &optional _) frame str))

(defun sldb-frame.number (frame)
  (destructuring-bind (n _ &optional _) frame n))

(defun sldb-frame.plist (frame)
  (destructuring-bind (_ _ &optional plist) frame plist))

(defun sldb-frame-restartable-p (frame)
  (and (plist-get (sldb-frame.plist frame) :restartable) t))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*swank\\>"))
    (or (loop for frame in frames
              until (string-match rx (sldb-frame.string frame))
              collect frame)
        frames)))

(defun sldb-insert-frames (frames more)
  "Insert FRAMES into buffer.
If MORE is non-nil, more frames are on the Dylan stack."
  (mapc #'sldb-insert-frame frames)
  (when more
    (dime-insert-propertized
     `(,@nil sldb-default-action sldb-fetch-more-frames
             sldb-previous-frame-number
             ,(sldb-frame.number (first (last frames)))
             point-entered sldb-fetch-more-frames
             start-open t
             face sldb-section-face
             mouse-face highlight)
     " --more--")
    (insert "\n")))

(defun sldb-compute-frame-face (frame)
  (if (sldb-frame-restartable-p frame)
      'sldb-restartable-frame-line-face
      'sldb-frame-line-face))

(defun sldb-insert-frame (frame &optional face)
  "Insert FRAME with FACE at point.
If FACE is nil, `sldb-compute-frame-face' is used to determine the face."
  (setq face (or face (sldb-compute-frame-face frame)))
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame))
        (props `(frame ,frame sldb-default-action sldb-toggle-details)))
    (dime-propertize-region props
      (dime-propertize-region '(mouse-face highlight)
        (insert " " (in-sldb-face frame-label (format "%2d:" number)) " ")
        (dime-insert-indented
         (dime-add-face face string)))
      (insert "\n"))))

(defun sldb-fetch-more-frames (&rest ignore)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        (prev (get-text-property (point) 'sldb-previous-frame-number)))
    ;; we may be called twice, PREV is nil the second time
    (when prev
      (let* ((count 40)
             (from (1+ prev))
             (to (+ from count))
             (frames (dime-eval `(swank:backtrace ,from ,to)))
             (more (dime-length= frames count))
             (pos (point)))
        (delete-region (line-beginning-position) (point-max))
        (sldb-insert-frames frames more)
        (goto-char pos)))))


;;;;;; SLDB examining text props

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart)
      (error "No restart at point")))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-var-number-at-point ()
  (let ((var (get-text-property (point) 'var)))
    (cond (var var)
	  (t (error "No variable at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-frame-region ()
  (dime-property-bounds 'frame))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (when (> (point) sldb-backtrace-start-marker)
    (goto-char (previous-single-char-property-change
                (if (get-text-property (point) 'frame)
                    (car (sldb-frame-region))
                    (point))
                'frame
                nil sldb-backtrace-start-marker))))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))
    ;; Recenter to bottom of the window; -2 to account for the
    ;; empty last line displayed in sldb buffers.
    (recenter -2)))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


;;;;;; SLDB recenter & redisplay

;; FIXME: these functions need factorization

(defun dime-show-buffer-position (position &optional recenter)
  "Ensure sure that the POSITION in the current buffer is visible."
  (let ((window (display-buffer (current-buffer) t)))
    (save-selected-window
      (select-window window)
      (goto-char position)
      (ecase recenter
        (top    (recenter 0))
        (center (recenter))
        ((nil)
         (unless (pos-visible-in-window-p)
           (cond ((= (current-column) 0) (recenter 1))
                 (t (recenter)))))))))

(defun sldb-recenter-region (start end &optional center)
  "Make the region from START to END visible.
Avoid point motions, if possible.
Minimize scrolling, if CENTER is nil.
If CENTER is true, scroll enough to center the region in the window."
  (let ((pos (point))  (lines (count-screen-lines start end t)))
    (assert (and (<= start pos) (<= pos end)))
    ;;(sit-for 0)
    (cond ((and (pos-visible-in-window-p start)
                (pos-visible-in-window-p end)))
          ((< lines (window-height))
           (cond (center (recenter (+ (/ (- (window-height) 1 lines)
                                         2)
                                      (dime-count-lines start pos))))
                 (t (recenter (+ (- (window-height) 1 lines)
                                 (dime-count-lines start pos))))))
          (t
           (goto-char start)
           (recenter 0)
           (cond ((pos-visible-in-window-p pos)
                  (goto-char pos))
                 (t
                  (goto-char start)
                  (unless noninteractive ; for running the test suite
                    (forward-line (- (window-height) 2)))))))))

;; not sure yet, whether this is a good idea.
(defmacro dime-save-coordinates (origin &rest body)
  "Restore line and column relative to ORIGIN, after executing BODY.

This is useful if BODY deletes and inserts some text but we want to
preserve the current row and column as closely as possible."
  (declare (indent 1))
  (let ((base (make-symbol "base"))
        (goal (make-symbol "goal"))
        (mark (make-symbol "mark")))
    `(let* ((,base ,origin)
            (,goal (dime-coordinates ,base))
            (,mark (point-marker)))
       (set-marker-insertion-type ,mark t)
       (prog1 (save-excursion ,@body)
         (dime-restore-coordinate ,base ,goal ,mark)))))

(defun dime-coordinates (origin)
  ;; Return a pair (X . Y) for the column and line distance to ORIGIN.
  (let ((y (dime-count-lines origin (point)))
        (x (save-excursion
             (- (current-column)
                (progn (goto-char origin) (current-column))))))
    (cons x y)))

(defun dime-restore-coordinate (base goal limit)
  ;; Move point to GOAL. Coordinates are relative to BASE.
  ;; Don't move beyond LIMIT.
  (save-restriction
    (narrow-to-region base limit)
    (goto-char (point-min))
    (let ((col (current-column)))
      (forward-line (cdr goal))
      (when (and (eobp) (bolp) (not (bobp)))
        (backward-char))
      (move-to-column (+ col (car goal))))))

(defun dime-count-lines (start end)
  "Return the number of lines between START and END.
This is 0 if START and END at the same line."
  (- (count-lines start end)
     (if (save-excursion (goto-char end) (bolp)) 0 1)))


;;;;; SLDB commands

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos &rest _)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-cycle ()
  "Cycle between restart list and backtrace."
  (interactive)
  (let ((pt (point)))
    (cond ((< pt sldb-restart-list-start-marker)
           (goto-char sldb-restart-list-start-marker))
          ((< pt sldb-backtrace-start-marker)
           (goto-char sldb-backtrace-start-marker))
          (t
           (goto-char sldb-restart-list-start-marker)))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (save-excursion
        (sldb-insert-frames (dime-eval `(swank:backtrace ,(1+ last) nil))
                            nil)))))


;;;;;; SLDB show source

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (dime-eval-async
   `(swank:frame-source-location ,frame-number)
   (lambda (source-location)
     (destructure-case source-location
       ((:error message)
        (message "%s" message)
        (ding))
       (t
        (dime-show-source-location source-location))))))

(defun dime-show-source-location (source-location &optional no-highlight-p)
  (save-selected-window   ; show the location, but don't hijack focus.
    (dime-goto-source-location source-location)
    (unless no-highlight-p (dime-highlight-sexp))
    (dime-show-buffer-position (point))))

(defun dime-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (dime-flash-region start end)))

(defun dime-highlight-line (&optional timeout)
  (dime-flash-region (+ (line-beginning-position) (current-indentation))
                      (line-end-position)
                      timeout))


;;;;;; SLDB toggle details

(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  ;; fetch and display info about local variables and catch tags
  (destructuring-bind (start end frame locals catches) (sldb-frame-details)
    (dime-save-coordinates start
      (delete-region start end)
      (dime-propertize-region `(frame ,frame details-visible-p t)
        (sldb-insert-frame frame (if (sldb-frame-restartable-p frame)
                                     'sldb-restartable-frame-line-face
                                     ;; FIXME: can we somehow merge the two?
                                     'sldb-detailed-frame-line-face))
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert indent1 (in-sldb-face section
                            (if locals "Locals:" "[No Locals]")) "\n")
          (sldb-insert-locals locals indent2 frame)
          (when catches
            (insert indent1 (in-sldb-face section "Catch-tags:") "\n")
            (dolist (tag catches)
              (dime-propertize-region `(catch-tag ,tag)
                (insert indent2 (in-sldb-face catch-tag (format "%s" tag))
                        "\n"))))
          (setq end (point)))))
    (sldb-recenter-region start end)))

(defun sldb-frame-details ()
  ;; Return a list (START END FRAME LOCALS CATCHES) for frame at point.
  (let* ((frame (get-text-property (point) 'frame))
         (num (car frame)))
    (destructuring-bind (start end) (sldb-frame-region)
      (list* start end frame
             (dime-eval `(swank:frame-locals-and-catch-tags ,num))))))

(defvar sldb-insert-frame-variable-value-function
  'sldb-insert-frame-variable-value)

(defun sldb-insert-locals (vars prefix frame)
  "Insert VARS and add PREFIX at the beginning of each inserted line.
VAR should be a plist with the keys :name, :id, and :value."
  (loop for i from 0
        for var in vars do
        (destructuring-bind (&key name id value) var
          (dime-propertize-region (list 'sldb-default-action 'sldb-inspect-var
                                         'var i)
            (insert prefix
                    (in-sldb-face local-name
                      (concat name (if (zerop id) "" (format "#%d" id))))
                    " = ")
            (funcall sldb-insert-frame-variable-value-function value frame i)
            (insert "\n")))))

(defun sldb-insert-frame-variable-value (value frame index)
  (insert (in-sldb-face local-value value)))

(defun sldb-hide-frame-details ()
  ;; delete locals and catch tags, but keep the function name and args.
  (destructuring-bind (start end) (sldb-frame-region)
    (let ((frame (get-text-property (point) 'frame)))
      (dime-save-coordinates start
        (delete-region start end)
        (dime-propertize-region '(details-visible-p nil)
          (sldb-insert-frame frame))))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:sldb-disassemble ,frame)
                      (lambda (result)
			(dime-show-description result nil)))))


;;;;;; SLDB eval and inspect

(defun sldb-eval-in-frame (string)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (list (dime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:eval-string-in-frame ,string ,number)
                      (if current-prefix-arg
                          'dime-write-string
                        'dime-display-eval-result))))

(defun sldb-pprint-eval-in-frame (string)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (list (dime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      (lambda (result)
			(dime-show-description result nil)))))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (dime-read-from-minibuffer
                      "Inspect in frame (evaluated): "
                      (dime-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:inspect-in-frame ,string ,number)
                      'dime-open-inspector)))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (dime-eval-async `(swank:inspect-frame-var ,frame ,var)
                      'dime-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (dime-eval-async '(swank:inspect-current-condition)
                    'dime-open-inspector))


;;;;;; SLDB movement

(defun sldb-down ()
  "Select next frame."
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  "Select previous frame."
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p) (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-show-source)
    (sldb-toggle-details t)))

(defun sldb-details-up ()
  "Select previous frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  "Select next frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-down))


;;;;;; SLDB restarts

(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (assert sldb-restarts () "sldb-quit called outside of sldb buffer")
  (dime-rex () ('(swank:throw-to-toplevel))
    ((:ok x) (error "sldb-quit returned [%s]" x))
    ((:abort _) _)))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (assert sldb-restarts () "sldb-continue called outside of sldb buffer")
  (dime-rex ()
      ('(swank:sldb-continue))
    ((:ok _)
     (message "No restart named continue")
     (ding))
    ((:abort _) _)))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (dime-eval-async '(swank:sldb-abort)
                    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER (index into `sldb-restarts') specifies the
restart to invoke, otherwise use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (dime-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort _) _))))

(defun sldb-invoke-restart-by-name (restart-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Restart: " sldb-restarts nil t
                                        ""
                                        'sldb-invoke-restart-by-name))))
  (sldb-invoke-restart (cl-position restart-name sldb-restarts
                                    :test 'string= :key 'first)))

(defun sldb-break-with-default-debugger (&optional dont-unwind)
  "Enter default debugger."
  (interactive "P")
  (dime-rex ()
      ((list 'swank:sldb-break-with-default-debugger
             (not (not dont-unwind)))
       nil dime-current-thread)
    ((:abort _) _)))

(defun sldb-break-with-system-debugger (&optional lightweight)
  "Enter system debugger (gdb)."
  (interactive "P")
  (dime-attach-gdb dime-buffer-connection lightweight))

(defun dime-attach-gdb (connection &optional lightweight)
  "Run `gud-gdb'on the connection with PID `pid'.

If `lightweight' is given, do not send any request to the
inferior Dylan (e.g. to obtain default gdb config) but only
operate from the Emacs side; intended for cases where the Dylan is
truly screwed up."
  (interactive
   (list (dime-read-connection "Attach gdb to: " (dime-connection)) "P"))
  (let ((pid  (dime-pid connection))
        (file (dime-dylan-implementation-program connection))
        (commands (unless lightweight
                    (let ((dime-dispatching-connection connection))
                      (dime-eval `(swank:gdb-initial-commands))))))
    (gud-gdb (format "gdb -p %d %s" pid (or file "")))
    (with-current-buffer gud-comint-buffer
      (dolist (cmd commands)
        ;; First wait until gdb was initialized, then wait until current
        ;; command was processed.
        (while (not (looking-back comint-prompt-regexp))
          (sit-for 0.01))
        ;; We do not use `gud-call' because we want the initial commands
        ;; to be displayed by the user so he knows what he's got.
        (insert cmd)
        (comint-send-input)))))

(defun dime-read-connection (prompt &optional initial-value)
  "Read a connection from the minibuffer. Returns the net
process, or nil."
  (assert (memq initial-value dime-net-processes))
  (cl-flet ((connection-identifier (p)
           (format "%s (pid %d)" (dime-connection-name p) (dime-pid p))))
    (let ((candidates (mapcar (lambda (p)
                                  (cons (connection-identifier p) p))
                              dime-net-processes)))
      (cdr (assoc (completing-read prompt candidates
                                   nil t (connection-identifier initial-value))
                  candidates)))))

(defun sldb-step ()
  "Step to next basic-block boundary."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:sldb-step ,frame))))

(defun sldb-next ()
  "Step over call."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:sldb-next ,frame))))

(defun sldb-out ()
  "Resume stepping after returning from this function."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:sldb-out ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (dime-eval-async `(swank:sldb-break-on-return ,frame)
                      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (dime-read-symbol-name "Function: " t)))
  (dime-eval-async `(swank:sldb-break ,name)
                    (lambda (msg) (message "%s" msg))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (dime-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (dime-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort _) _))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (dime-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort _) _))))

(defun dime-toggle-break-on-signals ()
  "Toggle the value of *break-on-signals*."
  (interactive)
  (dime-eval-async `(swank:toggle-break-on-signals)
    (lambda (msg) (message "%s" msg))))


;;;;;; SLDB recompilation commands

(defun sldb-recompile-frame-source (&optional raw-prefix-arg)
  (interactive "P")
  (dime-eval-async
   `(swank:frame-source-location ,(sldb-frame-number-at-point))
   (lexical-let ((policy (dime-compute-policy raw-prefix-arg)))
     (lambda (source-location)
       (destructure-case source-location
         ((:error message)
          (message "%s" message)
          (ding))
         (t
          (let ((dime-compilation-policy policy))
            (dime-recompile-location source-location))))))))


;;;; Thread control panel

(defvar dime-threads-buffer-name (dime-buffer-name :threads))
(defvar dime-threads-buffer-timer nil)

(defcustom dime-threads-update-interval nil
  "Interval at which the list of threads will be updated."
  :type '(choice
          (number :value 0.5)
          (const nil))
  :group 'dime-ui)

(defun dime-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name dime-threads-buffer-name))
    (dime-with-popup-buffer (name :connection t
                                   :mode 'dime-thread-control-mode)
      (dime-update-threads-buffer)
      (goto-char (point-min))
      (when dime-threads-update-interval
        (when dime-threads-buffer-timer
          (cancel-timer dime-threads-buffer-timer))
        (setq dime-threads-buffer-timer
              (run-with-timer
               dime-threads-update-interval
               dime-threads-update-interval
               'dime-update-threads-buffer)))
      (setq dime-popup-buffer-quit-function 'dime-quit-threads-buffer))))

(defun dime-longest-lines (list-of-lines)
  (let ((lengths (make-list (length (car list-of-lines)) 0)))
    (cl-flet ((process-line (line)
             (loop for element in line
                   for length on lengths
                   do (setf (car length)
                            (max (length (prin1-to-string element t))
                                 (car length))))))
      (mapc 'process-line list-of-lines)
      lengths)))

(defvar dime-thread-index-to-id nil)

(defun dime-quit-threads-buffer (&optional _)
  (when dime-threads-buffer-timer
    (cancel-timer dime-threads-buffer-timer)
    (setq dime-threads-buffer-timer nil))
  (dime-popup-buffer-quit t)
  (setq dime-thread-index-to-id nil)
  (dime-eval-async `(swank:quit-thread-browser)))

(defun dime-update-threads-buffer ()
  (interactive)
  (with-current-buffer dime-threads-buffer-name
    (dime-eval-async '(swank:list-threads)
      'dime-display-threads)))

(defun dime-move-point (position)
  "Move point in the current buffer and in the window the buffer is displayed."
  (let ((window (get-buffer-window (current-buffer) t)))
    (goto-char position)
    (when window
      (set-window-point window position))))

;;; FIXME: the region selection is jumping
(defun dime-display-threads (threads)
  (with-current-buffer dime-threads-buffer-name
    (let* ((inhibit-read-only t)
           (index (get-text-property (point) 'thread-id))
           (old-thread-id (and (numberp index)
                               (elt dime-thread-index-to-id index)))
           (old-line (line-number-at-pos))
           (old-column (current-column)))
      (setq dime-thread-index-to-id (mapcar 'car (cdr threads)))
      (erase-buffer)
      (dime-insert-threads threads)
      (let ((new-position (cl-position old-thread-id threads :key 'car)))
        (goto-char (point-min))
        (forward-line (1- (or new-position old-line)))
        (move-to-column old-column)
        (dime-move-point (point))))))

(defvar *dime-threads-table-properties*
  '(nil (face bold)))

(defun dime-format-threads-labels (threads)
  (let ((labels (mapcar (lambda (x)
                          (capitalize (substring (symbol-name x) 1)))
                        (car threads))))
    (cons labels (cdr threads))))

(defun dime-insert-thread (thread longest-lines)
  (unless (bolp) (insert "\n"))
  (loop for i from 0
        for align in longest-lines
        for element in thread
        for string = (prin1-to-string element t)
        for property = (nth i *dime-threads-table-properties*)
        do
        (if property
            (dime-insert-propertized property string)
            (insert string))
        (insert-char ?\  (- align (length string) -3))))

(defun dime-insert-threads (threads)
  (let* ((threads (dime-format-threads-labels threads))
         (longest-lines (dime-longest-lines threads))
         (labels (let (*dime-threads-table-properties*)
                   (with-temp-buffer
                     (dime-insert-thread (car threads) longest-lines)
                     (buffer-string)))))
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  labels))
    (loop for index from 0
          for thread in (cdr threads)
          do
          (dime-propertize-region `(thread-id ,index)
            (dime-insert-thread thread longest-lines)))))


;;;;; Major mode

(define-derived-mode dime-thread-control-mode fundamental-mode
  "Threads"
  "DIME Thread Control Panel Mode.

\\{dime-thread-control-mode-map}
\\{dime-popup-buffer-mode-map}"
  (when dime-truncate-lines
    (set (make-local-variable 'truncate-lines) t))
  (setq buffer-undo-list t))

(dime-define-keys dime-thread-control-mode-map
  ("a" 'dime-thread-attach)
  ("d" 'dime-thread-debug)
  ("g" 'dime-update-threads-buffer)
  ("k" 'dime-thread-kill))

(defun dime-thread-kill ()
  (interactive)
  (dime-eval `(cl:mapc 'swank:kill-nth-thread
                        ',(dime-get-properties 'thread-id)))
  (call-interactively 'dime-update-threads-buffer))

(defun dime-get-region-properties (prop start end)
  (loop for position = (if (get-text-property start prop)
                           start
                           (next-single-property-change start prop))
        then (next-single-property-change position prop)
        while (<= position end)
        collect (get-text-property position prop)))

(defun dime-get-properties (prop)
  (if (use-region-p)
      (dime-get-region-properties prop
                                   (region-beginning)
                                   (region-end))
      (let ((value (get-text-property (point) prop)))
        (when value
          (list value)))))

(defun dime-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id))
        (file (dime-swank-port-file)))
    (dime-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (dime-read-port-and-connect nil))

(defun dime-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (dime-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode dime-connection-list-mode fundamental-mode
  "Dime-Connections"
  "DIME Connection List Mode.

\\{dime-connection-list-mode-map}
\\{dime-popup-buffer-mode-map}"
  (when dime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(dime-define-keys dime-connection-list-mode-map
  ("d"         'dime-connection-list-make-default)
  ("g"         'dime-update-connection-list)
  ((kbd "C-k") 'dime-quit-connection-at-point)
  ("R"         'dime-restart-connection-at-point))

(defun dime-connection-at-point ()
  (or (get-text-property (point) 'dime-connection)
      (error "No connection at point")))

(defun dime-quit-connection-at-point (connection)
  (interactive (list (dime-connection-at-point)))
  (let ((dime-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (dime-quit-dylan t)
    (while (memq connection dime-net-processes)
      (when (time-less-p end (current-time))
        (message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0 100)))
  (dime-update-connection-list))

(defun dime-restart-connection-at-point (connection)
  (interactive (list (dime-connection-at-point)))
  (let ((dime-dispatching-connection connection))
    (dime-restart-inferior-dylan)))

(defun dime-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (dime-select-connection (dime-connection-at-point))
  (dime-update-connection-list))

(defvar dime-connections-buffer-name (dime-buffer-name :connections))

(defun dime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (dime-with-popup-buffer (dime-connections-buffer-name
                            :mode 'dime-connection-list-mode)
    (dime-draw-connection-list)))

(defun dime-update-connection-list ()
 "Display a list of all connections."
 (interactive)
 (let ((pos (point))
       (inhibit-read-only t))
   (erase-buffer)
   (dime-draw-connection-list)
   (goto-char pos)))

(defun dime-draw-connection-list ()
  (let ((default-pos nil)
        (default dime-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse dime-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (dime-insert-propertized
       (list 'dime-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (dime-connection-number p)
               (dime-connection-name p)
               (or (process-id p) (process-contact p))
               (dime-pid p)
               (dime-dylan-implementation-type p))))
    (when default
      (goto-char default-pos))))


;;;; Inspector

(defgroup dime-inspector nil
  "Inspector faces."
  :prefix "dime-inspector-"
  :group 'dime)

(defface dime-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'dime-inspector)

(defface dime-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'dime-inspector)

(defface dime-inspector-value-face
  (if (dime-face-inheritance-possible-p)
      '((t (:inherit font-lock-builtin-face)))
    '((((background light)) (:foreground "MediumBlue" :bold t))
      (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for things which can themselves be inspected."
  :group 'dime-inspector)

(defface dime-inspector-action-face
  (if (dime-face-inheritance-possible-p)
      '((t (:inherit font-lock-warning-face)))
    '((t (:foreground "OrangeRed"))))
  "Face for labels of inspector actions."
  :group 'dime-inspector)

(defface dime-inspector-type-face
    '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'dime-inspector)

(defvar dime-inspector-mark-stack '())
(defvar dime-saved-window-config)

(defun dime-inspect (string)
  "Eval an expression and inspect the result."
  (interactive
   (list (dime-read-from-minibuffer "Inspect value (evaluated): "
				     (dime-sexp-at-point))))
  (dime-eval-async `(swank:init-inspector ,string) 'dime-open-inspector))

(define-derived-mode dime-inspector-mode fundamental-mode
  "Dime-Inspector"
  "
\\{dime-inspector-mode-map}
\\{dime-popup-buffer-mode-map}"
  (set-syntax-table lisp-mode-syntax-table)
  (dime-set-truncate-lines)
  (setq buffer-read-only t))

(defun dime-inspector-buffer ()
  (or (get-buffer (dime-buffer-name :inspector))
      (dime-with-popup-buffer ((dime-buffer-name :inspector)
                                :mode 'dime-inspector-mode)
        (setq dime-inspector-mark-stack '())
        (buffer-disable-undo)
        (make-local-variable 'dime-saved-window-config)
        (setq dime-popup-buffer-quit-function 'dime-inspector-quit)
        (setq dime-saved-window-config (current-window-configuration))
        (current-buffer))))

(defmacro dime-inspector-fontify (face string)
  `(dime-add-face ',(intern (format "dime-inspector-%s-face" face)) ,string))

(defvar dime-inspector-insert-ispec-function 'dime-inspector-insert-ispec)

(defun dime-open-inspector (inspected-parts &optional point hook)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT. If HOOK is provided, it is added to local
KILL-BUFFER hooks for the inspector buffer."
  (with-current-buffer (dime-inspector-buffer)
    (when hook
      (add-hook 'kill-buffer-hook hook t t))
    (setq dime-buffer-connection (dime-current-connection))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (&key id title content) inspected-parts
        (macrolet ((fontify (face string)
                            `(dime-inspector-fontify ,face ,string)))
          (dime-propertize-region
              (list 'dime-part-number id
                 'mouse-face 'highlight
                 'face 'dime-inspector-value-face)
            (insert title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n" (fontify label "--------------------") "\n")
          (save-excursion
            (dime-inspector-insert-content content))
          (pop-to-buffer (current-buffer))
          (when point
            (check-type point cons)
            (ignore-errors
              (goto-char (point-min))
              (forward-line (1- (car point)))
              (move-to-column (cdr point)))))))))

(defvar dime-inspector-limit 500)

(defun dime-inspector-insert-content (content)
  (dime-inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (dime-inspector-insert-chunk chunk t t)))))

(defun dime-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (dime-inspector-insert-more-button start t))
    (mapc #'dime-inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (dime-inspector-insert-more-button end nil))))

(defun dime-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (dime-propertize-region
           (list 'dime-part-number id
                 'mouse-face 'highlight
                 'face 'dime-inspector-value-face)
         (insert string)))
      ((:label string)
       (insert (dime-inspector-fontify label string)))
      ((:action string id)
       (dime-insert-propertized (list 'dime-action-number id
                                       'mouse-face 'highlight
                                       'face 'dime-inspector-action-face)
                                 string)))))

(defun dime-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  ;; FIXME: why would somebody narrow the buffer?
  (save-restriction
    (widen)
    (cons (line-number-at-pos)
          (current-column))))

(defun dime-inspector-property-at-point ()
  (let ((properties '(dime-part-number dime-range-button
                      dime-action-number)))
    (cl-flet ((find-property (point)
             (loop for property in properties
                   for value = (get-text-property point property)
                   when value
                   return (list property value))))
      (or (find-property (point))
          (find-property (1- (point)))))))

(defun dime-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (let ((opener (lexical-let ((point (dime-inspector-position)))
                  (lambda (parts)
                    (when parts
                      (dime-open-inspector parts point)))))
        (new-opener (lambda (parts)
                      (when parts
                        (dime-open-inspector parts)))))
    (destructuring-bind (property value)
        (dime-inspector-property-at-point)
        (case property
          (dime-part-number
           (dime-eval-async `(swank:inspect-nth-part ,value)
                              new-opener)
           (push (dime-inspector-position) dime-inspector-mark-stack))
          (dime-range-button
           (dime-inspector-fetch-more value))
          (dime-action-number
           (dime-eval-async `(swank::inspector-call-nth-action ,value)
                             opener))
          (t (error "No object at point"))))))

(defun dime-inspector-operate-on-click (event)
  "Move to events' position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'dime-part-number)
                    (get-text-property point 'dime-range-button)
                    (get-text-property point 'dime-action-number)))
           (goto-char point)
           (dime-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun dime-inspector-pop ()
  "Reinspect the previous object."
  (interactive)
  (dime-eval-async
   `(swank:inspector-pop)
   (lambda (result)
     (cond (result
	    (dime-open-inspector result (pop dime-inspector-mark-stack)))
	   (t
	    (message "No previous object")
	    (ding))))))

(defun dime-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (let ((result (dime-eval `(swank:inspector-next))))
    (cond (result
	   (push (dime-inspector-position) dime-inspector-mark-stack)
	   (dime-open-inspector result))
	  (t (message "No next object")
	     (ding)))))

(defun dime-inspector-quit (&optional kill-buffer)
  "Quit the inspector and kill the buffer."
  (interactive)
  (dime-eval-async `(swank:quit-inspector))
  (set-window-configuration dime-saved-window-config)
  (dime-popup-buffer-quit t))

;; FIXME: first return value is just point.
;; FIXME: could probably use dime-search-property.
(defun dime-find-inspectable-object (direction limit)
  "Find the next/previous inspectable object.
DIRECTION can be either 'next or 'prev.
LIMIT is the maximum or minimum position in the current buffer.

Return a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned."
  (let ((finder (ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'dime-part-number nil limit)))
          (setq prop (get-text-property newpos 'dime-part-number))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun dime-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (destructuring-bind (pos foundp)
          (dime-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char minpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (destructuring-bind (pos foundp)
          (dime-find-inspectable-object 'prev minpos)
        ;; DIME-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
            (if (not previously-wrapped-p) ; cycle detection
                (progn (goto-char maxpos) (setq previously-wrapped-p t))
                (error "No inspectable objects")))))))

(defun dime-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (dime-inspector-next-inspectable-object (- arg)))

(defun dime-inspector-describe ()
  (interactive)
  (dime-eval-describe `(swank:describe-inspectee)))

(defun dime-inspector-pprint (part)
  (interactive (list (or (get-text-property (point) 'dime-part-number)
                         (error "No part at point"))))
  (dime-eval-describe `(swank:pprint-inspector-part ,part)))

(defun dime-inspector-eval (string)
  "Eval an expression in the context of the inspected object."
  (interactive (list (dime-read-from-minibuffer "Inspector eval: ")))
  (dime-eval-with-transcript `(swank:inspector-eval ,string)))

(defun dime-inspector-history ()
  "Show the previously inspected objects."
  (interactive)
  (dime-eval-describe `(swank:inspector-history)))

(defun dime-inspector-show-source (part)
  (interactive (list (or (get-text-property (point) 'dime-part-number)
                         (error "No part at point"))))
  (dime-eval-async
   `(swank:find-source-location-for-emacs '(:inspector ,part))
   #'dime-show-source-location))

(defun dime-inspector-reinspect ()
  (interactive)
  (dime-eval-async `(swank:inspector-reinspect)
                    (lexical-let ((point (dime-inspector-position)))
                      (lambda (parts)
                        (dime-open-inspector parts point)))))

(defun dime-inspector-toggle-verbose ()
  (interactive)
  (dime-eval-async `(swank:inspector-toggle-verbose)
                    (lexical-let ((point (dime-inspector-position)))
                      (lambda (parts)
                        (dime-open-inspector parts point)))))

(defun dime-inspector-insert-more-button (index previous)
  (dime-insert-propertized
   (list 'dime-range-button (list index previous)
         'mouse-face 'highlight
         'face 'dime-inspector-action-face)
   (if previous " [--more--]\n" " [--more--]")))

(defun dime-inspector-fetch-all ()
  "Fetch all inspector contents and go to the end."
  (interactive)
  (goto-char (1- (point-max)))
  (let ((button (get-text-property (point) 'dime-range-button)))
    (when button
      (let (dime-inspector-limit)
        (dime-inspector-fetch-more button)))))

(defun dime-inspector-fetch-more (button)
  (destructuring-bind (index prev) button
    (dime-inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (dime-rcurry
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (apply #'delete-region (dime-property-bounds 'dime-range-button))
          (dime-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun dime-inspector-fetch-chunk (chunk prev cont)
  (dime-inspector-fetch chunk dime-inspector-limit prev cont))

(defun dime-inspector-fetch (chunk limit prev cont)
  (destructuring-bind (from to) (dime-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (dime-eval-async
            `(swank:inspector-range ,from ,to)
            (dime-rcurry (lambda (chunk2 chunk1 limit prev cont)
                            (dime-inspector-fetch
                             (dime-inspector-join-chunks chunk1 chunk2)
                             limit prev cont))
                          chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun dime-inspector-next-range (chunk limit prev)
  (destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun dime-inspector-join-chunks (chunk1 chunk2)
  (destructuring-bind (i1 l1 s1 e1) chunk1
    (destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(set-keymap-parent dime-inspector-mode-map dime-parent-map)

(dime-define-keys dime-inspector-mode-map
  ([return] 'dime-inspector-operate-on-point)
  ("\C-m"   'dime-inspector-operate-on-point)
  ([mouse-2] 'dime-inspector-operate-on-click)
  ("l" 'dime-inspector-pop)
  ("n" 'dime-inspector-next)
  (" " 'dime-inspector-next)
  ("d" 'dime-inspector-describe)
  ("p" 'dime-inspector-pprint)
  ("e" 'dime-inspector-eval)
  ("h" 'dime-inspector-history)
  ("g" 'dime-inspector-reinspect)
  ("v" 'dime-inspector-toggle-verbose)
  ("\C-i" 'dime-inspector-next-inspectable-object)
  ([(shift tab)] 'dime-inspector-previous-inspectable-object) ; Emacs translates S-TAB
  ([backtab]     'dime-inspector-previous-inspectable-object) ; to BACKTAB on X.
  ("." 'dime-inspector-show-source)
  (">" 'dime-inspector-fetch-all))


;;;; Buffer selector

(defvar dime-selector-methods nil
  "List of buffer-selection methods for the `dime-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar dime-selector-other-window nil
  "If non-nil use switch-to-buffer-other-window.")

(defun dime-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-dime-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car dime-selector-methods)))
  (let* ((dime-selector-other-window other-window)
         (ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (cl-find ch dime-selector-methods :key #'car)))
    (cond (method
           (funcall (third method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (dime-selector)))))

(defmacro def-dime-selector-method (key description &rest body)
  "Define a new `dime-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda ()
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (dime-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq dime-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key dime-selector-methods :key #'car))
                    #'< :key #'car))))

(def-dime-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (loop for (key line function) in dime-selector-methods
          do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (dime-selector)
  (current-buffer))

(pushnew (list ?4 "Select in other window" (lambda () (dime-selector t)))
         dime-selector-methods :key #'car)

(def-dime-selector-method ?q "Abort."
  (top-level))

(def-dime-selector-method ?i
  "*inferior-dylan* buffer."
  (cond ((and (dime-connected-p) (dime-process))
         (process-buffer (dime-process)))
        (t
         "*inferior-dylan*")))

(def-dime-selector-method ?v
  "*dime-events* buffer."
  dime-event-buffer-name)

(def-dime-selector-method ?l
  "most recently visited dylan-mode buffer."
  (dime-recently-visited-buffer 'dylan-mode))

(def-dime-selector-method ?d
  "*sldb* buffer for the current connection."
  (or (sldb-get-default-buffer)
      (error "No debugger buffer")))

(def-dime-selector-method ?e
  "most recently visited emacs-dylan-mode buffer."
  (dime-recently-visited-buffer 'emacs-dylan-mode))

(def-dime-selector-method ?c
  "DIME connections buffer."
  (dime-list-connections)
  dime-connections-buffer-name)

(def-dime-selector-method ?n
  "Cycle to the next Dylan connection."
  (dime-cycle-connections)
  (concat "*dime-repl "
          (dime-connection-name (dime-current-connection))
          "*"))

(def-dime-selector-method ?t
  "DIME threads buffer."
  (dime-list-threads)
  dime-threads-buffer-name)

(defun dime-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))


;;;; Contrib modules

(defvar dime-required-modules '())

(defun dime-require (module)
  (pushnew module dime-required-modules)
  (when (dime-connected-p)
    (dime-load-contribs)))

(defun dime-load-contribs ()
  (let ((needed (cl-remove-if
                 (lambda (s)
                   (member (cl-subseq (symbol-name s) 1)
                           (mapcar #'downcase (dime-dylan-modules))))
                 dime-required-modules)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Dylan.
      (setf (dime-dylan-modules)
            (dime-eval `(swank:swank-require ',needed))))))

(defstruct dime-contrib
  name
  dime-dependencies
  swank-dependencies
  enable
  disable
  authors
  license)

(defmacro define-dime-contrib (name docstring &rest clauses)
  (declare (indent 1) (doc-string 2))
  (destructuring-bind (&key dime-dependencies
                            swank-dependencies
                            on-load
                            on-unload
                            authors
                            license)
      (loop for (key . value) in clauses append `(,key ,value))
    (let ((enable (intern (concat (symbol-name name) "-init")))
          (disable (intern (concat (symbol-name name) "-unload"))))
    `(progn
       ,@(mapcar (lambda (d) `(require ',d)) dime-dependencies)
       (defun ,enable ()
         ,@(mapcar (lambda (d) `(dime-require ',d)) swank-dependencies)
         ,@on-load)
       (defun ,disable ()
         ,@on-unload)
       (put 'dime-contribs ',name
            (make-dime-contrib
             :name ',name :authors ',authors :license ',license
             :dime-dependencies ',dime-dependencies
             :swank-dependencies ',swank-dependencies
             :enable ',enable :disable ',disable))))))

(defun dime-all-contribs ()
  (loop for (name val) on (symbol-plist 'dime-contribs) by #'cddr
        when (dime-contrib-p val)
        collect val))

(defun dime-find-contrib (name)
  (get 'dime-contribs name))

(defun dime-read-contrib-name ()
  (let ((names (loop for c in (dime-all-contribs) collect
                     (symbol-name (dime-contrib-name c)))))
    (intern (completing-read "Contrib: " names nil t))))

(defun dime-enable-contrib (name)
  (interactive (list (dime-read-contrib-name)))
  (let ((c (or (dime-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (dime-contrib-enable c))))

(defun dime-disable-contrib (name)
  (interactive (list (dime-read-contrib-name)))
  (let ((c (or (dime-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (dime-contrib-disable c))))


;;;;; Pull-down menu

(defvar dime-easy-menu
  (let ((C '(dime-connected-p)))
    `("DIME"
      [ "Edit Definition..."       dime-edit-definition ,C ]
      [ "Return From Definition"   dime-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          dime-complete-symbol ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              dime-eval-defun ,C ]
       [ "Eval Last Expression"    dime-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   dime-pprint-eval-last-expression ,C ]
       [ "Eval Region"             dime-eval-region ,C ]
       [ "Interactive Eval..."     dime-interactive-eval ,C ]
       [ "Edit Dylan Value..."      dime-edit-value ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     dime-macroexpand-1 ,C ]
       [ "Macroexpand All..."      dime-macroexpand-all ,C ]
       [ "Create Trace Buffer"     dime-redirect-trace-output ,C ]
       [ "Toggle Trace..."         dime-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             dime-untrace-all ,C]
       [ "Disassemble..."          dime-disassemble-symbol ,C ]
       [ "Inspect..."              dime-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           dime-compile-defun ,C ]
       [ "Compile/Load File"       dime-compile-and-load-file ,C ]
       [ "Compile File"            dime-compile-file ,C ]
       [ "Compile Region"          dime-compile-region ,C ]
       "--"
       [ "Next Note"               dime-next-note t ]
       [ "Previous Note"           dime-previous-note t ]
       [ "Remove Notes"            dime-remove-notes t ]
       [ "List Notes"              dime-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            dime-who-calls ,C ]
       [ "Who References... "      dime-who-references ,C ]
       [ "Who Sets..."             dime-who-sets ,C ]
       [ "Who Binds..."            dime-who-binds ,C ]
       [ "Who Macroexpands..."     dime-who-macroexpands ,C ]
       [ "Who Specializes..."      dime-who-specializes ,C ]
       [ "List Callers..."         dime-list-callers ,C ]
       [ "List Callees..."         dime-list-callees ,C ]
       [ "Next Location"           dime-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Select Buffer"           dime-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     dime-toggle-profile-fdefinition ,C ]
       [ "Profile Project"         dime-profile-project ,C]
       [ "Profile by Substring"    dime-profile-by-substring ,C ]
       [ "Unprofile All"           dime-unprofile-all ,C ]
       [ "Show Profiled"           dime-profiled-functions ,C ]
       "--"
       [ "Report"                  dime-profile-report ,C ]
       [ "Reset Counters"          dime-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      dime-describe-symbol ,C ]
       [ "Apropos..."              dime-apropos ,C ]
       [ "Apropos all..."          dime-apropos-all ,C ]
       [ "Apropos Project..."      dime-apropos-project ,C ]
       [ "Hyperspec..."            dime-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        dime-interrupt ,C ]
      [ "Abort Async. Command"     dime-quit ,C ]
      [ "Sync Project & Directory" dime-sync-project-and-default-directory ,C]
      )))

(defvar dime-sldb-easy-menu
  (let ((C '(dime-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." dime-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-dime dime-mode-map "DIME" dime-easy-menu)

(defun dime-add-easy-menu ()
  (easy-menu-add dime-easy-menu 'dime-mode-map))

(add-hook 'dime-mode-hook 'dime-add-easy-menu)

(defun dime-sldb-add-easy-menu ()
  (easy-menu-define menubar-dime-sldb
    sldb-mode-map "SLDB" dime-sldb-easy-menu)
  (easy-menu-add dime-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'dime-sldb-add-easy-menu)


;;;; Cheat Sheet

(defvar dime-cheat-sheet-table
  '((:title "Editing dylan code"
     :map dime-mode-map
     :bindings ((dime-eval-defun "Evaluate current top level form")
                (dime-compile-defun "Compile current top level form")
                (dime-interactive-eval "Prompt for form and eval it")
                (dime-compile-and-load-file "Compile and load current file")
                (dime-sync-project-and-default-directory "Synch default project and directory with current buffer")
                (dime-next-note "Next compiler note")
                (dime-previous-note "Previous compiler note")
                (dime-remove-notes "Remove notes")))
    (:title "Completion"
     :map dime-mode-map
     :bindings (dime-indent-and-complete-symbol
                dime-fuzzy-complete-symbol))
    (:title "Within SLDB buffers"
     :map sldb-mode-map
     :bindings ((sldb-default-action "Do 'whatever' with thing at point")
                (sldb-toggle-details "Toggle frame details visualization")
                (sldb-quit "Quit to REPL")
                (sldb-abort "Invoke ABORT restart")
                (sldb-continue "Invoke CONTINUE restart (if available)")
                (sldb-show-source "Jump to frame's source code")
                (sldb-eval-in-frame "Evaluate in frame at point")
                (sldb-inspect-in-frame "Evaluate in frame at point and inspect result")))
    (:title "Within the Inspector"
     :map dime-inspector-mode-map
     :bindings ((dime-inspector-next-inspectable-object "Jump to next inspectable object")
                (dime-inspector-operate-on-point "Inspect object or execute action at point")
                (dime-inspector-reinspect "Reinspect current object")
                (dime-inspector-pop "Return to previous object")
                ;;(dime-inspector-copy-down "Send object at point to REPL")
                (dime-inspector-toggle-verbose "Toggle verbose mode")
                (dime-inspector-quit "Quit")))
    (:title "Finding Definitions"
     :map dime-mode-map
     :bindings (dime-edit-definition
                dime-pop-find-definition-stack))))

(defun dime-cheat-sheet ()
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create (dime-buffer-name :cheat-sheet)))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (insert "DIME: The Superior Dylan Interaction Mode for Emacs (minor-mode).\n\n")
  (dolist (mode dime-cheat-sheet-table)
    (let ((title (cl-getf mode :title))
          (mode-map (cl-getf mode :map))
          (mode-keys (cl-getf mode :bindings)))
      (insert title)
      (insert ":\n")
      (insert (make-string (1+ (length title)) ?-))
      (insert "\n")
      (let ((keys '())
            (descriptions '()))
        (dolist (func mode-keys)
          ;; func is eithor the function name or a list (NAME DESCRIPTION)
          (push (if (symbolp func)
                    (prin1-to-string func)
                    (second func))
                descriptions)
          (let ((all-bindings (where-is-internal (if (symbolp func)
                                                     func
                                                     (first func))
                                                 (symbol-value mode-map)))
                (key-bindings '()))
            (dolist (binding all-bindings)
              (when (and (vectorp binding)
                         (integerp (aref binding 0)))
                (push binding key-bindings)))
            (push (mapconcat 'key-description key-bindings " or ") keys)))
        (loop
           with key-length = (apply 'max (mapcar 'length keys))
           with desc-length = (apply 'max (mapcar 'length descriptions))
           for key in (nreverse keys)
           for desc in (nreverse descriptions)
           do (insert desc)
           do (insert (make-string (- desc-length (length desc)) ? ))
           do (insert " => ")
           do (insert (if (string= "" key)
                          "<not on any key>"
                          key))
           do (insert "\n")
           finally do (insert "\n")))))
  (setq buffer-read-only t)
  (goto-char (point-min)))

;;;; Utilities (no not Paul Graham style)

;;;; List frobbing

;; FIXME: Seems uncommon and less readable than loop.
(defun dime-map-alist (car-fn cdr-fn alist)
  "Map over ALIST, calling CAR-FN on the car, and CDR-FN on the
cdr of each entry."
  (mapcar (lambda (entry)
            (cons (funcall car-fn (car entry))
                  (funcall cdr-fn (cdr entry))))
          alist))

;;; FIXME: this looks almost dime `dime-alistify', perhaps the two
;;;        functions can be merged.
(defun dime-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (reverse (mapcar #'reverse accumulator)))))

(defun dime-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (cl-assoc k alist :test test)))
	(if probe
	    (push e (cdr probe))
            (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (loop for (key . value) in (reverse alist)
          collect (cons key (reverse value)))))

;;;;; Misc.

(defun dime-length= (seq n)
  "Return (= (length SEQ) N)."
  (etypecase seq
    (list
     (cond ((zerop n) (null seq))
           ((let ((tail (nthcdr (1- n) seq)))
              (and tail (null (cdr tail)))))))
    (sequence
     (= (length seq) n))))

(defun dime-length> (seq n)
  "Return (> (length SEQ) N)."
  (etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun dime-trim-whitespace (str)
  (save-match-data
    (string-match "^\\s-*\\(.*?\\)\\s-*$" str)
    (match-string 1 str)))

;;;;; Buffer related

(defun dime-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun dime-column-max ()
  (save-excursion
    (goto-char (point-min))
    (loop for column = (prog2 (end-of-line) (current-column) (forward-line))
          until (= (point) (point-max))
          maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun dime-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
              symbol-part))
      n)))

(defun dime-cl-symbol-project (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun dime-qualify-cl-symbol-name (symbol-or-name)
  "Return a project-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a project prefix the
current project is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (dime-cl-symbol-project s)
        s
      (format "%s::%s"
              (let* ((project dylan-buffer-module))
                (if project project "common-dylan"))
              (dime-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro dime-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (declare (indent 0))
  (let ((pointvar (cl-gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(defun dime-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (i (or count 1))
    (dime-forward-cruft)
    (forward-sexp)))

(defconst dime-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))

(defun dime-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at dime-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (dime-eval-feature-expression
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax
                       (signal 'dime-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (dime-forward-sexp)))))

(defun dime-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (dime-point-moves-p (skip-chars-forward " \t\n")
                              (forward-comment (buffer-size))
                              (inline (dime-forward-reader-conditional)))))

(defun dime-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(put 'dime-incorrect-feature-expression
     'error-conditions '(dime-incorrect-feature-expression error))

(put 'dime-unknown-feature-expression
     'error-conditions '(dime-unknown-feature-expression
                         dime-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the length=1 constraint is bogus
(defun dime-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (dime-keywordify e) (dime-dylan-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (dime-keywordify (car e))))
                    (case head
                      (:and #'every)
                      (:or #'some)
                      (:not
                         (lexical-let ((feature-expression e))
                           (lambda (f l)
                             (cond
                               ((dime-length= l 0) t)
                               ((dime-length= l 1) (not (apply f l)))
                               (t (signal 'dime-incorrect-feature-expression
                                          feature-expression))))))
                      (t (signal 'dime-unknown-feature-expression head))))
                  #'dime-eval-feature-expression
                  (cdr e)))
        (t (signal 'dime-incorrect-feature-expression e))))

;;;;; Extracting Dylan forms from the buffer or user

(defun dime-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (dime-region-for-defun-at-point)))

(defun dime-region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun dime-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun dime-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(defun dime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (dime-beginning-of-symbol) (point)))

(defun dime-symbol-end-pos ()
  (save-excursion (dime-end-of-symbol) (point)))

(put 'dime-symbol 'end-op 'dime-end-of-symbol)
(put 'dime-symbol 'beginning-op 'dime-beginning-of-symbol)
(put 'dime-symbol 'thing-at-point
     (lambda ()
       (let ((bounds (bounds-of-thing-at-point 'dime-symbol)))
         (when bounds
           (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun dime-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (or (thing-at-point 'dime-symbol)
      (let ((string (thing-at-point 'sexp)))
        (if string (substring-no-properties string) nil))))

(defun dime-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (dime-sexp-at-point) (error "No expression at point.")))

(defun dime-string-at-point ()
  "Returns the string at point as a string, otherwise nil."
  (let ((sexp (dime-sexp-at-point)))
    (if (eql (char-syntax (aref sexp 0)) ?\")
        sexp
        nil)))

(defun dime-string-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (dime-string-at-point) (error "No string at point.")))

(defun dime-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))


(provide 'dime)
(run-hooks 'dime-load-hook)

;;; dime.el ends here
