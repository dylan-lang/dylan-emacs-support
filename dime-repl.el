;;; dime-repl.el ---
;;
;; Original Author: Helmut Eller
;; Contributors: to many to mention
;; License: GNU GPL (same license as Emacs)
;;
;;; Description:
;;

;;
;;; Installation:
;;
;; Call dime-setup and include 'dime-repl as argument:
;;
;;  (dime-setup '(dime-repl [others conribs ...]))
;;

(require 'dime)

(defvar-local dime-repl-project-stack nil
  "The stack of projects visited in this repl.")

(defvar-local dime-repl-directory-stack nil
  "The stack of default directories associated with this repl.")

(defvar-local dime-repl-prompt-start-mark nil)

(defvar-local dime-repl-input-start-mark nil)

(defvar-local dime-repl-old-input-counter 0
  "Counter used to generate unique `dime-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together.")

(define-dime-contrib dime-repl
  "Read-Eval-Print Loop written in Emacs Lisp.

This contrib implements a Dylan Listener along with some niceties like
a persistent history and various \"shortcut\" commands.  Nothing here
depends on comint.el; I/O is multiplexed over DIME's socket.

This used to be the default REPL for DIME, but it was hard to
maintain."
  (:authors "too many to mention")
  (:license "GPL")
  (:on-load
   (add-hook 'dime-event-hooks 'dime-repl-event-hook-function)
   (add-hook 'dime-connected-hook 'dime-repl-connected-hook-function)
   (setq dime-find-buffer-project-function 'dime-repl-find-buffer-project))
  (:on-unload (dime-repl-remove-hooks)))

;;;;; dime-repl

(defgroup dime-repl nil
  "The Read-Eval-Print Loop (*dime-repl* buffer)."
  :prefix "dime-repl-"
  :group 'dime)

(defcustom dime-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from dylan forms."
  :type '(character)
  :group 'dime-repl)

(defcustom dime-repl-only-save-dylan-buffers t
  "When T we only attempt to save dylan-mode file buffers. When
  NIL dime will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'dime-repl)

(defface dime-repl-prompt-face
  (if (dime-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the DIME REPL."
  :group 'dime-repl)

(defface dime-repl-output-face
  (if (dime-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Dylan output in the DIME REPL."
  :group 'dime-repl)

(defface dime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the DIME REPL."
  :group 'dime-repl)

(defface dime-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the DIME REPL."
  :group 'dime-repl)

(defcustom dime-repl-history-file "~/.dime-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'dime-repl)

(defcustom dime-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'dime-repl)

(defcustom dime-repl-history-file-coding-system
  (cond ((dime-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t dime-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'dime-repl)


;; dummy defvar for compiler
(defvar dime-repl-read-mode)

(defun dime-reading-p ()
  "True if Dylan is currently reading input from the REPL."
  (with-current-buffer (dime-output-buffer)
    dime-repl-read-mode))


;;;; Stream output

(dime-def-connection-var dime-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(defvar-local dime-output-start nil
  "Marker for the start of the output for the evaluation.")

(defvar-local dime-output-end nil
  "Marker for end of output. New output is inserted at this mark.")

(defun dime-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (dime-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (dime-connection-output-buffer)
              (let ((connection (dime-connection)))
                (with-current-buffer (dime-repl-buffer t connection)
                  (unless (eq major-mode 'dime-repl-mode)
                    (dime-repl-mode))
                  (setq dime-buffer-connection connection)
                  (dime-reset-repl-markers)
                  (unless noprompt
                    (dime-repl-insert-prompt))
                  (current-buffer)))))))

(defvar dime-repl-banner-function 'dime-repl-insert-banner)

(defun dime-repl-update-banner ()
  (funcall dime-repl-banner-function)
  (dime-move-point (point-max))
  (dime-mark-output-start)
  (dime-mark-input-start)
  (dime-repl-insert-prompt))

(defun dime-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; DIME " (or (dime-changelog-date)
                                          "- ChangeLog file not found"))))
      (insert welcome))))

(defun dime-init-output-buffer (connection)
  (with-current-buffer (dime-output-buffer t)
    (setq dime-buffer-connection connection
          dime-repl-directory-stack '()
          dime-repl-project-stack '())
    (dime-repl-update-banner)))

(defun dime-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (dime-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (dime-repl-show-maximum-output)))

(defun dime-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (when (and (plusp (length string))
               (eq (process-status dime-buffer-connection) 'open))
      (dime-write-string string))))

(defvar dime-open-stream-hooks)

(defun dime-open-stream-to-dylan (port)
  (let ((stream (open-network-stream "*dylan-output-stream*"
                                     (dime-with-connection-buffer ()
                                       (current-buffer))
				     dime-dylan-host port)))
    (dime-set-query-on-exit-flag stream)
    (set-process-filter stream 'dime-output-filter)
    (let ((pcs (process-coding-system (dime-current-connection))))
      (set-process-coding-system stream (car pcs) (cdr pcs)))
    (when-let (secret (dime-secret))
      (dime-net-send secret stream))
    (run-hook-with-args 'dime-open-stream-hooks stream)
    stream))

(defvar dime-write-string-function 'dime-repl-write-string)

(defun dime-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the
hashtable `dime-output-target-to-marker'; output is inserted at this marker."
  (funcall dime-write-string-function string target))

(defun dime-repl-write-string (string &optional target)
  (case target
    ((nil) (dime-repl-emit string))
    (:repl-result (dime-repl-emit-result string))
    (t (dime-emit-to-target string target))))

(defvar dime-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro dime-save-marker (marker &rest body)
  (declare (indent 1))
  (let ((pos (cl-gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(defun dime-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (dime-output-buffer)
    (save-excursion
      (goto-char dime-output-end)
      (dime-save-marker dime-output-start
        (dime-propertize-region '(face dime-repl-output-face
                                        rear-nonsticky (face))
          (insert-before-markers string)
          (when (and (= (point) dime-repl-prompt-start-mark)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker dime-output-end (1- (point)))))))
    (when dime-repl-popup-on-output
      (setq dime-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (dime-repl-show-maximum-output)))

(defun dime-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (dime-output-buffer)
    (save-excursion
      (dime-save-marker dime-output-start
        (dime-save-marker dime-output-end
          (goto-char dime-repl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (dime-propertize-region `(face dime-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (dime-repl-show-maximum-output)))

(defvar dime-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defvar dime-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun dime-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (case target
    ((nil)
     (with-current-buffer (dime-output-buffer)
       dime-output-end))
    (:repl-result
     (with-current-buffer (dime-output-buffer)
       dime-repl-input-start-mark))
    (t
     (gethash target dime-output-target-to-marker))))

(defun dime-emit-to-target (string target)
  "Insert STRING at target TARGET.
See `dime-output-target-to-marker'."
  (let* ((marker (dime-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun dime-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (pop-to-buffer (dime-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;;
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Dylan and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to dime-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Dylan, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `dime-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt).
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

(dime-def-connection-var dime-dylan-project-prompt-string
    "opendylan"
  "The current project name of the Superior dylan.
This is automatically synchronized from Dylan.")

(defun dime-reset-repl-markers ()
  (dolist (markname '(dime-output-start
                      dime-output-end
                      dime-repl-prompt-start-mark
                      dime-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar dime-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    map))

(dime-define-keys dime-prefix-map
  ("\C-z" 'dime-switch-to-output-buffer)
  ("\M-p" 'dime-repl-set-project))

(dime-define-keys dime-mode-map
  ("\C-c~" 'dime-sync-project-and-default-directory))

(dime-define-keys dime-connection-list-mode-map
  ((kbd "RET") 'dime-goto-connection)
  ([return] 'dime-goto-connection))

(dime-define-keys dime-repl-mode-map
  ("\C-m" 'dime-repl-return)
  ([return] 'dime-repl-return)
  ("\C-j" 'dime-repl-newline-and-indent)
  ("\C-\M-m" 'dime-repl-closing-return)
  ([(control return)] 'dime-repl-closing-return)
  ("\C-a" 'dime-repl-bol)
  ([home] 'dime-repl-bol)
  ("\M-p" 'dime-repl-previous-input)
  ((kbd "C-<up>") 'dime-repl-backward-input)
  ("\M-n" 'dime-repl-next-input)
  ((kbd "C-<down>") 'dime-repl-forward-input)
  ("\M-r" 'dime-repl-previous-matching-input)
  ("\M-s" 'dime-repl-next-matching-input)
  ("\C-c\C-c" 'dime-interrupt)
  ;("\t"   'dime-complete-symbol)
  ("\t"   'dime-indent-and-complete-symbol)
  ("\M-\t" 'dime-complete-symbol)
  (" "    'dime-space)
  ("\C-c\C-o" 'dime-repl-clear-output)
  ("\C-c\M-o" 'dime-repl-clear-buffer)
  ("\C-c\C-u" 'dime-repl-kill-input)
  ("\C-c\C-n" 'dime-repl-next-prompt)
  ("\C-c\C-p" 'dime-repl-previous-prompt)
  ("\C-c\C-z" 'dime-nop))

(dime-define-keys dime-inspector-mode-map
  ((kbd "M-RET") 'dime-inspector-copy-down-to-repl))

(dime-define-keys sldb-mode-map
  ("\C-y" 'sldb-insert-frame-call-to-repl))

(def-dime-selector-method ?r
  "DIME Read-Eval-Print-Loop."
  (dime-output-buffer))

(define-minor-mode dime-repl-map-mode
  "Minor mode which makes dime-repl-mode-map available.
\\{dime-repl-mode-map}"
  nil
  nil
  dime-repl-mode-map)

(defun dime-repl-mode ()
  "Major mode for interacting with a superior Dylan.
\\{dime-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dime-repl-mode)
  (dime-editing-mode 1)
  (dime-repl-map-mode 1)
  (lisp-mode-variables t)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq dime-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when dime-repl-history-file
    (dime-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'dime-repl-safe-save-merged-history nil t))
  (add-hook 'kill-emacs-hook 'dime-repl-save-all-histories)
  (dime-setup-command-hooks)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with DIME-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function)
       'dime-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'dime-repl-mode-end-of-defun)
  (run-mode-hooks 'dime-repl-mode-hook))

(defun dime-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*dime-repl %s*" (dime-connection-name connection))))

(defun dime-repl ()
  (interactive)
  (dime-switch-to-output-buffer))

(defun dime-repl-mode-beginning-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (dime-repl-mode-end-of-defun (- arg))
      (dotimes (i (or arg 1))
        (dime-repl-previous-prompt))))

(defun dime-repl-mode-end-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (dime-repl-mode-beginning-of-defun (- arg))
      (dotimes (i (or arg 1))
        (dime-repl-next-prompt))))

(defun dime-repl-send-string (string &optional command-string)
  (cond (dime-repl-read-mode
         (dime-repl-return-string string))
        (t (dime-repl-eval-string string))))

(defun dime-repl-eval-string (string)
  (dime-rex ()
      ((list 'swank:listener-eval string) (dime-current-project))
    ((:ok result)
     (dime-repl-insert-result result))
    ((:abort condition)
     (dime-repl-show-abort condition))))

(defun dime-repl-insert-result (result)
  (with-current-buffer (dime-output-buffer)
    (save-excursion
      (when result
        (destructure-case result
          ((:values &rest strings)
           (cond ((null strings)
                  (dime-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (dime-repl-emit-result s t)))))))
      (dime-repl-insert-prompt))
    (dime-repl-show-maximum-output)))

(defun dime-repl-show-abort (condition)
  (with-current-buffer (dime-output-buffer)
    (save-excursion
      (dime-save-marker dime-output-start
        (dime-save-marker dime-output-end
          (goto-char dime-output-end)
          (insert-before-markers (format "; Evaluation aborted on %s.\n" condition))
          (dime-repl-insert-prompt))))
    (dime-repl-show-maximum-output)))

(defun dime-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.
Return the position of the prompt beginning."
  (goto-char dime-repl-input-start-mark)
  (dime-save-marker dime-output-start
    (dime-save-marker dime-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " (dime-dylan-project-prompt-string))))
        (dime-propertize-region
            '(face dime-repl-prompt-face read-only t intangible t
                   dime-repl-prompt t
                   rear-nonsticky (dime-repl-prompt read-only face intangible))
          (insert-before-markers prompt))
        (set-marker dime-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun dime-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                   (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defvar dime-repl-current-input-hooks)

(defun dime-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'dime-repl-current-input-hooks
                                        until-point-p)
      (buffer-substring-no-properties dime-repl-input-start-mark
                                      (if until-point-p
                                          (point)
                                        (point-max)))))

(defun dime-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))

(defun dime-mark-input-start ()
  (set-marker dime-repl-input-start-mark (point) (current-buffer)))

(defun dime-mark-output-start ()
  (set-marker dime-output-start (point))
  (set-marker dime-output-end (point)))

(defun dime-mark-output-end ()
  ;; Don't put dime-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties dime-output-start dime-output-end
                       '(;;face dime-repl-output-face
                         rear-nonsticky (face))))

(defun dime-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (cond ((and (>= (point) dime-repl-input-start-mark)
              (dime-same-line-p (point) dime-repl-input-start-mark))
         (goto-char dime-repl-input-start-mark))
        (t (beginning-of-line 1))))

(defun dime-repl-in-input-area-p ()
   (<= dime-repl-input-start-mark (point)))

(defun dime-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) dime-repl-input-start-mark))

(defun dime-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger DIME-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (dime-repl-at-prompt-start-p))
           (dime-repl-in-input-area-p))
      (goto-char dime-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun dime-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. DIME-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max)))
           (dime-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun dime-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (dime-repl-find-prompt t))

(defun dime-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (dime-repl-find-prompt))

(defun dime-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'dime-repl-prompt))
    (while (progn
             (dime-search-property-change prop backward)
             (not (or (dime-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (dime-end-of-proprange-p prop)
      (goto-char origin))))

(defun dime-search-property-change (prop &optional backward)
  (cond (backward
         (goto-char (or (previous-single-char-property-change (point) prop)
			(point-min))))
        (t
         (goto-char (or (next-single-char-property-change (point) prop)
			(point-max))))))

(defun dime-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar dime-repl-return-hooks)

(defun dime-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input only if a whole expression has been entered,
i.e. the parenthesis are matched.

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (dime-check-connected)
  (cond (end-of-input
         (dime-repl-send-input))
        (dime-repl-read-mode ; bad style?
         (dime-repl-send-input t))
        ((and (get-text-property (point) 'dime-repl-old-input)
              (< (point) dime-repl-input-start-mark))
         (dime-repl-grab-old-input end-of-input)
         (dime-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'dime-repl-return-hooks))
        ((dime-input-complete-p dime-repl-input-start-mark (point-max))
         (dime-repl-send-input t))
        (t
         (dime-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun dime-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun dime-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (dime-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (dime-repl-add-to-input-history
     (buffer-substring dime-repl-input-start-mark end))
    (when newline
      (insert "\n")
      (dime-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties dime-repl-input-start-mark
                           (point)
                           `(dime-repl-old-input
                             ,(incf dime-repl-old-input-counter))))
    (let ((overlay (make-overlay dime-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'dime-repl-input-face)))
  (let ((input (dime-repl-current-input)))
    (goto-char (point-max))
    (dime-mark-input-start)
    (dime-mark-output-start)
    (dime-repl-send-string input)))

(defun dime-repl-grab-old-input (replace)
  "Resend the old REPL input at point.
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `dime-repl-old-input'."
  (multiple-value-bind (beg end) (dime-property-bounds 'dime-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char dime-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun dime-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region dime-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (dime-repl-return))

(defun dime-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region dime-repl-prompt-start-mark (point-max))
    (insert "\n")
    (dylan-indent-line)))

(defun dime-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region dime-repl-input-start-mark (point-max)))

(defun dime-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position dime-repl-input-start-mark) (point))
         (kill-region dime-repl-input-start-mark (point)))
        ((= (point) (marker-position dime-repl-input-start-mark))
         (dime-repl-delete-current-input))))

(defun dime-repl-replace-input (string)
  (dime-repl-delete-current-input)
  (insert-and-inherit string))

(defun dime-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char dime-repl-input-start-mark)
    (line-beginning-position)))

(defvar dime-repl-clear-buffer-hook)

(defun dime-repl-clear-buffer ()
  "Delete the output generated by the Dylan process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) dime-repl-prompt-start-mark)
    (delete-region dime-output-start dime-output-end)
    (when (< (point) dime-repl-input-start-mark)
      (goto-char dime-repl-input-start-mark))
    (recenter t))
  (run-hooks 'dime-repl-clear-buffer-hook))

(defun dime-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion
                 (dime-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (dime-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun dime-repl-set-project (project)
  "Set the project of the REPL buffer to PROJECT."
  (interactive (list (let* ((p (dime-current-project)))
                       (dime-read-project-name "Project: " p))))
  (with-current-buffer (dime-output-buffer)
    (let ((previouse-point (- (point) dime-repl-input-start-mark)))
      (destructuring-bind (name prompt-string)
          (dime-repl-shortcut-eval `(swank:set-package ,project))
        (setf (dime-dylan-project-prompt-string) prompt-string)
        (setf dime-buffer-project name)
        (dime-repl-insert-prompt)
        (when (plusp previouse-point)
          (goto-char (+ previouse-point dime-repl-input-start-mark)))))))


;;;;; History

(defcustom dime-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'dime-repl)

(defcustom dime-repl-history-remove-duplicates nil
  "*When T all duplicates are removed except the last one."
  :type 'boolean
  :group 'dime-repl)

(defcustom dime-repl-history-trim-whitespaces nil
  "*When T strip all whitespaces from the beginning and end."
  :type 'boolean
  :group 'dime-repl)

(defvar-local dime-repl-input-history '()
  "History list of strings read from the REPL buffer.")

(defun dime-string-trim (character-bag string)
  (cl-flet ((find-bound (&optional from-end)
           (cl-position-if-not (lambda (char) (memq char character-bag))
                            string :from-end from-end)))
    (let ((start (find-bound))
          (end (find-bound t)))
      (if start
          (cl-subseq string start (1+ end))
          ""))))

(defun dime-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (when dime-repl-history-trim-whitespaces
    (setq string (dime-string-trim '(?\n ?\ ?\t) string)))
  (unless (equal string "")
    (when dime-repl-history-remove-duplicates
      (setq dime-repl-input-history
            (remove string dime-repl-input-history)))
    (unless (equal string (car dime-repl-input-history))
      (push string dime-repl-input-history))))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'dime-repl-history-replace,
;; otherwise we reinitialize them.

(defvar dime-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar dime-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun dime-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq dime-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length dime-repl-input-history))
         (pos0 (cond ((dime-repl-history-search-in-progress-p)
                      dime-repl-input-history-position)
                     (t min-pos)))
         (pos (dime-repl-position-in-history pos0 direction (or regexp "")
                                              (dime-repl-current-input)))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (dime-repl-replace-input (nth pos dime-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not dime-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (dime-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq dime-repl-input-history-position pos)
    (setq this-command 'dime-repl-history-replace)))

(defun dime-repl-history-search-in-progress-p ()
  (eq last-command 'dime-repl-history-replace))

(defun dime-repl-terminate-history-search ()
  (setq last-command this-command))

(defun dime-repl-position-in-history (start-pos direction regexp
                                       &optional exclude-string)
  "Return the position of the history item matching REGEXP.
Return -1 resp. the length of the history if no item matches.
If EXCLUDE-STRING is specified then it's excluded from the search."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history dime-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          for history-item = (nth pos history)
          if (and (string-match regexp history-item)
                  (not (equal history-item exclude-string)))
          return pos)))

(defun dime-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (dime-repl-history-replace 'backward (dime-repl-history-pattern t)))

(defun dime-repl-next-input ()
  "Cycle forwards through input history.
See `dime-repl-previous-input'."
  (interactive)
  (dime-repl-history-replace 'forward (dime-repl-history-pattern t)))

(defun dime-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (dime-repl-history-replace 'forward (dime-repl-history-pattern)))

(defun dime-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (dime-repl-history-replace 'backward (dime-repl-history-pattern)))

(defun dime-repl-previous-matching-input (regexp)
  (interactive (list (dime-read-from-minibuffer
		      "Previous element matching (regexp): ")))
  (dime-repl-terminate-history-search)
  (dime-repl-history-replace 'backward regexp))

(defun dime-repl-next-matching-input (regexp)
  (interactive (list (dime-read-from-minibuffer
		      "Next element matching (regexp): ")))
  (dime-repl-terminate-history-search)
  (dime-repl-history-replace 'forward regexp))

(defun dime-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((dime-repl-history-search-in-progress-p)
         dime-repl-history-pattern)
        (use-current-input
         (assert (<= dime-repl-input-start-mark (point)))
         (let ((str (dime-repl-current-input t)))
           (cond ((string-match "^[ \t\n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun dime-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history.

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (dime-repl-current-input)))
  (let ((merged-history
         (dime-repl-merge-histories dime-repl-input-history
                                     (dime-repl-read-history nil t))))
    (setq dime-repl-input-history
          (cl-delete string merged-history :test #'string=))
    (dime-repl-save-history))
  (dime-repl-delete-current-input))

;;;;; Persistent History

(defun dime-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (cl-remove-if test new-hist)
            (cl-remove-if test old-hist))))

(defun dime-repl-load-history (&optional filename)
  "Set the current DIME REPL history.
It can be read either from FILENAME or `dime-repl-history-file' or
from a user defined filename."
  (interactive (list (dime-repl-read-history-filename)))
  (let ((file (or filename dime-repl-history-file)))
    (setq dime-repl-input-history (dime-repl-read-history file t))))

(defun dime-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.
The default value for FILENAME is `dime-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename dime-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun dime-repl-read-history-filename ()
  (read-file-name "Use DIME REPL history from file: "
                  dime-repl-history-file))

(defun dime-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`dime-repl-merge-histories'."
  (interactive (list (dime-repl-read-history-filename)))
  (let ((file (or filename dime-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (dime-repl-merge-histories (dime-repl-read-history file t)
                                              dime-repl-input-history)))
        (dime-repl-save-history file hist)))))

(defun dime-repl-save-history (&optional filename history)
  "Simply save the current DIME REPL history to a file.
When DIME is setup to always load the old history and one uses only
one instance of dime all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `dime-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (dime-repl-read-history-filename)))
  (let ((file (or filename dime-repl-history-file))
        (hist (or history dime-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (cl-subseq hist 0 (min (length hist) dime-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs dime-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for DIME REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun dime-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'dime-repl-mode)
        (dime-repl-safe-save-merged-history)))))

(defun dime-repl-safe-save-merged-history ()
  (dime-repl-call-with-handler
   #'dime-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun dime-repl-safe-load-history ()
  (dime-repl-call-with-handler
   #'dime-repl-load-history
   "%S while loading the history. Continue? "))

(defun dime-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(define-key dime-repl-mode-map
  (string dime-repl-shortcut-dispatch-char) 'dime-handle-repl-shortcut)

(define-minor-mode dime-repl-read-mode
  "Mode the read input from Emacs
\\{dime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . dime-repl-return)
    ([return] . dime-repl-return)
    ("\C-c\C-b" . dime-repl-read-break)
    ("\C-c\C-c" . dime-repl-read-break)))

(defvar-local dime-read-string-threads nil)

(defvar-local dime-read-string-tags nil)

(defun dime-repl-read-string (thread tag)
  (dime-switch-to-output-buffer)
  (push thread dime-read-string-threads)
  (push tag dime-read-string-tags)
  (goto-char (point-max))
  (dime-mark-output-end)
  (dime-mark-input-start)
  (dime-repl-read-mode 1))

(defun dime-repl-return-string (string)
  (dime-dispatch-event `(:emacs-return-string
                          ,(pop dime-read-string-threads)
                          ,(pop dime-read-string-tags)
                          ,string))
  (dime-repl-read-mode -1))

(defun dime-repl-read-break ()
  (interactive)
  (dime-dispatch-event `(:emacs-interrupt ,(car dime-read-string-threads))))

(defun dime-repl-abort-read (thread tag)
  (with-current-buffer (dime-output-buffer)
    (pop dime-read-string-threads)
    (pop dime-read-string-tags)
    (dime-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(defstruct (dime-repl-shortcut (:conc-name dime-repl-shortcut.))
  symbol names handler one-liner)

(defvar dime-repl-shortcut-table nil
  "A list of dime-repl-shortcuts")

(defvar dime-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar dime-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun dime-handle-repl-shortcut ()
  (interactive)
  (if (> (point) dime-repl-input-start-mark)
      (insert (string dime-repl-shortcut-dispatch-char))
      (let ((shortcut (dime-lookup-shortcut
                       (completing-read "Command: "
                                        (dime-bogus-completion-alist
                                         (dime-list-all-repl-shortcuts))
                                        nil t nil
                                        'dime-repl-shortcut-history))))
        (with-struct (dime-repl-shortcut. handler) shortcut
          (let ((dime-within-repl-shortcut-handler-p t))
            (call-interactively handler))))))

(defun dime-list-all-repl-shortcuts ()
  (loop for shortcut in dime-repl-shortcut-table
        append (dime-repl-shortcut.names shortcut)))

(defun dime-lookup-shortcut (name)
  (cl-find-if (lambda (s) (member name (dime-repl-shortcut.names s)))
           dime-repl-shortcut-table))

(defmacro defdime-repl-shortcut (edylan-name names &rest options)
  "Define a new repl shortcut. EDYLAN-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created.

NAMES is a list of \(full-name . aliases\).

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when edylan-name
        `(defun ,edylan-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-dime-repl-shortcut
                          :symbol ',edylan-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq dime-repl-shortcut-table
             (cl-remove-if (lambda (s)
                          (member ',(car names) (dime-repl-shortcut.names s)))
                        dime-repl-shortcut-table))
       (push new-shortcut dime-repl-shortcut-table)
       ',edylan-name)))

(defun dime-repl-shortcut-eval (sexp &optional project)
  "This function should be used by REPL shortcut handlers instead
of `dime-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when dime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (dime-repl-add-to-input-history (prin1-to-string sexp)))
  (dime-eval sexp project))

(defun dime-repl-shortcut-eval-async (sexp &optional cont project)
  "This function should be used by REPL shortcut handlers instead
of `dime-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when dime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (dime-repl-add-to-input-history (prin1-to-string sexp)))
  (dime-eval-async sexp cont project))

(defun dime-list-repl-short-cuts ()
  (interactive)
  (dime-with-popup-buffer ((dime-buffer-name :repl-help))
    (let ((table (cl-sort (cl-copy-list dime-repl-shortcut-table) #'string<
                        :key (lambda (x)
                               (car (dime-repl-shortcut.names x))))))
      (save-excursion
        (dolist (shortcut table)
          (let ((names (dime-repl-shortcut.names shortcut)))
            (insert (pop names)) ;; first print the "full" name
            (when names
              ;; we also have aliases
              (insert " (aka ")
              (while (cdr names)
                (insert (pop names) ", "))
              (insert (car names) ")"))
            (when (dime-repl-shortcut.one-liner shortcut)
              (insert "\n     " (dime-repl-shortcut.one-liner shortcut)))
            (insert "\n")))))))

(defun dime-save-some-dylan-buffers ()
  (if dime-repl-only-save-dylan-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode dime-dylan-modes)
                                    (not (null buffer-file-name)))))
      (save-some-buffers)))

(defdime-repl-shortcut dime-repl-shortcut-help ("help")
  (:handler 'dime-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defdime-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'dime-set-default-directory)
  (:one-liner "Change the current directory."))

(defdime-repl-shortcut nil ("pwd")
  (:handler (lambda ()
              (interactive)
              (let ((dir (dime-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defdime-repl-shortcut dime-repl-push-directory
    ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (dime-eval '(swank:default-directory))
                      nil nil "")))
              (push (dime-eval '(swank:default-directory))
                    dime-repl-directory-stack)
              (dime-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defdime-repl-shortcut dime-repl-pop-directory
    ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null dime-repl-directory-stack)
                  (message "Directory stack is empty.")
                  (dime-set-default-directory
                   (pop dime-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defdime-repl-shortcut nil ("change-project" "change-package" "!p" "in-project" "in")
  (:handler 'dime-repl-set-project)
  (:one-liner "Change the current project."))

(defdime-repl-shortcut dime-repl-push-project ("push-project" "+p")
  (:handler (lambda (project)
              (interactive (list (dime-read-project-name "Project: ")))
              (push (dime-current-project) dime-repl-project-stack)
              (dime-repl-set-project project)))
  (:one-liner "Save the current project and set it to a new one."))

(defdime-repl-shortcut dime-repl-pop-project ("pop-project" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null dime-repl-project-stack)
                  (message "Project stack is empty.")
                  (dime-repl-set-project
                   (pop dime-repl-project-stack)))))
  (:one-liner "Restore the last saved project."))

(defdime-repl-shortcut dime-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car dime-repl-input-history))
              (insert "\n")
              (dime-repl-send-input)))
  (:one-liner "Resend the last form."))

(defdime-repl-shortcut dime-repl-disconnect ("disconnect")
  (:handler 'dime-disconnect)
  (:one-liner "Disconnect the current connection."))

(defdime-repl-shortcut dime-repl-disconnect-all ("disconnect-all")
  (:handler 'dime-disconnect-all)
  (:one-liner "Disconnect all connections."))

(defdime-repl-shortcut dime-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (dime-connected-p)
                (dime-quit-dylan))
              (dime-kill-all-buffers)))
  (:one-liner "Quit all Dylans and close all DIME buffers."))

(defdime-repl-shortcut dime-repl-quit ("quit")
  (:handler (lambda ()
	      (interactive)
              ;; `dime-quit-dylan' determines the connection to quit
              ;; on behalf of the REPL's `dime-buffer-connection'.
              (let ((repl-buffer (dime-output-buffer)))
                (dime-quit-dylan)
                (kill-buffer repl-buffer))))
  (:one-liner "Quit the current Dylan."))

(defdime-repl-shortcut dime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (dime-read-symbol-name "Name (symbol): " t)
                                 (dime-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value
                      " \"REPL generated global variable.\")")
              (dime-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defdime-repl-shortcut dime-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (dime-save-some-dylan-buffers)
              (dime-repl-shortcut-eval-async
               `(swank:compile-file-if-needed
                 ,(dime-to-dylan-filename filename) t)
               #'dime-compilation-finished)))
  (:one-liner "Compile (if neccessary) and load a dylan file."))

(defdime-repl-shortcut nil  ("restart-inferior-dylan")
  (:handler 'dime-restart-inferior-dylan)
  (:one-liner "Restart *inferior-dylan* and reconnect DIME."))

(defun dime-redirect-inferior-output (&optional noerror)
  "Redirect output of the inferior-process to the REPL buffer."
  (interactive)
  (let ((proc (dime-inferior-process)))
    (cond (proc
           (let ((filter (dime-rcurry #'dime-inferior-output-filter
                                       (dime-current-connection))))
             (set-process-filter proc filter)))
	  (noerror)
	  (t (error "No inferior dylan process")))))

(defun dime-inferior-output-filter (proc string conn)
  (cond ((eq (process-status conn) 'closed)
         (message "Connection closed.  Removing inferior output filter.")
         (message "Lost output: %S" string)
         (set-process-filter proc nil))
        (t
         (dime-output-filter conn string))))

(defun dime-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create (dime-buffer-name :trace))))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (incf dime-last-output-target-id)))
        (puthash target marker dime-output-target-to-marker)
        (dime-eval `(swank:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; dime-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun dime-inspector-copy-down-to-repl (number)
   "Evaluate the inspector slot at point via the REPL (to set `*')."
   (interactive (list (or (get-text-property (point) 'dime-part-number)
                          (error "No part at point"))))
   (dime-repl-send-string (format "%s" `(swank:inspector-nth-part ,number)))
   (dime-repl))

(defun sldb-insert-frame-call-to-repl ()
  "Insert a call to a frame at point."
  (interactive)
  (let ((call (dime-eval `(swank-backend::frame-call
                            ,(sldb-frame-number-at-point)))))
    (dime-switch-to-output-buffer)
    (if (>= (point) dime-repl-prompt-start-mark)
        (insert call)
	(save-excursion
	  (goto-char (point-max))
	  (insert call))))
  (dime-repl))

(defun dime-set-default-directory (directory)
  "Make DIRECTORY become Dylan's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (dime-from-dylan-filename
              (dime-repl-shortcut-eval `(swank:set-default-directory
                                          ,(dime-to-dylan-filename dir)))))
    (with-current-buffer (dime-output-buffer)
      (setq default-directory dir))))

(defun dime-sync-project-and-default-directory ()
  "Set Dylan's project and directory to the values in current buffer."
  (interactive)
  (let* ((project (dime-current-project))
         (exists-p (or (null project)
                       (dime-eval `(cl:packagep (swank::guess-package ,project)))))
         (directory default-directory))
    (when (and project exists-p)
      (dime-repl-set-project project))
    (dime-set-default-directory directory)
    ;; Sync *inferior-dylan* dir
    (let* ((proc (dime-process))
           (buffer (and proc (process-buffer proc))))
      (when buffer
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "project: %s%s  directory: %s"
             (with-current-buffer (dime-output-buffer)
               (dime-current-project))
             (if exists-p "" (format " (project %s doesn't exist)" project))
             directory)))

(defun dime-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((dime-dispatching-connection (dime-connection-at-point)))
    (switch-to-buffer (dime-output-buffer))))

(defvar dime-repl-easy-menu
  (let ((C '(dime-connected-p)))
    `("REPL"
      [ "Send Input"             dime-repl-return ,C ]
      [ "Close and Send Input "  dime-repl-closing-return ,C ]
      [ "Interrupt Dylan process" dime-interrupt ,C ]
      "--"
      [ "Previous Input"         dime-repl-previous-input t ]
      [ "Next Input"             dime-repl-next-input t ]
      [ "Goto Previous Prompt "  dime-repl-previous-prompt t ]
      [ "Goto Next Prompt "      dime-repl-next-prompt t ]
      [ "Clear Last Output"      dime-repl-clear-output t ]
      [ "Clear Buffer "          dime-repl-clear-buffer t ]
      [ "Kill Current Input"     dime-repl-kill-input t ])))

(defun dime-repl-add-easy-menu ()
  (easy-menu-define menubar-dime-repl dime-repl-mode-map
    "REPL" dime-repl-easy-menu)
  (easy-menu-define menubar-dime dime-repl-mode-map
    "DIME" dime-easy-menu)
  (easy-menu-add dime-repl-easy-menu 'dime-repl-mode-map))

(add-hook 'dime-repl-mode-hook 'dime-repl-add-easy-menu)

(defun dime-hide-inferior-dylan-buffer ()
  "Display the REPL buffer instead of the *inferior-dylan* buffer."
  (let* ((buffer (if (dime-process)
                     (process-buffer (dime-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (dime-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

(defun dime-repl-connected-hook-function ()
  (destructuring-bind (project prompt)
      (let ((dime-current-thread t))
	(dime-eval '(swank:create-repl nil)))
    (setf (dime-dylan-project-prompt-string) prompt))
  (dime-hide-inferior-dylan-buffer)
  (dime-init-output-buffer (dime-connection)))

(defun dime-repl-event-hook-function (event)
  (destructure-case event
    ((:write-string output &optional target)
     (dime-write-string output target)
     t)
    ((:read-string thread tag)
     (assert thread)
     (dime-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (dime-repl-abort-read thread tag)
     t)
    ((:open-dedicated-output-stream port)
     (dime-open-stream-to-dylan port)
     t)
    ((:new-project project prompt-string)
     (setf (dime-dylan-project-prompt-string) prompt-string)
     (let ((buffer (dime-connection-output-buffer)))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (setq dime-buffer-project project))))
     t)
    (t nil)))

(defun dime-repl-find-buffer-project ()
  "common-dylan")

(defun dime-repl-remove-hooks ()
  (remove-hook 'dime-event-hooks 'dime-repl-event-hook-function)
  (remove-hook 'dime-connected-hook 'dime-repl-connected-hook-function))

(provide 'dime-repl)
