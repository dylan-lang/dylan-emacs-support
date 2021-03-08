;;; dylan-opt.el --- Dylan optimization faces -*- lexical-binding: t -*-

;; Author: Hannes Mehnert
;; Author: Lassi Kortela
;; SPDX-License-Identifier: GPL-2.0-or-later
;; Package-Requires: ((emacs "25.1"))
;; URL: https://opendylan.org/

;;; Commentary:

;; Read optimization information emitted by the Dylan compiler.  Add
;; extra faces on top of the ordinary `dylan-mode' faces to indicate
;; which parts of the source code were optimized in what ways.

;;; Code:

;; TODO: major problem: if one coloring of the same type is contained
;; in another, you can't recognize the inner one - make the color
;; lighter/darker/a white background at the borders? any good ideas
;; here?

(require 'cl-lib)

(require 'dylan-mode)

(defgroup dylan-opt nil
  "Major mode for editing Dylan source code."
  :group 'dylan)

(defface dylan-opt-face-not-all-methods-known
  `((t (:background "magenta")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

(defface dylan-opt-face-failed-to-select-where-all-known
  `((t (:background "red")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

(defface dylan-opt-face-lambda-call
  `((t (:background "lightskyblue")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

;; should be darkgray...
(defface dylan-opt-face-inlining
  `((t (:background "dimgray")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

(defface dylan-opt-face-slot-accessor-fixed-offset
  `((t (:background "forestgreen")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

;; should be lightgray according to documentation
(defface dylan-opt-face-eliminated
  `((t (:background "pink")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

;; no documentation for this :/
(defface dylan-opt-face-dynamic-extent
  `((t (:background "DarkTurquoise")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

(defface dylan-opt-face-program-notes
  `((t (:background "yellow")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

;; no documentation for that - and according to source only relevant
;; for dylan library?
(defface dylan-opt-face-bogus-upgrade
  `((t (:background "orange")))
  "Face for Dylan optimization information."
  :group 'dylan-opt)

(defconst dylan-opt--type-alist
  '((color-not-all-methods-known
     dylan-opt-face-not-all-methods-known
     "Not all methods known")
    (color-failed-to-select-where-all-known
     dylan-opt-face-failed-to-select-where-all-known
     "Failed to select where all known")
    (color-lambda-call
     dylan-opt-face-lambda-call
     "Lambda call")
    (color-inlining
     dylan-opt-face-inlining
     "Inlining")
    (color-slot-accessor-fixed-offset
     dylan-opt-face-slot-accessor-fixed-offset
     "Slot accessor fixed offset")
    (color-eliminated
     dylan-opt-face-eliminated
     "Eliminated")
    (color-dynamic-extent
     dylan-opt-face-dynamic-extent
     "Dynamic extent")
    (color-program-notes
     dylan-opt-face-program-notes
     "Program note")
    (color-bogus-upgrade
     dylan-opt-face-bogus-upgrade
     "Bogus upgrade"))
  "List of optimization types emitted by the Dylan compiler.

Each entry has 3 elements:

- The color-* symbol used in the compiler's dump file format.
- The dylan-opt-face-* face name used in Emacs.
- The human-readable title shown in mouse pop-ups.")

(defvar-local dylan-opt--regions '()
  "Optimization regions parsed from the Dylan compiler.")

;; TODO: `dylan-opt--remove-overlays' should use ELisp search
;; functions to find all optimization overlays in the buffer.  We
;; shouldn't have to manually keep track of them in this list.
(defvar-local dylan-opt--overlays '()
  "All Dylan optimization overlays for the current buffer.")

(defun dylan-opt--read-all (stream)
  "Helper to read all Emacs Lisp forms from STREAM."
  (let ((forms '()))
    (condition-case _ (while t (push (read stream) forms))
      (end-of-file (reverse forms)))))

(defun dylan-opt--parse-regions (opt-file)
  "Parse Dylan optimization regions from OPT-FILE."
  (cl-flet ((check (x) (unless x
                         (error
                          "Dylan optimization file uses unknown format"))))
    (with-temp-buffer
      (insert-file-contents opt-file)
      (save-match-data
        (check (looking-at (regexp-quote "; HQNDYLANCOLRINFO 1 0")))
        (goto-char (match-end 0)))
      (let ((forms (dylan-opt--read-all (current-buffer)))
            (regions '()))
        (dolist (form forms (reverse regions))
          (check (listp form))
          (check (equal 3 (cl-list-length form)))
          (check (memq (nth 0 form) '(color-backgrounds color-foregrounds)))
          (let* ((opt-type (nth 1 form))
                 (oregions (nth 2 form)))
            (check (and (symbolp opt-type) (not (null opt-type))))
            (check (equal 2 (cl-list-length oregions)))
            (check (eq 'quote (nth 0 oregions)))
            (setq oregions (nth 1 oregions))
            (mapc (lambda (oregion)
                    (check (listp oregion))
                    (check (equal 4 (cl-list-length oregion)))
                    (check (cl-every #'integerp oregion))
                    (push (cons opt-type oregion) regions))
                  oregions)))))))

(defun dylan-opt--remove-overlays ()
  "Remove Dylan optimization faces from the current buffer."
  (mapc #'delete-overlay dylan-opt--overlays)
  (setq dylan-opt--overlays '()))

(defun dylan-opt--add-overlays ()
  "Add Dylan optimization faces to the current buffer."
  (save-excursion
    (dylan-opt--remove-overlays)
    (dolist (region dylan-opt--regions)
      (cl-destructuring-bind (opt-type sl sc el ec) region
        (goto-char 1)
        (forward-line (1- sl))
        (forward-char sc)
        (let* ((start (point))
               (overlay (overlays-at (point)))
               (opt-data (assq opt-type dylan-opt--type-alist))
               (opt-face (nth 1 opt-data))
               (opt-help (nth 2 opt-data)))
          (goto-char 1)
          (forward-line (1- el))
          (forward-char ec)
          (let ((end (point)))
            (when overlay
              (let ((oldstart (overlay-start (car overlay)))
                    (oldend (overlay-end (car overlay)))
                    (oldface (overlay-get (car overlay) 'face))
                    (oldmsg (overlay-get (car overlay) 'help-echo)))
                (delete-overlay (car overlay))
                (setq dylan-opt--overlays
                      (remove (car overlay) dylan-opt--overlays))
                (let ((over1 (make-overlay oldstart start))
                      (over2 (make-overlay end oldend)))
                  (overlay-put over1 'face oldface)
                  (overlay-put over2 'face oldface)
                  (overlay-put over1 'help-echo oldmsg)
                  (overlay-put over2 'help-echo oldmsg)
                  (push over1 dylan-opt--overlays)
                  (push over2 dylan-opt--overlays))))
            (let ((over (make-overlay start end)))
              (overlay-put over 'face opt-face)
              (overlay-put over 'help-echo opt-help)
              (push over dylan-opt--overlays))))))))

;;;###autoload
(define-minor-mode dylan-opt-mode
  "Toggle Dylan-Opt minor mode for optimization info.

This mode can be used on top of `dylan-mode'.  It shows how
different regions of a Dylan source file have been optimized by
the compiler.  The Dylan compiler can produce an optimization dump
file (*.el) as a byproduct of normal compilation.  Use the
`dylan-opt' command to feed that file to Emacs and to enable
highlighting.

Once the dump file has been loaded, the `dylan-opt-mode' command
can be used to toggle the optimization highlighting on and off."
  :lighter " Opt"
  :global nil
  (cond ((not dylan-opt-mode)
         (dylan-opt--remove-overlays))
        ((not (null dylan-opt--regions))
         (dylan-opt--add-overlays))
        (t
         (message "Use `dylan-opt' to load optimization information"))))

(defun dylan-opt--default-file-name ()
  "Guess a default optimization file to match the current buffer."
  (let* ((path (buffer-file-name))
         (name (file-name-nondirectory path))
         (stem (substring name 0 (string-match "\\.[^.]*$" name)))
         (library dylan-buffer-library))
    (expand-file-name
     (concat (or (getenv "OPEN_DYLAN_USER_ROOT") "_build")
             "/build/" library "/" stem ".el"))))

;;;###autoload
(defun dylan-opt (opt-file)
  "Show Dylan optimization faces according to OPT-FILE.

See the command `dylan-opt-mode', which this command enables."
  (interactive
   (list (let ((default (dylan-opt--default-file-name)))
           (read-file-name
            "Dylan optimization file: "
            (file-name-directory default) nil t
            (file-name-nondirectory default)
            (lambda (x) (string-match ".*\\.el" x))))))
  (setq dylan-opt--regions (dylan-opt--parse-regions opt-file))
  (dylan-opt-mode 1))

(provide 'dylan-opt)

;;; dylan-opt.el ends here
