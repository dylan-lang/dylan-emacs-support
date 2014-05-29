;;; dylanlid-mode.el --- Major mode for editing Dylan LID files.

;; Copyright (C) 2014 Erik Charlebois

;; Author: Erik Charlebois (erikcharlebois@gmail.com)
;; Version: 1.00

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to "gwydion-bugs@cs.cmu.edu")
;; or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:

;; Dylan LID mode is a major mode for editing LID files which define libraries
;; for the Dylan programming language. It provides indenting and
;; font-lock support.
;;
;; This code requires Emacs 24.


;;; Code:

(defconst dylanlid-version "1.00"
  "Dylan LID Mode version number.")

(defun dylanlid-version ()
  "Return string describing the version of Dylan LID mode.
When called interactively, displays the version."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message (dylanlid-version))
    (format "Dylan LID Mode version %s" dylanlid-version)))


;;; Customization:

(defgroup dylanlid nil
  "Major mode for editing Dylan LID files."
  :group 'languages)


;;; Faces:

(defface dylanlid-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for Dylan LID keywords."
  :group 'dylanlid)

(defface dylanlid-value
  '((t))
  "Face for Dylan LID values."
  :group 'dylanlid)

(defface dylanlid-error
  '((t :inherit font-lock-warning-face))
  "Face for invalid lines in Dylan LID files.

Valid lines begin with a keyword or a value continuation
whitespace prefix."
  :group 'dylanlid)


;;; Regular expressions:

(defvar dylanlid-font-lock-keywords
  `(
    ;; Header lines with keywords, and lines with value continuations.
    (,(concat "^"
	      "\\(?:\\("
	      "[a-zA-Z][-a-zA-Z0-9]*:"	; keyword...
	      "\\)\\|"
	      "[ \t]"			; ...or continuation prefix
	      "\\)"
	      "[ \t]*"			; space
	      "\\("
	      ;; Can keyword lines have empty values?
	      "[^ \t\n][^\n]*?"		; value
	      "\\)"
	      "[ \t]*\\(\n\\|\\'\\)")	; tail space
     (1 'dylanlid-keyword nil t)
     (2 'dylanlid-value))

    ;; Invalid lines. This pattern assumes we've already tried the
    ;; pattern for header lines with keywords and it didn't match.
    ;;
    ;; Note: Ideally, we'd mark any subsequent continuation lines invalid,
    ;; too. Look into a way to do that.
    (,(concat "^"
	      "[^ \t\n]"		; any invalid prefix character
	      "[^\n]*\n")		; rest of line
     . 'dylanlid-error))
 "Value to which `font-lock-keywords' should be set when
fontifying Dylan LID files in Dylan LID Mode.")


;;; Font locking:

(defun dylanlid-font-lock-fontify-region (beg end loudly)
  "Dylan LID Mode font lock fontification."
  (let ((font-lock-dont-widen t)
        (font-lock-keywords dylanlid-font-lock-keywords)
        (font-lock-keywords-only t))
    (font-lock-default-fontify-region beg end loudly)))


;;; Clickable files:

(defun dylanlid-make-file-link (start end src-dir)
  "Attempt to make the region between START and END into a
 clickable link to open a module for editing, with modules located
 relative to SRC-DIR"
  (let* ((name (buffer-substring-no-properties start end))
         (fname (split-string name "\\."))
         (basename (concat (mapconcat 'file-name-as-directory
                                      (cons src-dir (butlast fname)) "")
                           (car (last fname))))
         (dylan (concat basename ".dylan")))
    (when (file-exists-p dylan)
      (lexical-let ((map (make-sparse-keymap))
                    (src-file dylan))
        (define-key map [mouse-1]
          (lambda ()
            (interactive)
            (find-file src-file)))
        (put-text-property start end 'keymap map)
        (put-text-property start end 'mouse-face 'highlight)
        (put-text-property start end 'help-echo
                           "mouse-1: edit file")))))

(defun dylanlid-make-files-clickable ()
  "Make all modules with existing files clickable, where clicking opens them"
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(keymap mouse-face help-echo))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Files:\\s-*" nil t)
      (lexical-let ((bound nil))
        (save-excursion
          (when (re-search-forward "^[a-zA-Z0-9\\-]+\\s-*:" nil t)
            (setq bound (match-beginning 0))))
        (while (re-search-forward "[a-zA-Z0-9\\/\\.\\-]+" bound t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (src-dir (file-name-directory (buffer-file-name))))
            (dylanlid-make-file-link beg end src-dir))
          (next-line)
          (beginning-of-line)
          (re-search-forward "\\s-*" nil t))))))


;;; dylanlid-mode:

(defvar dylanlid-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?: "_" table)
    table))

;;;###autoload
(define-derived-mode dylanlid-mode prog-mode "Dylan LID"
  "Major mode for editing Dylan LID files.

May be customized with the options in the `dylanlid' customization
group.

To see the current version of Dylan LID Mode, enter
`\\[dylanlid-version]'.

This mode runs the hook `dylanlid-mode-hook', as the final step
during initialization."
  (setq-local parse-sexp-lookup-properties t)
  (setq-local font-lock-defaults
              `(dylanlid-font-lock-keywords
                nil t nil nil
                (font-lock-fontify-region-function
                 . dylanlid-font-lock-fontify-region)))

  (run-with-idle-timer 1 t 'dylanlid-make-files-clickable))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lid\\'" . dylanlid-mode))


(provide 'dylanlid-mode)

;;; dylanlid-mode.el ends here
