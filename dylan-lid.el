;;; dylan-lid.el --- Dylan LID major mode -*- lexical-binding: t -*-

;; Lineage: Open Dylan

;; Copyright (C) 2014 Erik Charlebois
;; Copyright (C) 2017, 2018 Peter Hull
;; Copyright (C) 2021 Lassi Kortela
;; SPDX-License-Identifier: GPL-2.0-or-later

;; URL: https://opendylan.org/

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Dylan LID mode is a major mode for editing LID files which define libraries
;; for the Dylan programming language. It provides indenting and
;; font-lock support.

;;; Code:

;;; Customization:

(defgroup dylan-lid nil
  "Major mode for editing Dylan LID files."
  :group 'languages)

;;; Faces:

(defface dylan-lid-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Dylan LID keywords."
  :group 'dylan-lid)

(defface dylan-lid-value-face
  '((t))
  "Face for Dylan LID values."
  :group 'dylan-lid)

(defface dylan-lid-error-face
  '((t :inherit font-lock-warning-face))
  "Face for invalid lines in Dylan LID files.

Valid lines begin with a keyword or a value continuation
whitespace prefix."
  :group 'dylan-lid)

;;; Regular expressions:

(defvar dylan-lid-font-lock-keywords
  `(
    ;; Header lines with keywords, and lines with value continuations.
    (,(concat "^"
              "\\(?:\\("
              "[a-zA-Z][-a-zA-Z0-9]*:"  ; keyword...
              "\\)\\|"
              "[ \t]"                   ; ...or continuation prefix
              "\\)"
              "[ \t]*"                  ; space
              "\\("
              ;; Can keyword lines have empty values?
              "[^ \t\n][^\n]*?"         ; value
              "\\)"
              "[ \t]*\\(\n\\|\\'\\)")   ; tail space
     (1 'dylan-lid-keyword-face nil t)
     (2 'dylan-lid-value-face))

    ;; Invalid lines. This pattern assumes we've already tried the
    ;; pattern for header lines with keywords and it didn't match.
    ;;
    ;; Note: Ideally, we'd mark any subsequent continuation lines invalid,
    ;; too. Look into a way to do that.
    (,(concat "^"
              "[^ \t\n]"                ; any invalid prefix character
              "[^\n]*\n")               ; rest of line
     . 'dylan-lid-error-face))
  "Font lock keywords for ‘dylan-lid--mode’.  See ‘font-lock-keywords’.")

;;; Font locking:

(defun dylan-lid-font-lock-fontify-region (beg end loudly)
  "Fontify region in Dylan LID buffer.

The arguments BEG, END, LOUDLY are as for `font-lock-fontify-region'."
  (let ((font-lock-dont-widen t)
        (font-lock-keywords dylan-lid-font-lock-keywords)
        (font-lock-keywords-only t))
    (font-lock-default-fontify-region beg end loudly)))

;;; Clickable files:

(defun dylan-lid-make-file-link (start end src-dir)
  "Turn region between START and END into clickable link.

If the region points to an existing Dylan module, make it a link
that opens that module for editing. Modules are located relative
to SRC-DIR."
  (let* ((name (buffer-substring-no-properties start end))
         (fname (split-string name "\\."))
         (basename (concat (mapconcat 'file-name-as-directory
                                      (cons src-dir (butlast fname)) "")
                           (car (last fname))))
         (dylan (concat basename ".dylan")))
    (when (file-exists-p dylan)
      (let ((map (make-sparse-keymap))
            (src-file dylan))
        (define-key map [mouse-1]
          (lambda ()
            (interactive)
            (find-file src-file)))
        (put-text-property start end 'keymap map)
        (put-text-property start end 'mouse-face 'highlight)
        (put-text-property start end 'help-echo
                           "mouse-1: edit file")))))

(defun dylan-lid-make-files-clickable ()
  "Turn all Dylan modules with existing files into clickable links.

See `dylan-lid-make-file-link'."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(keymap mouse-face help-echo))
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward "^Files:\\s-*" nil t)
        (let ((bound nil))
          (save-excursion
            (when (re-search-forward "^[a-zA-Z0-9\\-]+\\s-*:" nil t)
              (setq bound (match-beginning 0))))
          (while (re-search-forward "[a-zA-Z0-9\\/\\.\\-]+" bound t)
            (let ((beg (match-beginning 0))
                  (end (match-end 0))
                  (src-dir (file-name-directory (buffer-file-name))))
              (dylan-lid-make-file-link beg end src-dir))
            (forward-line)
            (re-search-forward "\\s-*" nil t)))))))

(defun dylan-lid-make-lid-files-clickable ()
  "Turn all Dylan modules with existing files into clickable links.

Apply modifications only to `dylan-lid-mode' buffers and avoid
marking the buffers as modified.

See `dylan-lid-make-file-link'."
  (if (derived-mode-p 'dylan-lid-mode)
      (with-silent-modifications
        (dylan-lid-make-files-clickable))))

(defvar dylan-lid-timer-id nil
  "ID of the global Dylan LID timer.")

;;; dylan-lid-mode:

(defvar dylan-lid-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?: "_" table)
    table))

;;;###autoload
(define-derived-mode dylan-lid-mode prog-mode "Dylan LID"
  "Major mode for editing Dylan LID files.

May be customized with the options in the `dylan-lid' customization
group.

This mode runs the hook `dylan-lid-mode-hook', as the final step
during initialization."
  (setq-local parse-sexp-lookup-properties t)
  (setq-local font-lock-defaults
              `(dylan-lid-font-lock-keywords
                nil t nil nil
                (font-lock-fontify-region-function
                 . dylan-lid-font-lock-fontify-region)))
  (unless dylan-lid-timer-id
    (setq dylan-lid-timer-id (run-with-idle-timer 1 t 'dylan-lid-make-lid-files-clickable))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lid\\'" . dylan-lid-mode))

(provide 'dylan-lid)

;;; dylan-lid.el ends here
