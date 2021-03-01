;;; dime-note-tree.el --- Dylan interaction mode -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; URL: https://opendylan.org/
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Dime add-on to browse compiler notes as a tree.

;;; Code:

(require 'cl-lib)

(require 'dime)

(define-dime-contrib dime-note-tree
  "Display compiler messages in tree layout.

M-x dime-note-tree-show display the compiler notes in a tree
grouped by severity.

  `dime-note-tree-maybe-show' can be used as
  `dime-compilation-finished-hook'.
"
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL"))

(defun dime-note-tree-maybe-show (notes)
  "Show the compiler NOTES if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (cl-every #'dime-note-has-location-p notes)
    (dime-note-tree-show notes)))

(defun dime-note-tree-show (notes)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (dime-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (dime-with-popup-buffer ((dime-buffer-name :notes)
                             :mode 'dime-note-tree-mode)
      (when (null notes)
        (insert "[no notes]"))
      (dolist (tree (dime-note-tree--from-notes notes))
        (dime-note-tree--insert tree "")
        (insert "\n"))
      (goto-char (point-min)))))

(defvar dime-note-tree-printer 'dime-note-tree-default-printer)

(defun dime-note-tree--for-note (note)
  "Make a note tree for the given NOTE."
  (make-dime-note-tree :item (dime-note.message note)
                       :plist (list 'note note)
                       :print-fn dime-note-tree-printer))

(defun dime-note-tree--for-severity (severity notes collapsed-p)
  "Make a note tree for NOTES of the given SEVERITY.

COLLAPSED-P says whether the tree is initially collapsed."
  (make-dime-note-tree :item (format "%s (%d)"
                                     (dime-severity-label severity)
                                     (length notes))
                       :kids (mapcar #'dime-note-tree--for-note notes)
                       :collapsed-p collapsed-p))

(defun dime-note-tree--from-notes (notes)
  "Make a note tree from a list of NOTES."
  (let* ((alist (dime-alistify notes #'dime-note.severity #'eq))
         (collapsed-p (dime-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (dime-note-tree--for-severity severity notes
                                                collapsed-p))))

(defvar dime-note-tree-mode-map (make-sparse-keymap)
  "Keymap for Dime note tree mode.")

(define-derived-mode dime-note-tree-mode fundamental-mode
  "Compiler-Notes"
  "\\<dime-note-tree-mode-map>\
\\{dime-note-tree-mode-map}
\\{dime-popup-buffer-mode-map}
"
  (dime-set-truncate-lines))

(dime-define-keys dime-note-tree-mode-map
  ((kbd "RET") 'dime-note-tree-default-action-or-show-details)
  ([return] 'dime-note-tree-default-action-or-show-details)
  ([mouse-2] 'dime-note-tree-mouse-default-action-or-show-details))

(defun dime-note-tree-mouse-default-action-or-show-details (event)
  "Invoke the action pointed at by the mouse, or show details.

This command is meant to be bound to a mouse EVENT."
  (interactive "e")
  (cl-destructuring-bind (_mouse-2 (_w pos &rest ignore1) &rest ignore2) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point)
                                   'dime-compiler-notes-default-action)))
	(if fn (funcall fn) (dime-note-tree-show-details))))))

(defun dime-note-tree-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'dime-compiler-notes-default-action)))
    (if fn (funcall fn) (dime-note-tree-show-details))))

(defun dime-note-tree-show-details ()
  "Show details for the note tree at point."
  (interactive)
  (let* ((tree (dime-note-tree--at-point))
         (note (plist-get (dime-note-tree--plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (dime-note-tree--leaf-p tree))
           (dime-note-tree--toggle tree))
          (t
           (dime-show-source-location (dime-note.location note) t)))))


;;;;;; Tree Widget

(cl-defstruct (dime-note-tree (:conc-name dime-note-tree--))
  item
  (print-fn #'dime-note-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun dime-note-tree--leaf-p (tree)
  "Return t if the given note TREE is a leaf node, nil otherwise."
  (not (dime-note-tree--kids tree)))

(defun dime-note-tree-default-printer (tree)
  "Print note TREE using default representation."
  (princ (dime-note-tree--item tree) (current-buffer)))

(defun dime-note-tree--decoration (tree)
  "Return decorative prefix for note TREE as a string."
  (cond ((dime-note-tree--leaf-p tree) "-- ")
	((dime-note-tree--collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun dime-note-tree--insert-list (list prefix)
  "Insert LIST of note trees using PREFIX for each."
  (loop for (elt . rest) on list
	do (cond (rest
		  (insert prefix " |")
		  (dime-note-tree--insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (dime-note-tree--insert elt (concat prefix "  "))))))

(defun dime-note-tree--indent-item (start end prefix)
  "Insert PREFIX at the beginning of each line except the first.

This is used for labels spanning multiple lines. START and END
are the region to modify."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun dime-note-tree--insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (dime-note-tree-- print-fn kids collapsed-p start-mark end-mark)
      tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (insert (dime-note-tree--decoration tree))
      (funcall print-fn tree)
      (dime-note-tree--indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'dime-note-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (dime-note-tree--insert-list kids prefix))
      (setf (dime-note-tree--prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun dime-note-tree--at-point ()
  "Return the note tree at point, error if none."
  (cond ((get-text-property (point) 'dime-note-tree))
        (t (error "No tree at point"))))

(defun dime-note-tree--delete (tree)
  "Delete the region for TREE."
  (delete-region (dime-note-tree--start-mark tree)
                 (dime-note-tree--end-mark tree)))

(defun dime-note-tree--toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (dime-note-tree-- collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (dime-note-tree--delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (dime-note-tree--insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))

(provide 'dime-note-tree)

;;; dime-note-tree.el ends here
