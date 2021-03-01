(require 'dime)

(define-dime-contrib dime-compiler-notes-tree
  "Display compiler messages in tree layout.

M-x dime-list-compiler-notes display the compiler notes in a tree
grouped by severity.

  `dime-maybe-list-compiler-notes' can be used as
  `dime-compilation-finished-hook'.
"
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL"))

(defun dime-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (cl-every #'dime-note-has-location-p notes)
    (dime-list-compiler-notes notes)))

(defun dime-list-compiler-notes (notes)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (dime-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (dime-with-popup-buffer ((dime-buffer-name :notes)
                             :mode 'dime-compiler-notes-mode)
      (when (null notes)
        (insert "[no notes]"))
      (let ((collapsed-p))
        (dolist (tree (dime-compiler-notes-to-tree notes))
          (when (dime-tree.collapsed-p tree) (setf collapsed-p t))
          (dime-tree-insert tree "")
          (insert "\n"))
        (goto-char (point-min))))))

(defvar dime-tree-printer 'dime-tree-default-printer)

(defun dime-tree-for-note (note)
  (make-dime-tree :item (dime-note.message note)
                  :plist (list 'note note)
                  :print-fn dime-tree-printer))

(defun dime-tree-for-severity (severity notes collapsed-p)
  (make-dime-tree :item (format "%s (%d)"
                                 (dime-severity-label severity)
                                 (length notes))
                  :kids (mapcar #'dime-tree-for-note notes)
                  :collapsed-p collapsed-p))

(defun dime-compiler-notes-to-tree (notes)
  (let* ((alist (dime-alistify notes #'dime-note.severity #'eq))
         (collapsed-p (dime-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (dime-tree-for-severity severity notes
                                           collapsed-p))))

(defvar dime-compiler-notes-mode-map)

(define-derived-mode dime-compiler-notes-mode fundamental-mode
  "Compiler-Notes"
  "\\<dime-compiler-notes-mode-map>\
\\{dime-compiler-notes-mode-map}
\\{dime-popup-buffer-mode-map}
"
  (dime-set-truncate-lines))

(dime-define-keys dime-compiler-notes-mode-map
  ((kbd "RET") 'dime-compiler-notes-default-action-or-show-details)
  ([return] 'dime-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'dime-compiler-notes-default-action-or-show-details/mouse))

(defun dime-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point)
                                   'dime-compiler-notes-default-action)))
	(if fn (funcall fn) (dime-compiler-notes-show-details))))))

(defun dime-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'dime-compiler-notes-default-action)))
    (if fn (funcall fn) (dime-compiler-notes-show-details))))

(defun dime-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (dime-tree-at-point))
         (note (plist-get (dime-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (dime-tree-leaf-p tree))
           (dime-tree-toggle tree))
          (t
           (dime-show-source-location (dime-note.location note) t)))))


;;;;;; Tree Widget

(cl-defstruct (dime-tree (:conc-name dime-tree.))
  item
  (print-fn #'dime-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun dime-tree-leaf-p (tree)
  (not (dime-tree.kids tree)))

(defun dime-tree-default-printer (tree)
  (princ (dime-tree.item tree) (current-buffer)))

(defun dime-tree-decoration (tree)
  (cond ((dime-tree-leaf-p tree) "-- ")
	((dime-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun dime-tree-insert-list (list prefix)
  "Insert a list of trees."
  (loop for (elt . rest) on list
	do (cond (rest
		  (insert prefix " |")
		  (dime-tree-insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (dime-tree-insert elt (concat prefix "  "))))))

(defun dime-tree-insert-decoration (tree)
  (insert (dime-tree-decoration tree)))

(defun dime-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun dime-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (dime-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (dime-tree-insert-decoration tree)
      (funcall print-fn tree)
      (dime-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'dime-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (dime-tree-insert-list kids prefix))
      (setf (dime-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun dime-tree-at-point ()
  (cond ((get-text-property (point) 'dime-tree))
        (t (error "No tree at point"))))

(defun dime-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (dime-tree.start-mark tree)
                 (dime-tree.end-mark tree)))

(defun dime-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (dime-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (dime-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (dime-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))

(provide 'dime-compiler-notes-tree)
