;;;
;;; This file implements commands for going to dylan definitions.  Currently,
;;; the solutions are ad hoc and only work for the current buffer.  There is
;;; no environmental support, and no code for crunching several files and
;;; building a datafile of notes on what sort of definition an identifier is
;;; and where its various definitions can be found.
;;;
;;; Currently, there are two commands, goto-dylan-function and
;;; goto-dylan-class.  These are separate for two reasons:
;;;    1] I did not want a single command to prompt me all the time as to
;;;       whether it should search for a function or a class or whatever.
;;;    2] With functions, I often repeatedly invoke the command, but with
;;;       a class, you only need to find its definition once.  Of course,
;;;       the ability to research from the current point would be good
;;;       for classes when you only know part of a name, and the file contains
;;;       multiple classes with the same leading characters in their names.
;;; Anyway, quick solutions are what they are :-)
;;;
;;; Written by Bill Chiles.
;;;

(defvar dylan-definition-pattern
  "define[ \t]+\\(open[ \t]+\\|sealed[ \t]+\\)*\\(generic\\|method\\)[ \t]+")

(defvar dylan-id-pattern "[-a-zA-Z0-9!&*<>=|^$%@_+~?/]+")

(defvar dylan-not-id-pattern "[^-a-zA-Z0-9!&*<>=|^$%@_+~?/]+")

(defvar last-dylan-function-id nil)

;;; I bind this to "C-z .".
;;;
(defun goto-dylan-function (p)
  "Go to a Dylan function definition in the current buffer.
   This command uses the identifier before or surrounding point, but if the
   prefix argument is supplied, this command prompts for the identifier.
   Repeated invocations search from point forward so that you can through the
   generic function and all methods.  This command drops a mark before going
   to the definition, and it drops only one mark for repeated invocations."
  (interactive "p")
  (let* (;; Bind just-invoked before possibly reading from user.
	 (just-invoked (eq last-command 'goto-dylan-function))
	 (id (cond ((/= p 1)
		    (read-string "Function: "))
		   (just-invoked
		    last-dylan-function-id)
		   (t
		    (save-excursion
		      ;; Go backwards then forwards to make sure we get the
		      ;; whole thing.
		      (if (not (re-search-backward dylan-id-pattern nil t))
			  (error "Can't find Dylan ID."))
		      ;; Backwards doesn't match as many characters as
		      ;; possible that fit the pattern.  It only matches the
		      ;; fewest, unlike re-search-forward.  SO, move back to
		      ;; a char that is not an ID, go forward over that
		      ;; char, and then find the whole thing forward.
		      (re-search-backward dylan-not-id-pattern nil t)
		      (forward-char 1)
		      (let ((start (point)))
			(re-search-forward dylan-id-pattern)
			(buffer-substring start (point)))))))
	 (id-pattern (concat dylan-definition-pattern (regexp-quote id)))
	 (where (save-excursion
		  (if just-invoked
		      (re-search-forward id-pattern nil t)
		    (goto-char (point-min)))
		  (if (re-search-forward id-pattern nil t)
		      (point)
		    (error "Can't find definition for ID -- %s." id)))))
    (if (not just-invoked) (push-mark (point)))
    (goto-char where)
    (beginning-of-line)
    (setq last-dylan-function-id id)
    (setq this-command 'goto-dylan-function)))


(defvar dylan-define-class-pattern
  "define[ \t]+\\(open[ \t]+\\|sealed[ \t]+\\|abstract[ \t]+\\|concrete[ \t]+\\)*class")

(defun goto-dylan-class (p)
  "Go to a Dylan class definition in the current buffer.
   This command uses the identifier before or surrounding point, but if the
   prefix argument is supplied, this command prompts for the identifier."
  (interactive "p")
  (let* ((id (if (/= p 1)
		 (read-string "Class: ")
	       (save-excursion
		 ;; Go backwards then forwards to make sure we get the whole
		 ;; thing.
		 (if (not (re-search-backward dylan-id-pattern nil t))
		     (error "Can't find Dylan ID."))
		 ;; Backwards doesn't match as many characters as possible
		 ;; that fit the pattern.  It only matches the fewest,
		 ;; unlike re-search-forward.  SO, move back to a char that
		 ;; is not an ID, go forward over that char, and then find
		 ;; the whole thing forward.
		 (re-search-backward dylan-not-id-pattern nil t)
		 (forward-char 1)
		 (let ((start (point)))
		   (re-search-forward dylan-id-pattern)
		   (buffer-substring start (point))))))
	 (where (save-excursion
		  (goto-char (point-min))
		  (if (re-search-forward (concat dylan-define-class-pattern
						 "[ \t]+"
						 (regexp-quote id))
					 nil t)
		      (point)
		    (error "Can't find class definition for ID -- %s." id)))))
    (push-mark (point))
    (goto-char where)
    (beginning-of-line)))

