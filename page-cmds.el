;;;
;;; This file implements goto-page and view-page-directory on top of
;;; gnu-emacs's page primitives.
;;;
;;; Written by Bill Chiles.
;;;

;;; This holds the string used in goto-page between calls to the command.
;;;
(defvar goto-page-last-string nil)

(defun goto-page (p)
  "Go to page numbered according to view-page-directory.
   The prefix argument determines the page number.  If the prefix argument is
   zero, then this command prompts for a string and goes to the page with the
   string in its title.  Repeatedly invoking this command with the prefix
   argument set to zero causes it to search for the page title from the current
   position rather than starting at the beginning of the buffer.  This command
   drops a mark, but when it is invoked repeatedly, it only drops a mark the
   first time."
  (interactive "p")
  (let ((original-pos (point)))
    (cond ((= p 0)
	   (let* (;; Must save last-command test before prompting.
		  (just-invoked (eq last-command 'goto-page-string))
		  (str (if just-invoked
			   (let ((str (read-string 
				       (concat "Go to page with string ["
					       goto-page-last-string
					       "]: "))))
			     (if (string-equal str "")
				 goto-page-last-string
			       str))
			 (read-string "Go to page with string: ")))
		  ;; Where saves the point right after page-delimiter on each
		  ;; iteration of the loop below.
		  (where nil)
		  ;; We use won to terminate the loop when we find the string.
		  (won nil))
	     (if just-invoked
		 (forward-page)
	       (push-mark (point))
	       (goto-char (point-min)))
	     ;; Set where to point in case the first page we look at contains
	     ;; the string.
	     (setq where (point))
	     (while (and (if (goto-page-title)
			     (let ((end (point)))
			       (beginning-of-line)
			       (if (search-forward str end t)
				   (setq won t)
				 ;; If not this page, keep looping over pages.
				 t))
			   ;; If page is blank, keep looping over pages.
			   t)
			 ;; If we found the string, get out of here.
			 (not won)
			 ;; We must test for eob because forward-page always
			 ;; returns nil.  How winning -- NOT.
			 (not (eobp)))
	       (forward-page)
	       (setq where (point)))
	     (cond (won
		    (goto-char where)
		    (if (not just-invoked) (push-mark original-pos))
		    (setq this-command 'goto-page-string)
		    (setq goto-page-last-string str))
		   (t
		    (goto-char original-pos)
		    (error "No page with title \"%s\"." str)))))
	  ((< p 0)
	   (error "Page number must be positive."))
	  (t
	   (push-mark (point))
	   (goto-char (point-min))
	   (setq p (1- p))
	   (while (not (or (= p 0) (eobp)))
	     (forward-page)
	     (setq p (1- p)))
	   (cond ((not (= p 0))
		  (goto-char original-pos)
		  (error "No such page.")))))))

(defun view-page-directory ()
  "Display the current buffer's page directory in the buffer '*page-dir*'."
  (interactive)
  (let ((dir (page-directory))
	(buf (get-buffer-create "*page-dir*"))
	(current-buf (current-buffer)))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (let ((count 1))
      (while dir
	(let* ((count-str (int-to-string count))
	       (count-str-len (length count-str)))
	  ;; Formatting hack.  Assumes three digit or smaller page count.
	  (cond ((= count-str-len 1)
		 (insert "  " count-str))
		((= count-str-len 2)
		 (insert " " count-str))
		(t (insert count-str)))
	  (insert ": " (car dir) "\n")
	  (setq dir (cdr dir))
	  (setq count (1+ count)))))
    (goto-char (point-min))
    (switch-to-buffer-other-window current-buf)))

;;; This function returns a list of strings, one each for the first
;;; non-blank line following each page-delimiter pattern in the current
;;; buffer.
;;;
(defun page-directory ()
  (save-excursion
    (goto-char (point-min))
    (let ((res nil))
      (while (not (eobp))
	(if (goto-page-title)
	    (let ((end (point)))
	      (beginning-of-line)
	      (setq res (cons (buffer-substring (point) end) res)))
	  (setq res (cons "" res)))
	(forward-page))
      (reverse res))))

;;; This function goes to the first non-blank line following point.  The
;;; target line may be the line currently containing point.  If a non-blank
;;; line occurs on page, this function returns t, otherwise nil.  Note, when
;;; iterating over pages, the forward-page call in this function causes the
;;; caller of goto-page-title to call forward-page twice as many times.
;;;
(defun goto-page-title ()
  (let ((limit (save-excursion
		 (forward-page)
		 (beginning-of-line)
		 (point))))
    (re-search-forward "[^ \t\n]+$" limit t)))
