;;;
;;; This file implements fill-comment-paragraph.
;;;
;;; Written by Bill Chiles.
;;;

(defun fill-comment-paragraph ()
  "Fill all lines adjacent to the current line that contain the same comment
   prefix.  The comment prefix starts at the beginning of the current line
   and ends after any whitespace following the comment characters.  If the
   current line does not begin with a comment, then this fills all adjacent
   lines with the same leading whitespace."
  (interactive)
  (let* ((prefix (fill-comment-paragraph-prefix))
	 (prefix-regexp (concat (regexp-quote prefix) "[^ \t]"))
	 (start
	  (save-excursion
	    (forward-line -1)
	    (beginning-of-line)
	    (while (and (looking-at prefix-regexp) (not (bobp)))
	      (forward-line -1)
	      (beginning-of-line))
	    (if (not (looking-at prefix-regexp)) (forward-line 1))
	    (point)))
	 (end
	  (save-excursion
	    (forward-line 1)
	    (beginning-of-line)
	    (while (and (looking-at prefix-regexp)
			(progn (forward-line 1) (not (eobp))))
	      (beginning-of-line))
	    (if (eobp) (newline 1))
	    (point))))
    (let ((fill-prefix prefix))
      (fill-region-as-paragraph start end nil))))

;;; This function returns the "comment prefix" as a string.  If the line
;;; begins with a comment, the comment prefix is any whitespace, the comment
;;; start characters, and any whitespace following the is comment
;;; characters.  If the line does not begin with a comment, then the comment
;;; prefix is the leading whitespace on the line.  In this situation, some
;;; whitespace is required.
;;;
(defun fill-comment-paragraph-prefix ()
  (save-excursion
    (end-of-line)
    (let ((eol (point)))
      (beginning-of-line)
      (let ((start (point)))
	(if (or (and comment-start
		     (re-search-forward (concat "[ \t]*"
						(regexp-quote comment-start)
						;; Match any number of the
						;; last char of comment-start.
						"+[ \t]*")
					eol t))
		(re-search-forward "^[ \t]+" eol t))
	    (buffer-substring start (point))
	  (error "Line must be a comment or contain leading whitespace."))))))
