(defun dylan-find-buffer-module ()
  (let ((case-fold-search t)
        (regexp "^module:[ \t]*\\([^ \n\r\t]+\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))

(defun dylan-find-buffer-library ()
  (let ((lid-files (dylan-find-lid-files)))
    (save-excursion
      (if lid-files
	(let ((try-lid (car lid-files)))
	  (set-buffer (find-file-noselect try-lid))
	  (goto-char (point-min))
	  (let ((found
		  (re-search-forward "[Ll]ibrary:[ \t]*\\([^ \n\r\t]+\\)")))
	    (if found
	      (buffer-substring
	        (match-beginning 1)
		(match-end 1)))))))))

(defun dylan-find-lid-files ()
  (directory-files "." t ".*\\.lid" t))

(provide 'dylan-common)