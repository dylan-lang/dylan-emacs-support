;;;
;;; This file implements commands for interacting with Mindy, the compiler
;;; and the interpreter.
;;;
;;; Written by Bill Chiles.
;;;

;;; Currently, there's not much to offer, but it's a start :-).
;;;

(defvar mindy-compiler-switches ""
  "*The mindy-compile command splices this string after the compiler and
   before the file name on the command line.")

(defvar mindy-compiler "/afs/cs.cmu.edu/project/gwydion/mindy/bin/mc"
  "*This is the pathname to invoke the Mindy compiler.")

(defun mindy-compile ()
  "Compile the Dylan program in the current debugger.
   This function creates a shell command line from the variables mindy-compiler
   and mindy-compiler-switches, and the pathname from the current buffer.  Then
   this function calls compile on the shell command line.  For convenience,
   this command looks in the file headers for a 'library:' header and fills
   in a '-l' switch to the Mindy compiler if there is no -l switch already.
   Note, this is a development convenience only, and Dylan deprecates using
   file headers to associate code with a library."
  (interactive)
  (let* ((libp (string-match "-l\\([a-zA-Z0-9!?$%@_]+\\)"
			     mindy-compiler-switches))
	 (switches-lib (if libp
			   (buffer-substring (match-beginning 1)
					     (match-end 1))))
	 (buffer-lib
	  (save-excursion
	    (goto-char (point-min))
	    (let ((end (save-excursion
			 ;; Assume if there is a first blank line, then it
			 ;; terminates file headers.
			 (if (search-forward "\n\n" nil t) (point)))))
	      (if end
		  (let ((matchp (re-search-forward
				 "library:[ \t]*\\([a-zA-Z0-9!?$%@_]+\\)"
				 end t)))
		    (if matchp
			(buffer-substring (match-beginning 1)
					  (match-end 1))))))))
	 (switches
	  (cond ((and switches-lib buffer-lib)
		 (if (string-equal (downcase switches-lib)
				   (downcase buffer-lib))
		     mindy-compiler-switches
		   (error "Mindy-compiler-switches disagrees with library specified in file headers.")))
		(switches-lib
		 mindy-compiler-switches)
		(buffer-lib
		 (concat "-l" buffer-lib " " mindy-compiler-switches)))))
    (compile (concat mindy-compiler " " switches " " buffer-file-name))))
