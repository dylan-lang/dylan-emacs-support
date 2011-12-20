;; Interface to optimization coloring.

(defvar color-not-all-methods-known            "Magenta")
(defvar color-failed-to-select-where-all-known "Red")
(defvar color-lambda-call                      "Blue")
(defvar color-inlining                         "DimGray")
(defvar color-slot-accessor-fixed-offset       "ForestGreen")
(defvar color-eliminated                       "Pink")
(defvar color-dynamic-extent                   "DarkTurquoise")
(defvar color-program-notes                    "Yellow")
(defvar color-background                       "white")

(defvar color-bogus-upgrade                    "Orange")

(defface face-not-all-methods-known
  `((t (:background "magenta")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-failed-to-select-where-all-known
  `((t (:background "red")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-lambda-call
  `((t (:background "lightskyblue")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-inlining
  `((t (:background "dimgray")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-slot-accessor-fixed-offset
  `((t (:background "forestgreen")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-eliminated
  `((t (:background "pink")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-dynamic-extent
  `((t (:background "DarkTurquoise")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-program-notes
  `((t (:background "yellow")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-bogus-upgrade
  `((t (:background "orange")))
  "Bla bla bla"
  :group 'slime-mode-faces)

(defun find-dylan-library ()
  (let ((lid-files (find-dylan-lid-files)))
    (save-excursion
      (if lid-files
	(let ((try-lid (car lid-files)))
	  (set-buffer (find-file-noselect try-lid))
	  (goto-char (point-min))
	  (let ((found 
		  (re-search-forward "[Ll]ibrary:[ \t]*\\([-a-z0-9]*\\)")))
	    (if found
	      (buffer-substring 
	        (match-beginning 1)
		(match-end 1)))))))))

(defun find-dylan-lid-files ()
  (directory-files "." t ".*\\.lid" t))

(defun dylan-color-file ()
  (let* ((path (buffer-file-name))
	 (name (file-name-nondirectory path))
	 (stem (substring name 0 (string-match "\\.[^.]*$" name)))
	 (library (find-dylan-library)))
    (expand-file-name
     (concat (or (getenv "OPEN_DYLAN_USER_ROOT") "~/Open-Dylan")
             "/build/" library "/" stem ".el"))))

(defun color-foregrounds (color l)
  (save-excursion
    (while (not (null l))
      (let* ((spec (car l))
           (sl (car spec)) (sc (car (cdr spec)))
         (el (car (cdr (cdr spec)))) (ec (car (cdr (cdr (cdr spec))))))
        (goto-char 1) (forward-line (1- sl)) (forward-char sc)
        (let ((start (point))
              (overlay (slime-note-at-point)))
          (goto-char 1) (forward-line (1- el)) (forward-char ec)
          (let ((end (point)))
            (let ((over (make-overlay start end)))
              (if (string= color "Magenta")
                  (overlay-put over 'face 'face-not-all-methods-known)
                (if (string= color "Red")
                    (overlay-put over 'face 'face-failed-to-select-where-all-known)
                  (if (string= color "Blue")
                      (overlay-put over 'face 'face-lambda-call)
                    (if (string= color "ForestGreen")
                        (overlay-put over 'face 'face-slot-accessor-fixed-offset)
                      (if (string= color "Pink")
                          (overlay-put over 'face 'face-eliminated)
                        (if (string= color "DarkTurquoise")
                            (overlay-put over 'face 'face-dynamic-extent)
                          (if (string= color "Yellow")
                              (overlay-put over 'face 'face-program-notes)
                            (if (string= color "Orange")
                                (overlay-put over 'face 'face-bogus-upgrade)))))))))))))
;              (overlay-put over 'face color)))))
;            (if overlay
;                (slime-merge-note-into-overlay overlay severity message)
;              (slime-create-note-overlay "what?" start end :warning 'color)))))
;        (set-mark (point))
;        (message (concat "set mark at " (number-to-string (+ sl 1)) ":" (number-to-string sc) "-" (number-to-string (+ el 1)) ":" (number-to-string ec) ":" color)))
;        (facemenu-set-foreground color))
      (setq l (cdr l)))))

(defun color-backgrounds (color l)
  (save-excursion
    (while (not (null l))
      (let* ((spec (car l))
           (sl (car spec)) (sc (car (cdr spec)))
         (el (car (cdr (cdr spec)))) (ec (car (cdr (cdr (cdr spec))))))
        (goto-char 1) (forward-line (1- sl)) (forward-char sc)
        (set-mark (point))
        (goto-char 1) (forward-line (1- el)) (forward-char ec)
        (facemenu-set-background color))
      (setq l (cdr l)))))

(defun color-optimizations ()
  "Color the current Dylan file with recorded optimization information"
  (interactive)
  (let ((file (dylan-color-file)))
;    (setq old-buffer-read-only buffer-read-only)
;    (setq buffer-read-only nil)
;    (point-to-register 1)
;    (end-of-buffer)
;    (facemenu-set-foreground "black" 1)
;    (facemenu-set-background "yellow" 1)
;    (set-mark (point))
;    (slime-create-note-overlay "fooo" 10 20 :warning "barffff")
;    (let ((overlay (make-overlay 10 20)))
;      (overlay-put overlay 'slime-note "foooo"))
;    (forward-line 3)
;    (facemenu-set-foreground "green")
;    (register-to-point 1)

;    (message "Using color file: %s" file)
    (load-file file)
;    (message "Used color file: %s" file)

    (cond
      (old-buffer-read-only
        (setq buffer-read-only old-buffer-read-only)
        (set-buffer-modified-p nil))) ))

(if dylan-mode-map
    (progn
      (define-key dylan-mode-map [menu-bar dylan-misc dylan-color-optimizations] '("Color Optimizations" . color-optimizations))))

;; eof
