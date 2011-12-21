;; Interface to optimization coloring.

(defvar color-not-all-methods-known            "not-all-known")
(defvar color-failed-to-select-where-all-known "failed-to-select")
(defvar color-lambda-call                      "lambda-call")
(defvar color-inlining                         "inlined")
(defvar color-slot-accessor-fixed-offset       "accessor-fixed-offset")
(defvar color-eliminated                       "eliminated")
(defvar color-dynamic-extent                   "dynamic-extent")
(defvar color-program-notes                    "program-note")
(defvar color-background                       "background")
(defvar color-bogus-upgrade                    "bogus-upgrade")

; major problem: if one coloring of the same type is contained in another, you can't recognize the inner one - make the color lighter/darker/a white background at the borders? any good ideas here?

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
  `((t (:background "dimgray"))) ; should be darkgrey...
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-slot-accessor-fixed-offset
  `((t (:background "forestgreen")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-eliminated
  `((t (:background "pink"))) ; should be lightgrey according to documentation
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-dynamic-extent
  `((t (:background "DarkTurquoise"))) ; no documentation for this :/
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-program-notes
  `((t (:background "yellow")))
  "Bla bla bla"
  :group 'slime-mode-faces)
(defface face-bogus-upgrade
  `((t (:background "orange"))) ; no documentation for that - and according to source only relevant for dylan library?
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
              (overlay (overlays-at (point))))
          (goto-char 1) (forward-line (1- el)) (forward-char ec)
          (let ((end (point)))
            (if overlay
                (let ((oldstart (overlay-start (car overlay)))
                      (oldend (overlay-end (car overlay)))
                      (oldface (overlay-get (car overlay) 'face)))
                  (delete-overlay (car overlay))
                  (let ((over1 (make-overlay oldstart start))
                        (over2 (make-overlay end oldend)))
                    (overlay-put over1 'face oldface)
                    (overlay-put over2 'face oldface))))
            (let ((over (make-overlay start end)))
              (if (string= color "not-all-known")
                  (overlay-put over 'face 'face-not-all-methods-known)
                (if (string= color "failed-to-select")
                    (overlay-put over 'face 'face-failed-to-select-where-all-known)
                  (if (string= color "lambda-call")
                      (overlay-put over 'face 'face-lambda-call)
                    (if (string= color "inlined")
                      (overlay-put over 'face 'face-inlining)
                      (if (string= color "accessor-fixed-offset")
                          (overlay-put over 'face 'face-slot-accessor-fixed-offset)
                        (if (string= color "eliminated")
                            (overlay-put over 'face 'face-eliminated)
                          (if (string= color "dynamic-extent")
                              (overlay-put over 'face 'face-dynamic-extent)
                            (if (string= color "program-note")
                                (overlay-put over 'face 'face-program-notes)
                              (if (string= color "bogus-upgrade")
                                  (overlay-put over 'face 'face-bogus-upgrade))))))))))))))
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
    (message "Using color file: %s" file)
    (load-file file)
    (message "Used color file: %s" file)))

(if dylan-mode-map
    (progn
      (define-key dylan-mode-map [menu-bar dylan-misc dylan-color-optimizations] '("Color Optimizations" . color-optimizations))))

;; eof
