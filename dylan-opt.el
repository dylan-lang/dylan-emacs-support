;; Interface to optimization coloring.

;; TODO: major problem: if one coloring of the same type is contained
;; in another, you can't recognize the inner one - make the color
;; lighter/darker/a white background at the borders? any good ideas
;; here?

(require 'dylan-mode)

(defgroup dylan-opt nil
  "Major mode for editing Dylan source code."
  :group 'dylan)

(defface dylan-opt-face-not-all-methods-known
  `((t (:background "magenta")))
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-failed-to-select-where-all-known
  `((t (:background "red")))
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-lambda-call
  `((t (:background "lightskyblue")))
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-inlining
  `((t (:background "dimgray"))) ; should be darkgrey...
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-slot-accessor-fixed-offset
  `((t (:background "forestgreen")))
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-eliminated
  `((t (:background "pink"))) ; should be lightgrey according to documentation
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-dynamic-extent
  `((t (:background "DarkTurquoise"))) ; no documentation for this :/
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-program-notes
  `((t (:background "yellow")))
  "Bla bla bla"
  :group 'dylan-opt)

(defface dylan-opt-face-bogus-upgrade
  `((t (:background "orange"))) ; no documentation for that - and according to source only relevant for dylan library?
  "Bla bla bla"
  :group 'dylan-opt)

(defvar-local dylan-opt--overlays '()
  "Highlighting overlays of current buffer.")

(defun dylan-opt--add-overlays (which specs)
  (save-excursion
    (dolist (spec specs)
      (let* ((sl (car spec))
             (sc (car (cdr spec)))
             (el (car (cdr (cdr spec))))
             (ec (car (cdr (cdr (cdr spec))))))
        (goto-char 1)
        (forward-line (1- sl))
        (forward-char sc)
        (let ((start (point))
              (overlay (overlays-at (point))))
          (goto-char 1)
          (forward-line (1- el))
          (forward-char ec)
          (let ((end (point)))
            (when overlay
              (let ((oldstart (overlay-start (car overlay)))
                    (oldend (overlay-end (car overlay)))
                    (oldface (overlay-get (car overlay) 'face))
                    (oldmsg (overlay-get (car overlay) 'help-echo)))
                (delete-overlay (car overlay))
                (setq dylan-opt--overlays
                      (remove (car overlay) dylan-opt--overlays))
                (let ((over1 (make-overlay oldstart start))
                      (over2 (make-overlay end oldend)))
                  (overlay-put over1 'face oldface)
                  (overlay-put over2 'face oldface)
                  (overlay-put over1 'help-echo oldmsg)
                  (overlay-put over2 'help-echo oldmsg)
                  (push over1 dylan-opt--overlays)
                  (push over2 dylan-opt--overlays))))
            (let ((over (make-overlay start end)))
              (cond ((string= which "not-all-known")
                     (overlay-put over 'face
                                  'dylan-opt-face-not-all-methods-known))
                    ((string= which "failed-to-select")
                     (overlay-put
                      over 'face
                      'dylan-opt-face-failed-to-select-where-all-known))
                    ((string= which "lambda-call")
                     (overlay-put over 'face 'dylan-opt-face-lambda-call))
                    ((string= which "inlined")
                     (overlay-put over 'face 'dylan-opt-face-inlining))
                    ((string= which "accessor-fixed-offset")
                     (overlay-put over 'face
                                  'dylan-opt-face-slot-accessor-fixed-offset))
                    ((string= which "eliminated")
                     (overlay-put over 'face 'dylan-opt-face-eliminated))
                    ((string= which "dynamic-extent")
                     (overlay-put over 'face 'dylan-opt-face-dynamic-extent))
                    ((string= which "program-note")
                     (overlay-put over 'face 'dylan-opt-face-program-notes))
                    ((string= which "bogus-upgrade")
                     (overlay-put over 'face 'dylan-opt-face-bogus-upgrade)))
              (overlay-put over 'help-echo which)
              (push over dylan-opt--overlays))))))))

(defun dylan-opt--remove-overlays ()
  "Uncolor the current Dylan buffer of optimization information"
  (mapc #'delete-overlay dylan-opt--overlays)
  (setq dylan-opt--overlays '()))

(defun dylan-opt--default-file-name ()
  (let* ((path (buffer-file-name))
         (name (file-name-nondirectory path))
         (stem (substring name 0 (string-match "\\.[^.]*$" name)))
         (library dylan-buffer-library))
    (expand-file-name
     (concat (or (getenv "OPEN_DYLAN_USER_ROOT") "_build")
             "/build/" library "/" stem ".el"))))

(defun dylan-opt-from-file (file)
  "Color the current Dylan buffer with recorded optimization information"
  (interactive (list
                (let ((opt-file (dylan-opt--default-file-name)))
                  (read-file-name "Color optimization file: "
                                  (file-name-directory opt-file)
                                  nil
                                  t
                                  (file-name-nondirectory opt-file)
                                  (lambda (x) (string-match ".*\\.el" x))))))
  (message "Using color file: %s" file)
  (dylan-opt--remove-overlays)
  (load-file file)
  (message "Used color file: %s" file))

(provide 'dylan-opt)
