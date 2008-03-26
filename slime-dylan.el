;;;; Arglist Display

(defun slime-dylan-arglist-magic (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (and slime-space-information-p
             (slime-background-activities-enabled-p))
    (slime-dylan-show-arglist)))

(defun slime-dylan-show-arglist ()
  (let ((op (slime-dylan-operator-before-point)))
    (when op 
      (slime-eval-async `(swank:operator-arglist ,op ,(slime-current-package))
			(lambda (arglist)
			  (when arglist
			    (slime-message "%s" arglist)))))))

(defun slime-dylan-operator-before-point ()
  (ignore-errors 
    (save-excursion
      (backward-up-list 1)
      (backward-sexp 1)
      (slime-symbol-name-at-point))))

(defun slime-dylan-init ()
  (add-hook 'dylan-mode-hook 'slime-dylan-bind-keys))

(defun slime-dylan-bind-keys ()
  (define-key slime-mode-map (kbd "SPC") 'slime-dylan-arglist-magic)
  (local-set-key (kbd ",") 'slime-dylan-arglist-magic)
  (local-set-key (kbd "(") 'slime-dylan-arglist-magic))

(provide 'slime-dylan)