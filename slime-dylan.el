;;;; Arglist Display

(defun slime-dylan-arglist-magic (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (slime-background-activities-enabled-p)
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
      (slime-symbol-at-point))))

(defun slime-dylan-init ()
  (add-hook 'dylan-mode-hook 'slime-dylan-bind-keys))

(defun slime-dylan-bind-keys ()
  (define-key slime-mode-map (kbd "SPC") 'slime-dylan-arglist-magic)
  (local-set-key (kbd ",") 'slime-dylan-arglist-magic)
  (local-set-key (kbd "(") 'slime-dylan-arglist-magic))


;;; Source modified from slime-xref-browser.el
;;; slime-xref-browser.el --- xref browsing with tree-widget
;;
;; Author: Rui Patrocínio <rui.patrocinio@netvisao.pt>
;; Licencse: GNU GPL (same license as Emacs)
;; Modified by Hannes Mehnert <hannes@opendylan.org>

(defun slime-expand-subclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (slime-eval `(swank:dylan-subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander slime-expand-subclass-node
				    :has-children t)))))

(defun slime-expand-superclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (slime-eval `(swank:dylan-superclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander slime-expand-superclass-node
				    :has-children t)))))

(defun slime-dylan-browse-subclasses (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (slime-read-symbol-name "Class Name: ")))
  (slime-call-with-browser-setup 
   "*slime class browser*" (slime-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :expander 'slime-expand-subclass-node 
                    :has-echildren t))))

(defun slime-dylan-browse-superclasses (name)
  "Read the name of a class and show its superclasses."
  (interactive (list (slime-read-symbol-name "Class Name: ")))
  (slime-call-with-browser-setup 
   "*slime class browser*" (slime-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :expander 'slime-expand-superclass-node 
                    :has-echildren t))))

(defvar slime-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless slime-browser-map
  (setq slime-browser-map (make-sparse-keymap))
  (set-keymap-parent slime-browser-map widget-keymap)
  (define-key slime-browser-map "q" 'bury-buffer))

(defun slime-call-with-browser-setup (buffer package title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq slime-buffer-package package)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (slime-mode t)
  (use-local-map slime-browser-map)
  (widget-setup))

(provide 'slime-dylan)

