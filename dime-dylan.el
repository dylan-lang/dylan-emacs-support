(require 'dime)

;;;; Arglist Display

(defun dime-dylan-arglist-magic (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (dime-background-activities-enabled-p)
    (dime-dylan-show-arglist)))

(defun dime-dylan-show-arglist ()
  (let ((op (dime-dylan-operator-before-point)))
    (when op
      (dime-eval-async `(swank:operator-arglist ,op ,(dime-current-project))
                       (lambda (arglist)
                         (when arglist
                           (dime-message "%s" arglist)))))))

(defun dime-dylan-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (backward-sexp 1)
      (thing-at-point 'dime-symbol))))

(defun dime-dylan-init ()
  (add-hook 'dylan-mode-hook 'dime-dylan-bind-keys))

(defun dime-dylan-bind-keys ()
  (define-key dime-mode-map (kbd "SPC") 'dime-dylan-arglist-magic)
  (local-set-key (kbd ",") 'dime-dylan-arglist-magic)
  (local-set-key (kbd "(") 'dime-dylan-arglist-magic))


;;; Source modified from slime-xref-browser.el
;;; slime-xref-browser.el --- xref browsing with tree-widget
;;
;; Author: Rui Patrocínio <rui.patrocinio@netvisao.pt>
;; Licencse: GNU GPL (same license as Emacs)
;; Modified by Hannes Mehnert <hannes@opendylan.org>

(defun dime-expand-subclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (dime-eval `(swank:dylan-subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander dime-expand-subclass-node
				    :has-children t)))))

(defun dime-expand-superclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (dime-eval `(swank:dylan-superclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander dime-expand-superclass-node
				    :has-children t)))))

(defun dime-dylan-browse-subclasses (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-call-with-browser-setup
   "*dime class browser*" (dime-current-project) dylan-buffer-module "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name
                    :expander 'dime-expand-subclass-node
                    :has-echildren t))))

(defun dime-dylan-browse-superclasses (name)
  "Read the name of a class and show its superclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-call-with-browser-setup
   "*dime class browser*" (dime-current-project) dylan-buffer-module "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name
                    :expander 'dime-expand-superclass-node
                    :has-echildren t))))

(defvar dime-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless dime-browser-map
  (setq dime-browser-map (make-sparse-keymap))
  (set-keymap-parent dime-browser-map widget-keymap)
  (define-key dime-browser-map "q" 'bury-buffer))

(defun dime-call-with-browser-setup (buffer project module title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq dime-buffer-project project)
  (setq dylan-buffer-module module)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (dime-mode t)
  (use-local-map dime-browser-map)
  (widget-setup))

(provide 'dime-dylan)

