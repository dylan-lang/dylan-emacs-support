;;; dime-dylan.el --- Dylan interaction mode -*- lexical-binding: t -*-

;; URL: https://opendylan.org/
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Support code for Dime.

;;; Code:

(require 'dime)


;;; Source modified from slime-xref-browser.el
;;; slime-xref-browser.el --- xref browsing with tree-widget
;;
;; Author: Rui Patrocínio <rui.patrocinio@netvisao.pt>
;; Licencse: GNU GPL (same license as Emacs)
;; Modified by Hannes Mehnert <hannes@opendylan.org>

(defun dime-dylan-expand-subclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (dime-eval `(swank:dylan-subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander dime-dylan-expand-subclass-node
				    :has-children t)))))

(defun dime-dylan-expand-superclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (dime-eval `(swank:dylan-superclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander dime-dylan-expand-superclass-node
				    :has-children t)))))

(defun dime-dylan-browse-subclasses (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-dylan-call-with-browser-setup
   "*dime class browser*" (dime-current-project) dylan-buffer-module "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name
                    :expander 'dime-dylan-expand-subclass-node
                    :has-echildren t))))

(defun dime-dylan-browse-superclasses (name)
  "Read the name of a class and show its superclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-dylan-call-with-browser-setup
   "*dime class browser*" (dime-current-project) dylan-buffer-module "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name
                    :expander 'dime-dylan-expand-superclass-node
                    :has-echildren t))))

(defvar dime-dylan-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless dime-dylan-browser-map
  (setq dime-dylan-browser-map (make-sparse-keymap))
  (set-keymap-parent dime-dylan-browser-map widget-keymap)
  (define-key dime-dylan-browser-map "q" 'bury-buffer))

(defun dime-dylan-call-with-browser-setup (buffer project module title fn)
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
  (use-local-map dime-dylan-browser-map)
  (widget-setup))

(provide 'dime-dylan)

;;; dime-dylan.el ends here
