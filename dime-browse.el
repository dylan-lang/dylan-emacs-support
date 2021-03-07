;;; dime-browse.el --- Dylan interaction mode -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.1"))
;; URL: https://opendylan.org/
;; Author: Rui Patrocínio <rui.patrocinio@netvisao.pt>
;; Author: Hannes Mehnert <hannes@opendylan.org>
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Dylan class browser for Dime.

;;; Code:

;; Originally adapted from `slime-xref-browser.el'.

(require 'tree-widget)

(require 'dime)

(defun dime-browse--expand-subclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
        (loop for kid in (dime-eval `(swank:dylan-subclasses ,name))
              collect `(tree-widget
                        :tag ,kid
                        :expander dime-browse--expand-subclass-node
                        :has-children t)))))

(defun dime-browse--expand-superclass-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
        (loop for kid in (dime-eval `(swank:dylan-superclasses ,name))
              collect `(tree-widget
                        :tag ,kid
                        :expander dime-browse--expand-superclass-node
                        :has-children t)))))

(defvar dime-browse-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap for tree widget browsers")

(defun dime-browse--with-expander (name expander)
  (let ((project (dime-current-project))
        (module  dylan-buffer-module))
    (switch-to-buffer "*Dime class browser*")
    (kill-all-local-variables)
    (setq dime-buffer-project project)
    (setq dylan-buffer-module module)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (widget-insert "Class Browser" "\n\n")
      (save-excursion
        (widget-create 'tree-widget
                       :tag name
                       :expander expander
                       :has-echildren t)))
    (lisp-mode-variables t)
    (dime-mode t)
    (use-local-map dime-browse-map)
    (widget-setup)))

(defun dime-browse-subclasses (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-browse--with-expander name 'dime-browse--expand-subclass-node))

(defun dime-browse-superclasses (name)
  "Read the name of a class and show its superclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-browse--with-expander name 'dime-browse--expand-superclass-node))

(provide 'dime-browse)

;;; dime-browse.el ends here
