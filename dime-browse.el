;;; dime-browse.el --- Dylan interaction mode -*- lexical-binding: t -*-

;; Lineage: SLIME, Open Dylan

;; Copyright (C) 20?? Rui Patrocínio (SLIME)
;; Copyright (C) 2011, 2012 Hannes Mehnert
;; Copyright (C) 2013 Erik Charlebois
;; Copyright (C) 2021 Lassi Kortela
;; SPDX-License-Identifier: GPL-2.0-or-later

;; URL: https://opendylan.org/

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Dylan class browser for Dime.

;;; Code:

;; Originally adapted from `slime-xref-browser.el'.

(require 'cl-lib)
(require 'tree-widget)

(require 'dime)

(defun dime-browse--expand-node (expander swank widget)
  "Helper to expand a node in WIDGET using EXPANDER and SWANK command."
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
        (cl-loop for kid in (dime-eval `(,swank ,name))
                 collect `(tree-widget
                           :tag ,kid
                           :expander ',expander
                           :has-children t)))))

(defun dime-browse--expand-subclass-node (widget)
  "Helper to expand a subclass node in WIDGET."
  (dime-browse--expand-node
   'dime-browse--expand-subclass-node
   'swank:dylan-subclasses
   widget))

(defun dime-browse--expand-superclass-node (widget)
  "Helper to expand a superclass node in WIDGET."
  (dime-browse--expand-node
   'dime-browse--expand-superclass-node
   'swank:dylan-superclasses
   widget))

(defvar dime-browse-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap for the Dime class browser.")

(defun dime-browse--with-expander (name expander)
  "Internal function to browse class NAME using EXPANDER."
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

;;;###autoload
(defun dime-browse-subclasses (name)
  "Read the NAME of a class and show its subclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-browse--with-expander name 'dime-browse--expand-subclass-node))

;;;###autoload
(defun dime-browse-superclasses (name)
  "Read the NAME of a class and show its superclasses."
  (interactive (list (dime-read-symbol-name "Class Name: ")))
  (dime-browse--with-expander name 'dime-browse--expand-superclass-node))

(provide 'dime-browse)

;;; dime-browse.el ends here
