;;; dime-compat.el --- compatibility functions for DIME in Emacs 24.1

;; Author: Tom Emerson (tremerson@gmail.com)
;; No copyright. Do whatever you want with it.

;; This file defines compatibility mappings between cl-* functions that
;; were introduced in Emacs 24.3. When required in dime.el these allow
;; dylan-mode and DIME to load/compile in Emacs 24.1, but there are
;; still issues with the dswank communication.

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(dolist (funcs '((cl-assoc . assoc*)
                 (cl-copy-list . copy-list)
                 (cl-defmacro . defmacro*)
                 (cl-defstruct . defstruct)
                 (cl-defun . defun*)
                 (cl-every . every)
                 (cl-find . find)
                 (cl-find-if . find-if)
                 (cl-flet . flet)
                 (cl-gensym . gensym)
                 (cl-getf . getf)
                 (cl-mapcan . mapcan)
                 (cl-position . position)
                 (cl-reduce . reduce)
                 (cl-remove . remove*)
                 (cl-remove-if . remove-if)
                 (cl-remove-if-not . remove-if-not)
                 (cl-sort . sort*)
                 (cl-subseq . subseq)))
  (unless (fboundp (car funcs))
    (defalias (car funcs) (cdr funcs))))


(provide 'dime-compat)
