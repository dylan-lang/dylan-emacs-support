;;; dime-compat.el --- compatibility functions for DIME in Emacs 24.1

;; Author: Tom Emerson (tremerson@gmail.com)

(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(dolist (funcs '
          ((cl-assoc . assoc*)
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

;; (unless (fboundp 'cl-find-if)
;;   (defalias 'cl-find-if 'find-if))

;; (unless (fboundp 'cl-defmacro)
;;   (defalias 'cl-defmacro 'defmacro*))

;; (unless (fboundp 'cl-defun)
;;   (defalias 'cl-defun 'defun*))

;; (unless (fboundp 'cl-flet)
;;   (defalias 'cl-flet 'flet))

;; (unless (fboundp 'cl-sort)
;;   (defalias 'cl-sort 'sort*))

;; (unless (fboundp 'cl-remove)
;;   (defalias 'cl-remove 'remove*))


(provide 'dime-compat)
