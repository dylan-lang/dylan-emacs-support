;;
;;	$RCSfile: dylan-hilit19.el,v $
;;	$Revision: 1.1 $ 
;;	$State: Exp $
;;	$Date: 1998/12/02 03:50:50 $
;;	$Author: andreas $
;;	$Source: /scm/cvs/src/tools/elisp/dylan-hilit19.el,v $
;;	$Locker:  $
;;
;; dylan-hilit19.el -- hilit19 mode patterns for Dylan.
;; Copyright (c) 1995 Joseph N. Wilson
;; Copyright (c) NDC Systems 1995
;; Author: Joseph Wilson (jnw@cis.ufl.edu)
;; Author: David Lim (david@ndc.com)
;;
;; LCD Archive Entry:
;; dylan-hilit19|David Lim|david@ndc.com|
;; hilit19 mode patterns for Dylan.|
;; 14-Jun-1995|1.1|~/modes/dylan-hilit19.el.Z|
;;
;; This software is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;; 
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for more details.
;; 
;; You should have received a copy of the GNU Library General Public
;; License along with this software; if not, write to the Fre
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:
;;
;; You may want to load this as part of your Dylan mode hook
;; by putting something like the following in your .emacs:
;;
;;	(defvar dylan-mode-hook
;;	  (lambda ()
;;	    (progn
;;	      (load "dylan-hilit19.el"))))
;;
;; You also need to have dylan-mode.el from CMU
;; 

(hilit-set-mode-patterns
 'dylan-mode
 '(
   ;; available faces are in the constant hilit-default-face-table
   ;; comments
   ("/\\*" "\\*/" comment)		; /* C-like comment
   ("//.*$" nil comment)		; // C++ like comment

   ;; definitions - must come before regexp that includes "method"
   ;; originally taken from dylan-mode.el - font-lock stuff, 
   ;; some modifications made
   ("\\<\\(define\\([ \t\n]+\\(open\\|sealed\\|abstract\\|concrete\\|primary\\)\\)*[ \t]+\\(class\\|method\\|generic\\|variable\\|constant\\|library\\|module\\|macro\\)\\)\\>[ \t]+[^ \t(]*[ \t(]+" nil defun)

   ;; patterns that signal the start of a nested body
   ;; must not have a : after the word
   ("[ \t\n]\\(local\\s-*\\(method\\)?\\|method\\|if\\|block\\|begin\\|case\\|for\\|select\\|unless\\|until\\|while\\)\\>[^:]" nil keyword)
   ;; other keywords
   ;; must not have a : after the word
   ("[ \t\n]+\\(finally\\|else\\|elseif\\|let\\s-*\\(handler\\)?\\|otherwise\\|then\\|in\\|from\\|to\\|above\\|below\\|by\\|cleanup\\|exception\\|singleton\\|end\\)\\>[^:]" nil keyword)
   ;; module keywords
   ("[ \t\n]+\\(import\\|rename\\|export\\|use\\|create\\|all\\)\\>[^:]" nil keyword)
   ;; headers
   ("^[a-zA-z][a-zA-Z0-9-]+:" nil include)

   ;; slots
   ;; I am not sure where the inherited keyword should go. I made it an
   ;; adjective. Can you have an "sealed constant inherited slot foo" ?
   ("\\([ \t]+\\(sealed\\|open\\|inherited\\)*\\)?\\([ \t]+\\(instance\\|class\\|each-subclass\\|constant\\|virtual\\)*\\)?[ \t]+slot[ \t]+" nil keyword)

   ;; from jnw@aviator.cis.ufl.edu (Joseph N. Wilson)
   ;; anything enclosed in <> - types
   ("\\(<[-a-zA-Z\\!\\&\\*\\=\\|\\^\\$\\%\\@\\_]*>\\)" nil type)
   ;; =>
   ("[ \t\n]=>[ \t\n]" nil struct)
   ;; words that end with : - i.e. keywords
   ;; I do not like to highlight these keywords, so I have commented this out
   ("\\(^\\|[ \t]\\)[-a-zA-Z\\!\\&\\*\\=\\|\\^\\$\\%\\@\\_]+:" nil keyword)
   ;; words that start with #
   ("#[-a-zA-Z\\!\\&\\*\\=\\|\\^\\$\\%\\@\\_\"]+" nil keyword)

   ;; strings - backslash or quote?
   ;; interned symbols, e.g. #"test" will be highlighted as a string
   (hilit-string-find ?' string)
   )
 )
	 
