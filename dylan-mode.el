;;; dylan-mode.el --- Major mode for editing Dylan programs.

;; Copyright (C) 1994, 1995, 1996  Carnegie Mellon University
;; Copyright (C) 2004, 2005, 2007  Chris Page

;; Author: Robert Stockton (rgs@cs.cmu.edu), others, then Chris Page
;; Maintainer: Chris Page <cpage@opendylan.org>
;; Version: 1.22

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to "gwydion-bugs@cs.cmu.edu")
;; or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:

;; Dylan mode is a major mode for editing Dylan programs. It provides
;; indenting and font-lock support.
;;
;; This code requires Emacs 24.


;;; Code:

(defconst dylan-version "1.22"
  "Dylan Mode version number.")

(defun dylan-version ()
  "Return string describing the version of Dylan mode.
When called interactively, displays the version."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message (dylan-version))
    (format "Dylan Mode version %s" dylan-version)))


;;; Customization:

(defgroup dylan nil
  "Major mode for editing Dylan source."
  :group 'languages)

(defcustom dylan-indent 2
  "*Number of spaces to indent each sub-block."
  :type  'integer
  :group 'dylan)

(defcustom dylan-continuation-indent 2
  "*Number of spaces to indent each continued line."
  :type  'integer
  :group 'dylan)

(defcustom dylan-highlight-function-calls nil
  "*Whether to highlight function calls in Font Lock mode.
Applies only in Font Lock decoration level 2 or higher.

This uses a very simple regular expression that highlights just
about anything adjacent to a left-parenthesis."
  :type  'boolean
  :group 'dylan)


;;; Faces:

(defface dylan-header-background
  '((t :inherit header))
  "Background face for Dylan interchange file headers.

This is designed to apply background attributes to the entire
header, with other faces applied on top."
  :group 'dylan)

(defface dylan-header-separator
  '((t))
  "Face for the last line of Dylan interchange file headers."
  :group 'dylan)

(defface dylan-header-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for Dylan interchange file header keywords."
  :group 'dylan)

(defface dylan-header-value
  '((t))
  "Face for Dylan interchange file header values."
  :group 'dylan)

(defface dylan-header-module-name
  '((t :inherit font-lock-function-name-face))
  "Face for the `module:' name in Dylan interchange file headers."
  :group 'dylan)

(defface dylan-header-error
  '((t :inherit font-lock-warning-face))
  "Face for invalid lines in Dylan interchange file headers.

Valid lines begin with a keyword or a value continuation
whitespace prefix."
  :group 'dylan)


;;; Regular expressions:

(defvar dylan-unnamed-definition-words
  '(;; Melange/C-FFI
    "interface")
  "Words that introduce unnamed definitions like \"define interface\".")

(defvar dylan-named-definition-words
  '(;; Dylan
    "module" "library" "macro"
    ;; C-FFI
    "C-struct" "C-union" "C-function"
    "C-callable-wrapper")
  "Words that introduce simple named definitions like \"define library\".")

(defvar dylan-type-parameterized-definition-words
  '(;; Dylan
    "class"
    ;; C-FFI
    "C-subtype" "C-mapped-subtype")
  "Words that introduce type definitions like \"define class\". These are
also parameterized like \"define method\" and are appended to
`dylan-other-parameterized-definition-words'.")

(defvar dylan-other-parameterized-definition-words
  '(;; Dylan
    "method" "function"
    ;; C-FFI
    "C-variable" "C-address")
  "Words that introduce trickier definitions like \"define method\". These
require special definitions to be added to `dylan-start-expressions'.")

(defvar dylan-constant-simple-definition-words
  '(;; Dylan
    "constant")
  "Words that introduce module constant definitions. These must also be
simple definitions and are appended to `dylan-other-simple-definition-words'.")

(defvar dylan-variable-simple-definition-words
  '(;; Dylan
    "variable")
  "Words that introduce module variable definitions. These must also be
simple definitions and are appended to `dylan-other-simple-definition-words'.")

(defvar dylan-other-simple-definition-words
  '(;; Dylan
    "generic" "domain"
    ;; C-FFI
    "C-pointer-type"
    ;; Extensions
    "table")
  "Other words that introduce simple definitions (without implicit bodies).")

(defvar dylan-statement-words
  '(;; Dylan
    "if" "block" "begin" "method" "case" "for" "select" "when" "unless"
    "until" "while"
    ;; Extensions
    "iterate" "profiling")
  "Words that begin statements with implicit bodies.")

;; Names beginning "with-" and "without-" are commonly used as statement macros.
(defvar dylan-with-statement-prefix "with\\(out\\)\\{0,1\\}-")
(defvar dylan-statement-prefixes
  (concat "\\|\\_<" dylan-with-statement-prefix "[-_a-zA-Z?!*@<>$%]+"))

(defvar dylan-separator-words
  '("finally" "exception" "cleanup" "else" "elseif" "afterwards")
  "These are the patterns that act as separators in compound statements. This
may include any general pattern that must be indented specially.")

(defvar dylan-other-words
  '(;; Dylan
    "above" "below" "by" "from"
    "handler" "in" "instance\\?" "let" "local" "otherwise"
    "slot" "subclass" "then" "to"
    ;; Extensions
    "keyed-by" "virtual")
  "Keywords that do not require special indentation handling, but which
should be highlighted by font-lock.")

(defvar dylan-comment-pattern "//.*$"
  "Internal pattern for finding comments in Dylan code. Currently only
handles end-of-line comments.")

(defun dylan-make-pattern (start &rest list)
  "Build a search pattern that matches any of the patterns passed to it.
Makes sure that it doesn't match partial words."
  (let ((str (concat "\\_<" start "\\_>")))
    (while list
      (setq str (concat str "\\|\\_<" (car list) "\\_>"))
      (setq list (cdr list)))
    str))

(defvar dylan-start-expressions '()
  "Patterns that match that portion of a compound statement that
precedes the body. This is used to determine where the first
statement begins for indentation purposes.

Contains a list of patterns, each of which is either a regular
expression or a list of regular expressions. A set of balanced
parens will be matched between each list element.")

(defvar dylan-font-lock-header-keywords
  ;; Many of these regexp patterns are order-dependent, assuming the preceding
  ;; patterns have already been matched and fontified as appropriate, preventing
  ;; following patterns from being used to fontify the same text.
  ;;
  ;; Most of these patterns match up to the end of buffer, so that highlighting
  ;; occurs while entering header text in a new Dylan file. Notably, the pattern
  ;; for invalid header lines does not, so that it doesn't mark incomplete lines
  ;; as invalid while the user is still entering them. (It does mark even
  ;; temporarily invalid lines that aren't at the end of buffer, though.)
  `(
    ;; The "module:" header line. Highlight the module name.
    (,(concat "^"
	      "module:"			; keyword
	      "[ \t]*"			; space
	      "\\(\\("
	      "[-_a-zA-Z0-9?!*@<>$%]+"	; module name...
	      "\\)\\|"
	      "[^ \t\n][^\n]*?"		; ...or invalid value
	      "\\)"
	      "[ \t]*\\(\n\\|\\'\\)")	; tail space
     (1 (if (match-beginning 2)
	    'dylan-header-module-name
	  'dylan-header-error)))

    ;; The "language:" header line. Highlight the language name. This is a bit
    ;; of pedantry on my part -- this header is rarely used, except perhaps in
    ;; very old files -- so I'm just using the same face as for the module name,
    ;; rather than defining a separate face (or renaming the module name face to
    ;; be more generic). "infix-dylan" is the only portable value, so let's warn
    ;; about other values.
    (,(concat "^"
	      "language:"		; keyword
	      "[ \t]*"			; space
	      "\\(\\("
	      "infix-dylan"		; language name...
	      "\\)\\|"
	      "[^ \t\n][^\n]*?"		; ...or invalid value
	      "\\)"
	      "[ \t]*\\(\n\\|\\'\\)")	; tail space
     (1 (if (match-beginning 2)
	    'dylan-header-module-name
	  'dylan-header-error)))

    ;; Header lines with keywords, and lines with value continuations.
    (,(concat "^"
	      "\\(?:\\("
	      "[a-zA-Z][-a-zA-Z0-9]*:"	; keyword...
	      "\\)\\|"
	      "[ \t]"			; ...or continuation prefix
	      "\\)"
	      "[ \t]*"			; space
	      "\\("
	      ;; Can keyword lines have empty values?
	      "[^ \t\n][^\n]*?"		; value
	      "\\)"
	      "[ \t]*\\(\n\\|\\'\\)")	; tail space
     (1 'dylan-header-keyword nil t)
     (2 'dylan-header-value))

    ;; Invalid header lines. This pattern assumes we've already tried the
    ;; pattern for header lines with keywords and it didn't match.
    ;;
    ;; Note: Ideally, we'd mark any subsequent continuation lines invalid,
    ;; too. Look into a way to do that.
    (,(concat "^"
	      "[^ \t\n]"		; any invalid prefix character
	      "[^\n]*\n")		; rest of line
     . 'dylan-header-error)

    ;; Mark all lines in the header with the header background face (except for
    ;; the final, blank line).
    (,(concat "^"
	      "[ \t]*"			; possible continuation prefix
	      "[^ \t\n]+"		; any non-whitespace in line
	      "[^\n]*\n")		; rest of line
     (0 'dylan-header-background append))

    ;; Mark the final, blank line with the header separator face.
    (,(concat "^"
	      "[ \t]*\n")		; tail space
     . 'dylan-header-separator))
 "Value to which `font-lock-keywords' should be set when
fontifying Dylan interchange file headers in Dylan Mode.")

(defvar dylan-font-lock-keywords nil
  "Value to which `font-lock-keywords' should be set when in
Dylan Mode, for Font Lock decoration level 0.")
(defvar dylan-font-lock-keywords-1 nil
  "Value to which `font-lock-keywords' should be set when in
Dylan Mode, for Font Lock decoration level 1.")
(defvar dylan-font-lock-keywords-2 nil
  "Value to which `font-lock-keywords' should be set when in
Dylan Mode, for Font Lock decoration level 2.")

;; Regexp pattern that matches `define' and adjectives. A sub-pattern designed
;; to be followed by patterns that match the define word or other parts of the
;; definition macro call.
(defconst dylan-define-pattern "define\\([ \t]+\\w+\\)*[ \t]+")

(defvar dylan-other-definition-words
  (append dylan-unnamed-definition-words
          dylan-named-definition-words
          dylan-other-parameterized-definition-words))

(defvar dylan-definition-words
  (append dylan-type-parameterized-definition-words
          dylan-other-definition-words))

(defvar dylan-type-definition-pattern
  (regexp-opt dylan-type-parameterized-definition-words 'symbols))

(defvar dylan-other-definition-pattern
  (regexp-opt dylan-other-definition-words 'symbols))

(defvar dylan-definition-pattern
  (regexp-opt dylan-definition-words 'symbols))

(defvar dylan-named-definition-pattern
  (regexp-opt dylan-named-definition-words 'symbols))

(defvar dylan-unnamed-definition-pattern
  (regexp-opt dylan-unnamed-definition-words 'symbols))

(defvar dylan-type-parameterized-definition-pattern
  (regexp-opt dylan-type-parameterized-definition-words 'symbols))

(defvar dylan-parameterized-definition-pattern
  (regexp-opt (append dylan-type-parameterized-definition-words
                      dylan-other-parameterized-definition-words) 'symbols))

(defvar dylan-separator-word-pattern
  (regexp-opt dylan-separator-words 'symbols)
  ;; Try to find a way to make these context-sensitive, so they are only
  ;; highlighted within the appropriate statements. Unfortunately, doing this
  ;; robustly may require knowing the syntax of all statement macros and parsing
  ;; them so we don't highlight separators when they're within nested
  ;; statements. On the other hand, we may get a lot of mileage out of just
  ;; handling all the iteration words in for statements.
  "Separator words in statement macros.")

(defvar dylan-constant-simple-definition-pattern
  (regexp-opt dylan-constant-simple-definition-words 'symbols))

(defvar dylan-variable-simple-definition-pattern
  (regexp-opt dylan-variable-simple-definition-words 'symbols))

(defvar dylan-other-simple-definition-pattern
  (regexp-opt dylan-other-simple-definition-words 'symbols))

(defvar dylan-simple-definition-pattern
  (regexp-opt (append dylan-constant-simple-definition-words
                     dylan-variable-simple-definition-words
                     dylan-other-simple-definition-words) 'symbols))

(defvar dylan-end-keyword-pattern nil)
(defvar dylan-other-pattern nil)
(defvar dylan-keyword-pattern nil)
(defvar dylan-find-keyword-pattern nil)
(defvar dylan-beginning-of-form-pattern nil)

(defun dylan-mode-init-keyword-patterns ()
  "Construct Dylan Mode keyword patterns (used for indenting and font-lock),
using the values of the various keyword list variables."
  ;; Define regular expression patterns using the word lists.
  (setq dylan-keyword-pattern
	;; We disallow newlines in "define foo" patterns because it allows the
	;; actual keyword to be confused for a qualifier if another definition
	;; follows closely.
	(concat
	 (apply 'dylan-make-pattern
		(concat dylan-define-pattern dylan-definition-pattern)
		dylan-statement-words)
	 dylan-statement-prefixes))
  (setq dylan-end-keyword-pattern
	;; We intentionally disallow newlines in "end foo" constructs, because
	;; doing so makes it very difficult to deal with the keyword "end" in
	;; comments.
	(concat "\\_<end\\_>[ \t]*\\("
		(apply 'dylan-make-pattern
		       (append dylan-definition-words dylan-statement-words))
		dylan-statement-prefixes
		"\\)?"))
  (setq dylan-other-pattern
	(apply 'dylan-make-pattern
	       (concat "define\\([ \t\n]+\\w+\\)*[ \t\n]+"
		       dylan-simple-definition-pattern)
	       dylan-other-words))
  (setq dylan-start-expressions
	;; cpage 2007-04-06: Why are these listed here? Shouldn't we build these
	;; patterns from dylan-statement-words?
	`(("if[ \t\n]*" "")
	  ("block[ \t\n]*" "")
	  ("for[ \t\n]*" "")
	  ("select[ \t\n]*" "")
	  ("when[ \t\n]*" "")
	  ("unless[ \t\n]*" "")
	  ("until[ \t\n]*" "")
	  ("while[ \t\n]*" "")
	  ("iterate[ \t\n]+\\w+[ \t\n]*" "")
	  ("profiling[ \t\n]*" "")
	  ;; special patterns for "define method", which is funky
	  (,(concat "\\(" dylan-define-pattern "\\)?"
		    "\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*")
	   "[ \t\n]*=>[^;)]+;?")
	  (,(concat "\\(" dylan-define-pattern "\\)?"
		    "\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*")
	   "[ \t\n]*;")
	  ,(concat "define[ \t]+" dylan-named-definition-pattern
		   "[ \t\n]+[^ \t\n]+")
	  ,(concat "define[ \t]+" dylan-unnamed-definition-pattern)
	  (,(concat "\\(" dylan-define-pattern "\\)?"
		    dylan-parameterized-definition-pattern
		    "[ \t\n]+[^\( ]*[ \t\n]*")
	   "")
	  "begin"
	  "case"
	  ;; Since we don't know the syntax of all the "with(out)-" macros,
	  ;; just assume that the user has already split the line at
	  ;; the end of the header.
	  ,(concat dylan-with-statement-prefix "[^\n]*")
	  "[[({]"))
  (setq dylan-find-keyword-pattern (concat "[][)(}{\"']\\|\\_<define\\_>\\|"
				     dylan-end-keyword-pattern
				     "\\|" dylan-keyword-pattern))
  (setq dylan-beginning-of-form-pattern
        (concat "[;,]\\|=>\\|"
                dylan-find-keyword-pattern
                "\\|" dylan-separator-word-pattern)))

(defun dylan-mode-init-font-lock-keywords ()
  ;; Construct Dylan Mode font-lock keyword variables, using the values of the
  ;; various pattern variables.

  ;; Decoration level 0: Don't highlight anything, currently -- mostly useful
  ;; for testing. Think about how to best differentiate between 0 and 1 by
  ;; moving some keyword patterns from 1 to 0, or by moving 2 to 3 and moving
  ;; some from 1 to 2.
  (setq dylan-font-lock-keywords nil)

  ;; Decoration level 1: Most Dylan keywords
  (setq dylan-font-lock-keywords-1
	(append dylan-font-lock-keywords
		`(,dylan-end-keyword-pattern
		  ,dylan-keyword-pattern
		  ,dylan-separator-word-pattern
		  ;; Symbols with keyword syntax
		  "[-_a-zA-Z?!*@<>$%]+:"
		  ;; Symbols with string syntax
		  ;;
		  ;; Is there a better way to fontify these symbols? Using
		  ;; font-lock syntactic keywords, perhaps?
		  ("\\(#\\)\"[^\"]*\"?" 1 font-lock-string-face)
		  ;; Logical negation operator
		  ("\\W\\(~\\)" 1 font-lock-negation-char-face)
		  ;; Function signature keywords
		  ;;
		  ;; "#" does not have symbol or word syntax, so we can't
		  ;; match for "\\<" at the start of #-words. Match for "not
		  ;; a word constituent" instead. This highlights some
		  ;; patterns that aren't valid Dylan, but it's close
		  ;; enough. (e.g., it highlights "#key" within "##key".)
		  (,(concat "\\W"
			    (regexp-opt
			     '("#rest" "#key" "#all-keys" "#next")
			     t)
			    "\\>")
		   1 font-lock-keyword-face)
		  ,dylan-other-pattern
		  ;; Condition signaling function calls
		  (,(concat (regexp-opt
			     '("signal" "error" "cerror"
			       "break" "check-type" "abort")
			     'words)
			    "[ \t]*(")
		   1 font-lock-warning-face)
		  ;; Definition starts
		  (,(concat "\\_<\\(" dylan-define-pattern
			    "\\(" dylan-constant-simple-definition-pattern "\\|"
			    dylan-variable-simple-definition-pattern "\\|"
			    dylan-other-simple-definition-pattern "\\)"
			    "\\)\\_>[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (7 (cond ((match-beginning 4) 'font-lock-constant-face)
			    ((match-beginning 5) 'font-lock-variable-name-face)
			    (t 'font-lock-function-name-face))))
		  (,(concat "\\_<\\(" dylan-define-pattern
			    dylan-definition-pattern "\\)")
		   1 font-lock-keyword-face)
		  (,(concat "\\_<\\(" dylan-define-pattern
			    "\\(" dylan-type-definition-pattern "\\|"
			    dylan-other-definition-pattern "\\)"
			    "\\)\\_>[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (6 (cond ((match-beginning 4) 'font-lock-type-face)
			    (t 'font-lock-function-name-face))))
		  ;; Local methods
		  ("method[ \t\n]+\\(\\(\\s_\\|\\sw\\)+\\)"
                   1 font-lock-function-name-face)
                  ("\\(\\_<\\(\\s_\\|\\sw\\)+\\)\\_>\\s-+::"
                   1 font-lock-variable-name-face)
                  ("::\\s-+\\(\\_<\\(\\s_\\|\\sw\\)+\\)\\_>"
                   1 font-lock-type-face)
		  ;; Definition ends
		  (,(concat "\\_<end[ \t]+\\("
			    dylan-type-definition-pattern
			    "\\|\\w*\\)\\_>[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (3 (cond ((match-beginning 2) 'font-lock-type-face)
			    (t 'font-lock-function-name-face)))))))

  ;; Decoration level 2: Highlight all function and local variable definitions,
  ;; and, optionally, all function calls.
  (setq dylan-font-lock-keywords-2
	(append dylan-font-lock-keywords-1
		'(("slot[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-variable-name-face)
		  ("block[ \t\n]+(\\([^)]+\\)" 1 font-lock-function-name-face)
		  ("let[ \t\n]+\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-variable-name-face)
		  ;; This highlights commas and whitespace separating the
                  ;; variable names. Try to find a way to highlight only the
                  ;; variable names.
		  ("let[ \t\n]+(\\([^)]+\\)" 1 font-lock-variable-name-face))))
  (when dylan-highlight-function-calls
    (setq dylan-font-lock-keywords-2
	  (append dylan-font-lock-keywords-2
		  ;; Function calls
		  '(("\\_<\\(\\(\\s_\\|\\w\\)+\\)("
		     1 font-lock-function-name-face))))))

(defun dylan-look-back (regexp)
  "Attempt to find a match for REGEXP immediately preceding the
current point. Returns t if a match was found, nil otherwise.
In order for this to work properly, the search string must end
with \"$\". Also note that this will only work within the
current line."
  (save-excursion
    (save-restriction
      (let ((dot (point)))
        (beginning-of-line)
        (narrow-to-region dot (point))
        (re-search-forward regexp nil t)))))

(defvar dylan-find-keyword-pattern nil
  "A pattern that matches the beginnings and ends of various \"blocks\",
including parenthesized expressions.")

(defvar dylan-beginning-of-form-pattern nil
  "Like `dylan-find-keyword-pattern', but matches statement terminators as
well.")

(defun dylan-header-end ()
  "Get the position of the end of the interchange file header. Dylan
interchange file headers end at the first empty line in the buffer,
containing no whitespace. Note that a well-formed header would match

    \"\\`\\([a-zA-Z][-a-zA-Z0-9]*:.*\n\\([ \t]+.+\n\\)*\\)*\n\"

but this function is only meant to partition the file into header and body
so we can handle them separately, whether they are well-formed or not."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (or (and (re-search-forward "^[ \t]*\\(\n\\|\\'\\)" nil t)
	       ;; in Emacs 18, the search just returns `t', not the point.
	       (point))
	  (point-max)))))

(defun dylan-find-keyword (&optional match-statement-end in-case no-commas
				     start)
  "Move the point backward to the beginning of the innermost
enclosing compound statement or set of parentheses. Return t on
success and nil otherwise."
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end))
	(result 'not-found))
    (while (and (>= (point) header-end) (eq result 'not-found))
      ;; cpage 2007-04-14: This could handle block comments better. The
      ;; re-search-backward pattern doesn't skip over them, and
      ;; dylan-skip-whitespace-backward can't skip over a block comment if point
      ;; is inside it.
      (setq
       result
       (if (re-search-backward (if match-statement-end
				   dylan-beginning-of-form-pattern
				 dylan-find-keyword-pattern) header-end t)
	   (cond (;; Skip backwards over eol comments.
		  (dylan-look-back dylan-comment-pattern)
		  (goto-char (match-beginning 0))
		  'not-found)
		 ;; If point is inside a block comment, keep searching. Since
		 ;; we've just tested (above) for an eol comment, if the text
		 ;; has the comment face applied it must be the interior of a
		 ;; block comment. This isn't a complete solution for handling
		 ;; block comments, but it provides much better behavior than
		 ;; not performing this test at all -- in which case, the
		 ;; interior of block comments are treated like code.
		 ((equal (get-text-property (point) 'face)
			 font-lock-comment-face)
		  'not-found)
		 ;; Skip backwards over balanced parens.
		 ((looking-at "[])}'\"]")
		  (condition-case nil
		      (progn
			(forward-char 1)
			(backward-sexp 1)
			'not-found)
		    (error nil)))
		 ;; At start of unrecognized definition. Stop searching.
		 ((and (looking-at "define") ; non-nesting top level form
		       (not (looking-at dylan-keyword-pattern)))
		  nil)
		 ;; Skip backward over blocks/statements that end with "end".
		 ((or (looking-at "end")	    ; Point is either before or
		      (and (dylan-look-back "\\_<end[ \t]*$") ; after "end".
			   (backward-word 1)))
		  (dylan-find-keyword)	; Search for the start of the block.
		  ;; cpage 2007-05-17: Why does this check for "method" and
		  ;; "define"? Should it also check for "define...function"?
		  ;; What about "define...class", etc.?
		  (if (or (and (looking-at "method")
			       (dylan-look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$"))
			  (looking-at "define"))
		      nil
		    'not-found))
		 ;; cpage 2007-05-17: What is this for? Does it handle `until:'
		 ;; and `while:' within `for' iteration clauses? Shouldn't it
		 ;; test for the keywords with the colon?
		 ;;
		 ;; hack for overloaded uses of "while" and "until" words
		 ((or (looking-at "until") (looking-at "while"))
		  (if (save-excursion
			(condition-case nil
			    (progn
			      (backward-up-list 1)
			      (backward-sexp 1)
			      (looking-at "for\\_<")) (error nil)))
		      (backward-up-list 1))
		  t)
		 ;; Statement macro separator words.
		 ((and (looking-at dylan-separator-word-pattern)
		       (not match-statement-end))
		  'not-found)
		 ;; cpage 2007-05-17: What do the following three clauses look
		 ;; for?
		 ((and (looking-at ";")
		       (not match-statement-end))
		  'not-found)
		 ((and (looking-at ",")
		       (or (not match-statement-end) no-commas))
		  'not-found)
		 ((and (looking-at "=>")
		       (not (and match-statement-end in-case)))
		  'not-found)
		 (t t))
	 (goto-char (point-min))
	 nil)))
    (and (equal t result)
	 (>= (point) header-end))))

(defun dylan-find-end (&optional match-statement-end in-case no-commas)
  "Move the point forward to the end of the innermost enclosing
compound statement or set of parentheses. Returns t on success
and nil otherwise."
  (let ((result 'not-found))
    (while (eq result 'not-found)
      (setq
       result
       (if (re-search-forward (if match-statement-end
				  dylan-beginning-of-form-pattern
				dylan-find-keyword-pattern) nil t)
	   (let ((match-start (match-beginning 0)))
	     (cond ((dylan-look-back dylan-comment-pattern)
		    (forward-line)
		    'not-found)
		   ((dylan-look-back "[[({'\"]$")
		    (condition-case nil
			(progn
			  (backward-char 1)
			  (forward-sexp 1)
			  'not-found)
		      (error nil)))
		   ((dylan-look-back "[])}]$") t)
		   ((dylan-look-back "define$") ; top-level form special case
		    (dylan-find-end t nil nil)
		    nil)
		   ((dylan-look-back "\\_<end\\([ \t]+\\w+\\)?$")
		    (if (and (not (looking-at "[ \t]+\\(end\\|=>\\)\\_>"))
			     (looking-at "[ \t]+\\w+"))
			(goto-char (match-end 0)))
		    t)
		   ;; hack for overloaded uses of "while" and "until" reserved
		   ;; words
		   ((dylan-look-back "until$\\|while$")
		    (if (save-excursion
			  (condition-case nil
			      (progn
				(backward-up-list 1)
				(backward-sexp 1)
				(looking-at "for\\_<")) (error nil)))
			(up-list 1))
		    t)
		   ((save-excursion (goto-char match-start)
				    (looking-at dylan-separator-word-pattern))
		    t)
		   ((dylan-look-back ";$")
		    (if (not match-statement-end)
			'not-found
		      t))
		   ((dylan-look-back ",$")
		    (if (or (not match-statement-end) no-commas)
			'not-found
		      t))
		   ((dylan-look-back "=>$")
		    (if (not (and match-statement-end in-case))
			'not-found
		      t))
		   (t				; start compound statement
		    (if (save-excursion (goto-char match-start)
					(looking-at "define"))
			(progn (dylan-find-end) nil)
		      (dylan-find-end)
		      'not-found))))
	 (goto-char (point-max))
	 nil)))
    result))

(defun dylan-skip-star-comment-backward ()
  "Utility function for `dylan-skip-whitespace-backward'. Find
beginning of enclosing \"/*\" comment. Deals properly with
nested \"/*\" and with \"//\"."
  (re-search-backward "/\\*\\|\\*/")
  (while (cond ((dylan-look-back dylan-comment-pattern)
		(goto-char (match-beginning 0)))
	       ((looking-at "\\*/")
		(dylan-skip-star-comment-backward))
	       (t nil))
    (re-search-backward "/\\*\\|\\*/"))
  t)

(defun dylan-skip-star-comment-forward ()
  "Utility function for `dylan-skip-whitespace-forward'. Find
end of enclosing \"/*\" comment. Deals properly with nested
\"/*\" and with \"//\"."
  (re-search-forward "/\\*\\|\\*/")
  (while (cond ((dylan-look-back dylan-comment-pattern)
		(end-of-line))
	       ((dylan-look-back "/\\*$")
		(dylan-skip-star-comment-forward))
	       (t nil))
    (re-search-forward "/\\*\\|\\*/"))
  t)

(defun dylan-skip-whitespace-backward ()
  "Skips over both varieties of comments and other whitespace characters."
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end)))
    (unless (< (point) header-end)
      ;; skip syntactic whitespace
      (skip-syntax-backward " >" header-end)
      ;; skip comments
      (while (cond ((dylan-look-back dylan-comment-pattern)
		    (goto-char (match-beginning 0)))
		   ((dylan-look-back "\\*/$")
		    (goto-char (match-beginning 0))
		    (dylan-skip-star-comment-backward))
		   (t nil))
        (skip-syntax-backward " >" header-end)))))

(defun dylan-skip-whitespace-forward ()
  "Skips over both varieties of comments and other whitespace characters."
  ;; skip syntactic whitespace
  (skip-syntax-forward " >")
  ;; skip comments
  (while (cond ((looking-at dylan-comment-pattern)
		(goto-char (match-end 0))
		t)
	       ((looking-at "/\\*")
		(goto-char (match-end 0))
		(dylan-skip-star-comment-forward))
	       (t nil))
    (skip-syntax-forward " >")))

(defun dylan-aux-find-body-start (clauses)
  "Helper function for `dylan-find-body-start'."
  (save-excursion
    (cond ((null clauses) (point))
	  ((looking-at (car clauses))
	   (if (null (cdr clauses))
	       (match-end 0)
	     (goto-char (match-end 0))
	     (and (looking-at "[[({]")
		  (condition-case nil (forward-list) (error nil))
		  (dylan-aux-find-body-start (cdr clauses))))))))

(defun dylan-find-body-start (exprs)
  "When passed `dylan-start-expressions', processes it to find the
beginning of the first statement in the compound statement that
starts at the current point."
  (cond ((null exprs) (point-max))
	((listp (car exprs))
	 (or (dylan-aux-find-body-start (car exprs))
             (dylan-find-body-start (cdr exprs))))
	(t (if (looking-at (car exprs))
	       (match-end 0)
	     (dylan-find-body-start (cdr exprs))))))

(defun dylan-backward-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the previous statement
and the current one. If we are already between statements, move back one
more."
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end)))
    (unless (< (point) header-end)
      (dylan-skip-whitespace-backward)
      (let* ((dot (point)))
        ;; skip over "separator words"
        (when (save-excursion
                (and (re-search-backward dylan-separator-word-pattern
                                         header-end t)
                     (if (not (looking-at "exception\\|elseif"))
                         (forward-word 1)
                       (goto-char (match-end 0))
                       (condition-case nil (forward-list 1)
                         (error nil))
                       t)
                     (>= (point) dot)))
          (re-search-backward dylan-separator-word-pattern header-end t)
          (dylan-skip-whitespace-backward))
        (if (dylan-look-back "[,;]$\\|=>$")
            (backward-char))
        (cond ((not (dylan-find-keyword t in-case no-commas))
               (if (dylan-look-back "\\(define\\|local\\)[ \t]+") ; hack
                   (goto-char (match-beginning 0))))
              ((looking-at dylan-separator-word-pattern)
               (let ((start (point)))
                 (cond ((looking-at "\\(exception\\|elseif\\)[ \t\n]*(")
                        (goto-char (match-end 1))
                        (condition-case nil (forward-list 1)
                          (error nil)))
                       (t (forward-word 1)))
                 (if (>= (point) dot)
                     (progn (goto-char start)
                            (dylan-backward-statement in-case no-commas)))))
              ((looking-at "[;,]\\|=>")
               (goto-char (match-end 0)))
              (t
               ;; check whether we were already at the first "form" in an
               ;; enclosing block
               (let ((first (dylan-find-body-start dylan-start-expressions)))
                 (if (< first dot)
                     (goto-char first)
                   (if (dylan-look-back "\\(define\\|local\\)[ \t]+") ; hack
                       (goto-char (match-beginning 0)))))))))))

(defun dylan-beginning-of-form ()
  "Finds the beginning of the innermost statement that contains or
terminates at the current point."
  (interactive)
  (dylan-backward-statement)
  (dylan-skip-whitespace-forward))

(defun dylan-forward-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the current statement
and the next one. If we are already between statements, move forward one
more."
  (dylan-skip-whitespace-forward)
  (let ((dot (point)))
    ;; skip over "separator words"
    (if (looking-at dylan-separator-word-pattern)
        (if (not (looking-at "exception\\|elseif"))
            (forward-word 1)
          (goto-char (match-end 0))
          (condition-case nil (forward-list 1)
            (error nil))))
    (cond ((not (dylan-find-end t in-case no-commas))
           (if (dylan-look-back "\\(define\\|local\\)[ \t]+")	; hack
               (goto-char (match-beginning 0))))
          (t)))
  (cond ((looking-at "[,;]$") (forward-char))
        ((looking-at "=>") (forward-word 1))))

(defun dylan-end-of-form ()
  "Finds the end of the innermost statement that contains or begins
at the current point."
  (interactive)
  (dylan-forward-statement))


;;; Indentation:

(defun dylan-indent-if-continuation (term-char line-start block-start
                                               &optional in-case)
  (save-excursion
    (goto-char line-start)
    (dylan-skip-whitespace-backward)
    (if (dylan-look-back "finally$")	; special case -- this one is tricky
        0				; because "for" can have empty bodies
      (let ((real-start (point)))
        (dylan-backward-statement in-case)
        (unless (= (point) real-start) ; make sure we went back a statement
          (dylan-skip-whitespace-forward))
        (cond ((and (= block-start 0) (not (looking-at "define")))
               0)			; special case for beginning of file
              ((= real-start block-start) 0)
              ((< (point) block-start)
               (- dylan-continuation-indent dylan-indent))
              ;; Indent keyword args to line up with the first arg after #key.
              ((looking-at "#key[ \t]+")
               (- (match-end 0) (match-beginning 0)))
              ;; What does this do?
              ((< (save-excursion
                    (dylan-forward-statement in-case
                                             (equal term-char ";"))
                    (point)) line-start)
               0)
              ;; Comma-separated lists of local methods should be indented to
              ;; line up with the first method when it begins on the same line
              ;; as "local".
              ((looking-at "local[ \t]+")
               (- (match-end 0) (match-beginning 0)))
              (t dylan-continuation-indent))))))

(defun dylan-indent-line ()
  "Indents a line of dylan code according to its nesting."
  (interactive)

  ;; If we're inside the interchange file header, let the user indent as
  ;; they please. If we're indenting a region (i.e., if this function wasn't
  ;; called interactively), leave the header indenting as-is.
  (if (< (point) (dylan-header-end))
      (when (called-interactively-p 'interactive)
        (when (<= (current-column) (current-indentation))
          (back-to-indentation))
        (insert-char ?\  dylan-indent))
    (save-excursion
      ;; Move point to the end of the current indentation. This allows us to
      ;; use looking-at to examine the start of the current line of code
      ;; without having to put whitespace at the start of all the patterns.
      (back-to-indentation)
      (let* ((body-start)     ; Beginning of "body" of enclosing compound statement.
             (in-paren)       ; t if in parenthesized expression.
             (paren-indent 0) ; Indentation of first non-space after open paren.
             (in-case)        ; t if in "case" or "select" statement.
             (block-indent    ; Indentation of enclosing compound statement.
              (save-excursion
                (if (not (dylan-find-keyword))
                    nil
                  (and (looking-at "method")
                       (dylan-look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$")
                       (goto-char (match-beginning 0)))
                  (and (looking-at "[[({]")
                       (setq in-paren t)
                       (save-excursion
                         (let ((dot (point)))
                           (forward-char)
                           (re-search-forward "[^ \t]")
                           (setq paren-indent (- (point) dot 1)))))
                  (and (looking-at "select\\|case") (setq in-case t))
                  (setq body-start (dylan-find-body-start
                                    dylan-start-expressions))
                  (current-column))))
             (indent		; correct indentation for this line
              (cond ((not block-indent)
                     (dylan-indent-if-continuation ";" (point) 0))
                    ;; some keywords line up with start of comp. stmt
                    ((looking-at dylan-separator-word-pattern) block-indent)
                    ;; end keywords line up with start of comp. stmt
                    ((looking-at dylan-end-keyword-pattern) block-indent)
                    ;; parenthesized expressions (separated by commas)
                    (in-case
                     ;; if the line is blank, we pick an arbitrary
                     ;; indentation for now. We judge the "proper"
                     ;; indentation by how the statement is punctuated once
                     ;; it is finished
                     (cond ((looking-at "^$")
                            (if (save-excursion
                                  ;; Look for end of prev statement. This
                                  ;; is hairier than it should be because
                                  ;; we may be at the end of the buffer
                                  (let ((dot (point)))
                                    (dylan-forward-statement t)
                                    (dylan-skip-whitespace-backward)
                                    (if (> (point) dot)
                                        (dylan-backward-statement t))
                                    (dylan-look-back ";$\\|=>$")))
                                (+ block-indent dylan-indent dylan-indent
                                   (dylan-indent-if-continuation
                                    "," (point) body-start t))
                              (+ block-indent dylan-indent
                                 (dylan-indent-if-continuation
                                  "," (point) body-start t))))
                           ((save-excursion
                              (dylan-forward-statement t)
                              (dylan-look-back ",$\\|=>$"))
                            (+ block-indent dylan-indent
                               (dylan-indent-if-continuation "," (point)
                                                             body-start t)))
                           (t (+ block-indent dylan-indent dylan-indent
                                 (dylan-indent-if-continuation
                                  "," (point) body-start t)))))
                    (in-paren (+ block-indent paren-indent
                                 (dylan-indent-if-continuation "," (point)
                                                               body-start)))
                    ;; statements (separated by semi-colons)
                    (t (+ block-indent dylan-indent
                          (dylan-indent-if-continuation ";" (point)
                                                        body-start))))))
        (unless (= indent (current-indentation))
          (save-excursion
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to-column indent))))))
  ;; put the cursor where the user is likely to want it.
  (let ((col (current-indentation)))
    (when (< (current-column) col)
      (move-to-column col))))


;;; Motion:

;; This intensely DWIMish function tries to insert whatever text is needed to
;; finish off the enclosing indentation context.
(defun dylan-insert-block-end ()
  "Insert whatever text is needed to finish off the enclosing indentation
context (e.g. \"end method foo;\"). Makes educated guesses about whether
newlines and closing punctuation are needed."
  (interactive)
  (let* ((here (point))
         (terminator)
         (need-newline)
         (str
          (save-excursion
            (if (not (dylan-find-keyword))
                (error "No nesting block"))
            ;; need newline if multi-line block and not "("
            (setq need-newline (not (or (looking-at "[[({]")
                                        (save-excursion (end-of-line)
                                                        (>= (point) here)))))
            (setq terminator
                  (save-excursion
                    (cond ((not (dylan-find-keyword)) ";")
                          ((looking-at "[[({]") "")
                          (t ";"))))
            ;; We intentionally fail to accept newlines in "define
            ;; foo" because it can cause undue confusion.
            (if (looking-at
                 (concat "define\\([ \t]+\\w+\\)*[ \t]*"
                         dylan-definition-pattern))	; find the actual word
                (goto-char (match-beginning 2)))
            (cond ((looking-at "begin") (concat " end" terminator))
                  ((looking-at "\\[") "]")
                  ((looking-at "(") ")")
                  ((looking-at "{") "}")
                  ((or (looking-at "\\(method\\|function\\|class\\|C-subtype\\|C-mapped-subtype\\)\\([ \t]+\\(\\s_\\|\\w\\)+\\)?")
                       (looking-at "\\(library\\|module\\)[ \t]+\\(\\s_\\|\\w\\)+")
                       (looking-at "\\w+"))
                   (concat " end "
                           (buffer-substring (match-beginning 0)
                                             (match-end 0))
                           terminator))
                  (t (concat " end" terminator))))))
    (when need-newline
      (beginning-of-line)
      (if (looking-at "[ \t]*$")
          (delete-horizontal-space)
        (end-of-line)
        (newline)))
    (let* ((start (point))
           (end (progn (insert str) (point))))
      (goto-char start)
      (while (re-search-forward "[ \t\n]+" end t)
	(replace-match " "))
      (goto-char end)
      (dylan-indent-line))))

(defvar dylan-defun-regexp "^ *\\(define \\|module:\\|interface \\)"
  "Regular expression identifying the beginning of a definition.")

(defun dylan-beginning-of-defun (&optional arg)
  "Move backward to next beginning of definition. With ARG, do this ARG times."
  (interactive "p")
  (let ((header-end (dylan-header-end)))
    (dotimes (i (or arg 1))
      (unless (< (point) header-end)
        (when (re-search-backward dylan-defun-regexp header-end t (or arg 1))
          (beginning-of-line))))))

(defun dylan-end-of-defun (&optional arg)
  "Move forward to next end of function. With ARG, do this ARG times."
  (interactive "p")
  (dotimes (i (or arg 1))
    (let ((dot (point)))
      (dylan-skip-whitespace-forward)
      (end-of-line)
      (let ((next-begin (or (re-search-forward dylan-defun-regexp nil 'move 1)
                            (point-max))))
        (beginning-of-line)
        (let ((last-end (re-search-backward "^ *end\\s-" nil 'move 1)))
          (if (and last-end (< last-end next-begin) (> last-end dot))
              (progn (goto-char last-end)
                     (end-of-line)
                     (forward-char))
            (progn (goto-char next-begin)
                   (beginning-of-line)
                   (backward-char))))))))


;;; Font locking:

(defun dylan-font-lock-fontify-region (beg end loudly)
  "Dylan Mode font lock fontification. Handles fontification of
interchange file headers separately from the file body; they have
entirely separate character and keyword syntaxes.

This is particularly important since headers can contain
apostrophes, for example, that would otherwise confuse the
first-pass, character syntax-based fontification and cause it to
treat code in the file body as the interior of a string."
  (let ((header-end (dylan-header-end)))
    ;; If the region overlaps the header, fontify the header with the
    ;; appropriate keyword patterns and character syntax table.
    (when (< beg header-end)
      (let ((end (min end header-end))
	    (font-lock-dont-widen t)
	    (font-lock-keywords dylan-font-lock-header-keywords)
	    (font-lock-keywords-only t))
	(save-restriction
	  (narrow-to-region 1 end)
          (font-lock-default-fontify-region beg end loudly))))
    ;; Fontify the Dylan code. We narrow the buffer to exclude the header from
    ;; character syntactic fontification.
    (when (> end header-end)
      (let ((beg (max beg header-end))
	    (font-lock-dont-widen t))
	(save-restriction
	  (narrow-to-region header-end (point-max))
          (font-lock-default-fontify-region beg end loudly))))))


;;; Library and module detection:

(defvar-local dylan-buffer-library nil
  "Library of the current buffer.")

(defvar-local dylan-buffer-module nil
  "Module of the current buffer.")

(defun dylan-find-buffer-module ()
  (let ((case-fold-search t)
        (regexp "^module:[ \t]*\\([^ \n\r\t]+\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))

(defun dylan-find-buffer-library (path inc)
  (let ((lid-files (directory-files path t ".*\\.lid" t)))
    (if lid-files
      (let ((try-lid (car lid-files)))
        (with-current-buffer (find-file-noselect try-lid)
          (goto-char (point-min))
          (let ((found
                 (re-search-forward "[Ll]ibrary:[ \t]*\\([^ \n\r\t]+\\)")))
            (if found
                (buffer-substring
                 (match-beginning 1)
                 (match-end 1))))))
      (when (< inc 5)
        (dylan-find-buffer-library (concat path "/..") (1+ inc))))))


;;; dylan-mode:

(defvar dylan-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?= "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\f " " table)
    (modify-syntax-entry ?# "'" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?/ "_ 124b" table)
    (modify-syntax-entry ?\* "_ 23n" table)
    table))

;;;###autoload
(define-derived-mode dylan-mode prog-mode "Dylan"
  "Major mode for editing Dylan programs.

May be customized with the options in the `dylan' customization
group.

Indentation is controlled by the `dylan-indent' customizable
variable. The default is two spaces.

To see the current version of Dylan Mode, enter
`\\[dylan-version]'.

This mode runs the hook `dylan-mode-hook', as the final step
during initialization.

\\{dylan-mode-map}"
  (dylan-mode-init-keyword-patterns)
  (dylan-mode-init-font-lock-keywords)

  (define-key dylan-mode-map "\M-a" 'dylan-beginning-of-form)
  (define-key dylan-mode-map "\M-e" 'dylan-end-of-form)
  (define-key dylan-mode-map "\M-)" 'dylan-insert-block-end)

  (setq-local indent-line-function 'dylan-indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+[ \t]*")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)

  (setq-local beginning-of-defun-function 'dylan-beginning-of-defun)
  (setq-local end-of-defun-function 'dylan-end-of-defun)

  (setq-local font-lock-defaults
              `((dylan-font-lock-keywords
                 dylan-font-lock-keywords-1
                 dylan-font-lock-keywords-2)
                nil t nil nil
                (font-lock-fontify-region-function
                 . dylan-font-lock-fontify-region)))
  (setq dylan-buffer-library (dylan-find-buffer-library "." 0))
  (setq dylan-buffer-module (dylan-find-buffer-module)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dylan\\'" . dylan-mode))


(provide 'dylan-mode)

;;; dylan-mode.el ends here
