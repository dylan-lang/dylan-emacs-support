;;; dylan-mode.el --- Major mode for editing Dylan programs.

;; Copyright (C) 1994, 1995, 1996  Carnegie Mellon University
;; Copyright (C) 2004, 2005, 2007  Chris Page

;; Author: Robert Stockton (rgs@cs.cmu.edu), others, then Chris Page
;; Maintainer: Chris Page <cpage@opendylan.org>
;; Version: 1.21

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
;; Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
;; comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
;; Also, see http://www.gwydiondylan.org/ for updates and documentation. 
;;
;; Known limitations:
;;   Limited support for block (i.e. "/*") comments
;;     TAB indentation doesn't work within "/*" comments
;;   Magic => support doesn't work at end of buffer
;;
;; Desired features:
;;   Copy indentation from first statement in body
;;   Delete-backward-expanding-tabs
;;   More consistency in font-lock highlighting
;;   Better support for "/*" comments

;; History:
;;   version 0.1: Quick one day hack -- appears to be useful
;;   version 0.2: Added font lock support
;;   version 0.3: Added misc features to work towards "industrial strength"
;;     Detects "continuation lines" and indents them specially
;;     Basic comment support
;;     Added "symbol character" support (with second syntax table for
;;     indentation and font-lock)
;;     Added indentation support for "elseif" and "exception" clauses
;;     Cleaned up a number of old bugs
;;   version 0.4: Brought into compliance with new "post-DN22" syntax
;;     new comment syntax
;;     new "return types" syntax
;;     accepts sealed, open, concrete, and abstract in class definitions
;;     fixed bug in comment indentation
;;     fine tune font-lock-regexps for "define ..."
;;   version 0.5:
;;     Added "dylan-insert-block-end" function.
;;     Fixed bug in short circuiting indentation outside top level forms.
;;   version 1.0:
;;     Major code reorganization
;;     Added full case statement support
;;     Fixed "continuations" at top level
;;     Added "beginning-of-form" and "end-of-form" commands
;;     Fixed support for character literals and for "quoted" quote chars
;;   version 1.1:
;;     The "font-lock-mode" support no longer interferes with other language
;;     modes.   (Thanks to emg@hip.atr.co.jp)
;;   version 1.2:
;;     Fixes for various bugs (thanks to wlott@cs.cmu.edu):
;;       "foo-end;" was mistaken for the end of a compound statement
;;       syntax tables sometimes ended in an odd state after errors
;;       indentation sometimes failed if parens weren't balanced
;;   version 1.3:
;;     Added font lock support for "sealed", "open", etc.
;;   version 1.4:
;;     Added special-case support for generic function "continuations" and
;;     for outdenting "=>" in function definitions.
;;   version 1.5:
;;     Adjusted regexps to accept "primary" and "free" adjectives
;;     Mentioned dylan-outdent-arrows in the documentation
;;     Added a space to comment-start
;;   version 1.6:
;;     Fixed bug in generic function continuations from 1.4.
;;   version 1.7:
;;     Merged changes from Joseph Wilson (jnw@cis.ufl.edu) to facilitate use 
;;     within more general modes.
;;   version 1.8:
;;     Font lock fix for XEmacs from John Shen <jshen@cas.org>.
;;   version 1.9:
;;     Fixed bug in indentation for expressions in square and curly braces.
;;     Generalized modifier-word handling for definitions.
;;     Generalized 'define words' lists for easier extension.
;;     Fixed "exceeded max nesting" bug with long lists of items.
;;     Added switches for font-lock highlighting of functions and definition 
;;     sites.
;;   version 1.10:
;;     Fixed bug in "," reindent code.  It couldn't deal with commas
;;     in the middle of strings.
;;   version 1.11 12/13/95 by David N. Gray <gray@harlequin.com>:
;;     Add C-M-a and C-M-e for beginning and end of definition.
;;     Fix font lock syntax table for XEmacs.
;;   version 1.12:
;;     Added support for "define function"
;;     Fixed to ignore keywords in the file header.
;;     Do not require ";" after return value (contributed by
;;     gray@harlequin.com)
;;     fixed various bugs resulting from overzealous acceptance of newlines:
;;       the word "end" at the end of a comment line caused bad indentation
;;       empty module and library definitions did strange things.
;;   version 1.13:
;;     Fixed dylan-insert-block-end to handle "define function" properly.
;;     (Hopefully) fixed bug in indenting function result declarations,
;;     which was introduced by the previous round of fixes.
;;   version 1.14:
;;     Modified to use font-lock-syntax-table if it is defined.  This
;;     eliminates the need to use unportable constructs to modify the
;;     behavior of font-lock mode -- thus, fontification should now be
;;     reliable on modern EMACSen.
;;   version 1.15
;;     Fixed syntax table bugs which primarily affected gnu-emacs
;;     19.[23] users.  
;;     Optimized "beyond-dylan-header".  
;;     Removed new-lines from various font-lock regexps so that
;;     adjacent declarations aren't glommed together.
;;   version 1.16
;;     Made symbols properly fontify as strings.
;;     Added the dylan-no-highlights-in-header variable (enabled by
;;     default) which keeps keywords in headers from being treated
;;     specially.
;;   adjusted 12/6/96 by David N. Gray to set dylan-no-highlights-in-header
;;     only for Emacs 19.31 or later
;;   Modified 7/19/98 by David N. Gray to indent bodies of "with-..." macros.
;;   Modified 16 Jan 99 by Eric M. Kidd for C-FFI macros.
;;   version 1.17
;;     Various fixes and changes over quite some time by Chris Page.
;;     Retroactively bumped the version number after noticing that it
;;     had been left at 1.16 far too long.
;;   version 1.18
;;     Changed some user-modifiable variables to Customization
;;     variables and defined a Dylan customization group. Other
;;     miscellaneous fixes and changes.
;;   version 1.19
;;     Added fontification of the Dylan interchange file header.
;;     Other miscellaneous fixes and cleanups.
;;   version 1.20
;;     Added support for multiple levels of font-lock decoration.
;;   version 1.21
;;     Removed dylan-outdent-arrows. It should always be on.

;;; Code:

(defconst dylan-version "1.21"
  "Dylan Mode version number.")

(defun dylan-version ()
  "Return string describing the version of Dylan mode.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message (dylan-version))
    (format "Dylan Mode version %s" dylan-version)))

;;; Customization

(defgroup dylan nil "Major mode for editing Dylan source." :group 'languages)

(defcustom dylan-indent 2
  "*Number of spaces to indent each sub-block."
  :type  'integer
  :group 'dylan)

(defcustom dylan-highlight-function-calls nil
  "*Whether to highlight function calls in Font Lock mode.
Applies only in Font Lock decoration level 2 or higher.

This uses a very simple regular expression that highlights just
about anything adjacent to a left-parenthesis."
  :type  'boolean
  :group 'dylan)

(defun dylan-indent-spaces-only ()
  "Indent using spaces only, without any tab characters.

A trivial function that sets `indent-tabs-mode' to nil, suitable
for use as a Dylan mode hook."
  (setq indent-tabs-mode nil))

(defcustom dylan-mode-hook '(dylan-indent-spaces-only)
  "*Hook called by `dylan-mode'."
  ;; To Do: Add support for imenu, then enable this option.
  ;; :options '(imenu-add-menubar-index)
  :options '(dylan-indent-spaces-only)
  :type  'hook
  :group 'dylan)

(defface dylan-header-background
  '((((class color)
      (background light))
     (:background "Lavender"))
    (((class color)
      (background dark))
     (:background "Navy Blue"))
    (((class grayscale)
      (background light))
     (:background "grey95"))
    (((class grayscale)
      (background dark))
     (:background "grey5")))
  "Background face for Dylan interchange file headers.

This is designed to apply background attributes to the entire
header, with other faces applied on top."
  :group 'dylan)

(defface dylan-header-separator
  '((t nil))
  "Face for the last line of Dylan interchange file headers."
  :group 'dylan)

(defface dylan-header-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for Dylan interchange file header keywords."
  :group 'dylan)

(defface dylan-header-value
  '((t nil))
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


;; Older variable originally documented as "user modifiable", but this should
;; rarely (if ever) be modified by users.

(defvar dylan-mode-for-emacs-21-and-later (not (string-lessp emacs-version "20"))
  "*Perform syntax highlighting in a way that requires GNU Emacs
21 or later.")


;; Private definitions.  Extensible by using dylan-add-keyword in your
;; dylan-mode-hook.

(defun dylan-add-keyword (variable keyword)
  "Add a custom keyword to `dylan-mode'.

VARIABLE is one of the dyl-*-words variables listed in
`dylan-mode.el'. KEYWORD is a string containing a word to add.

This is designed to be called from your `dylan-mode-hook'."
  (add-to-list variable keyword)
  (dylan-mode-init-patterns-and-keywords)
  (if (fboundp 'font-lock-mode)
      (setq font-lock-keywords dylan-font-lock-keywords)))

(defvar dyl-unnamed-definition-words
  '(;; Melange/C-FFI
    "interface")
  "Words that introduce unnamed definitions like \"define interface\".")

(defvar dyl-named-definition-words
  '(;; Dylan
    "module" "library" "macro"
    ;; C-FFI
    "C-struct" "C-union" "C-function"
    "C-callable-wrapper")
  "Words that introduce simple named definitions like \"define library\".")

(defvar dyl-type-parameterized-definition-words
  '(;; Dylan
    "class"
    ;; C-FFI
    "C-subtype" "C-mapped-subtype")
  "Words that introduce type definitions like \"define class\". These are
also parameterized like \"define method\" and are appended to 
`dyl-other-parameterized-definition-words'.")

(defvar dyl-other-parameterized-definition-words
  '(;; Dylan
    "method" "function"
    ;; C-FFI
    "C-variable" "C-address")
  "Words that introduce trickier definitions like \"define method\".  These
require special definitions to be added to `dyl-start-expressions'.")

(defvar dyl-constant-simple-definition-words
  '(;; Dylan
    "constant")
  "Words that introduce module constant definitions. These must also be
simple definitions and are appended to `dyl-other-simple-definition-words'.")

(defvar dyl-variable-simple-definition-words
  '(;; Dylan
    "variable")
  "Words that introduce module variable definitions. These must also be
simple definitions and are appended to `dyl-other-simple-definition-words'.")

(defvar dyl-other-simple-definition-words
  '(;; Dylan
    "generic" "domain"
    ;; C-FFI
    "C-pointer-type"
    ;; Extensions
    "table")
  "Other words that introduce simple definitions (without implicit bodies).")

(defvar dyl-statement-words
  '(;; Dylan
    "if" "block" "begin" "method" "case" "for" "select" "when" "unless"
    "until" "while"
    ;; Extensions
    "iterate" "profiling")
  "Words that begin statements with implicit bodies.")

;; Names beginning "with-" and "without-" are commonly used as statement macros.
(defvar dyl-with-statement-prefix "with\\(out\\)\\{0,1\\}-")
(defvar dyl-statement-prefixes
  (concat "\\|\\b" dyl-with-statement-prefix "[-_a-zA-Z?!*@<>$%]+"))

(defvar dyl-separator-words
  '(;; Dylan
    "finally" "exception" "cleanup" "else" "elseif" "afterwards")
  "Patterns that act as separators in compound statements.  This may
include any general pattern that must be indented specially.")

(defvar dyl-other-words
  '(;; Dylan
    "above" "below" "by" "from"
    "handler" "in" "instance" "let" "local" "otherwise"
    "slot" "subclass" "then" "to"
    ;; Extensions
    "keyed-by" "virtual")
  "Keywords that do not require special indentation handling, but which
should be highlighted by font-lock.")


;;; The mode implementation


;; Set up the key map

(defun dylan-mode-commands (map)
  (define-key map ";"       'dylan-insert-and-indent)
  (define-key map ","       'dylan-insert-and-indent)
  (define-key map ">"       'dylan-arrow-insert)
  (define-key map "\n"      'dylan-newline-and-indent)
  (define-key map "\t"      'dylan-indent-line)
  (define-key map "\177"    'backward-delete-char-untabify)
  (define-key map "\M-a"    'dylan-beginning-of-form)
  (define-key map "\M-e"    'dylan-end-of-form)
  (define-key map "\M-)"    'dylan-insert-block-end)
  (define-key map "\M-\C-a" 'dylan-beginning-of-defun)
  (define-key map "\M-\C-e" 'dylan-end-of-defun)
  (define-key map "\M-\C-h" 'dylan-mark-function))

(defvar dylan-mode-map ()
  "Keymap for Dylan mode.")
(if (not dylan-mode-map)
    (progn
      (setq dylan-mode-map (make-sparse-keymap))
      (dylan-mode-commands dylan-mode-map)))


;; Set up the abbrev table

(defun dyl-define-abbrev (table name expansion hook)
  ;; Compatibility wrapper for `define-abbrev' that passes a non-nil
  ;; sixth argument for SYSTEM-FLAG in Emacsen that support it
  ;; (currently only Emacs >= 21.2).
  (condition-case nil
      (define-abbrev table name expansion hook 0 t)
    (wrong-number-of-arguments
     (define-abbrev table name expansion hook 0))))

(defvar dylan-mode-abbrev-table nil
  "Abbrev table in use in Dylan Mode buffers.  Provides \"hooked\" 
abbreviations to reindent lines containing separator keywords.")
(if (not dylan-mode-abbrev-table)
    (progn
      (define-abbrev-table 'dylan-mode-abbrev-table ())
      (dyl-define-abbrev dylan-mode-abbrev-table "end" "end" 'reindent-line)
      (let ((list dyl-separator-words))
	(while list
	  (dyl-define-abbrev dylan-mode-abbrev-table
	    (car list) (car list) 'reindent-line)
	  (setq list (cdr list))))))


;; Set up syntax tables

(defvar dylan-mode-syntax-table nil
  "User-level syntax table.  Provides support for forward-word, etc.")
(defvar dylan-indent-syntax-table nil
  "Special syntax table used by the indent and font lock code for
finding keywords and the like.  This is necessary because there
is no equivalent to \"\b\" for identifiers.")

(defun dylan-set-up-syntax-tables ()
  (unless dylan-mode-syntax-table
	
    ;; Set up the user syntax table.
    (setq dylan-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?-  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?<  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?>  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ??  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?!  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?=  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?:  "_"  dylan-mode-syntax-table)
    (modify-syntax-entry ?'  "\"" dylan-mode-syntax-table)
    (modify-syntax-entry ?\f " "  dylan-mode-syntax-table)
    (modify-syntax-entry ?#  "'"  dylan-mode-syntax-table)
    
    ;; Set up the indent table; derived from the user table, we change the
    ;; syntax of various Dylan identifier characters to word constituents.
    (setq dylan-indent-syntax-table
	  (copy-syntax-table dylan-mode-syntax-table))
    (modify-syntax-entry ?_ "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?- "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?< "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?> "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?? "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?! "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?= "w" dylan-indent-syntax-table)
    (modify-syntax-entry ?: "w" dylan-indent-syntax-table)
    
    ;; Set up comment syntax (for both tables). Different Emacsen handle
    ;; comments differently.
    (cond ((or (and (boundp 'running-lemacs) running-lemacs)
	       (string-match "XEmacs" emacs-version))
	   (modify-syntax-entry ?\n "> b"    dylan-indent-syntax-table)
	   (modify-syntax-entry ?/  "w 1456" dylan-indent-syntax-table)
	   (modify-syntax-entry ?\* "w 23"   dylan-indent-syntax-table)
	   (modify-syntax-entry ?\n "> b"    dylan-mode-syntax-table)
	   (modify-syntax-entry ?/  "_ 1456" dylan-mode-syntax-table))
	  (t
	   (modify-syntax-entry ?\n "> b"    dylan-mode-syntax-table)
	   (modify-syntax-entry ?/  "_ 124b" dylan-mode-syntax-table)
	   (modify-syntax-entry ?\* "_ 23n"  dylan-mode-syntax-table)
	   (modify-syntax-entry ?\n "> b"    dylan-indent-syntax-table)
	   (modify-syntax-entry ?/  "w 124b" dylan-indent-syntax-table)
	   (modify-syntax-entry ?\* "w 23n"  dylan-indent-syntax-table)))))

(dylan-set-up-syntax-tables)


;; Ugly code, which you don't want to look at.
(defvar dylan-comment-pattern "//.*$"
  "Internal pattern for finding comments in Dylan code.  Currently only
handles end-of-line comments.")

(defun make-pattern (start &rest list)
  "Build a search pattern that matches any of the patterns passed to it.
Makes sure that it doesn't match partial words."
  (let ((str (concat "\\b" start "\\b")))
    (while list
      (setq str (concat str "\\|\\b" (car list) "\\b"))
      (setq list (cdr list)))
    str))

(defvar dyl-start-expressions '()
  "Patterns that match that portion of a compound statement that
precedes the body.  This is used to determine where the first
statement begins for indentation purposes.

Contains a list of patterns, each of which is either a regular
expression or a list of regular expressions.  A set of balanced
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
	      "[-_a-zA-Z?!*@<>$%]+"	; module name...
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
(defconst dyl-define-pattern "define\\([ \t]+\\w+\\)*[ \t]+")

(defvar dyl-other-definition-words nil)
(defvar dyl-definition-words nil)
(defvar dyl-type-definition-pattern nil)
(defvar dyl-other-definition-pattern nil)
(defvar dyl-definition-pattern nil)
(defvar dyl-named-definition-pattern nil)
(defvar dyl-unnamed-definition-pattern nil)
(defvar dyl-type-parameterized-definition-pattern nil)
(defvar dyl-parameterized-definition-pattern nil)
(defvar dyl-end-keyword-pattern nil)
(defvar dyl-separator-word-pattern nil
  ;; Try to find a way to make these context-sensitive, so they are only
  ;; highlighted within the appropriate statements.  Unfortunately, doing this
  ;; robustly may require knowing the syntax of all statement macros and parsing
  ;; them so we don't highlight separators when they're within nested
  ;; statements.  On the other hand, we may get a lot of mileage out of just
  ;; handling all the iteration words in for statements.
  "Separator words in statement macros.")
(defvar dyl-constant-simple-definition-pattern nil)
(defvar dyl-variable-simple-definition-pattern nil)
(defvar dyl-other-simple-definition-pattern nil)
(defvar dyl-simple-definition-pattern nil)
(defvar dyl-other-pattern nil)
(defvar dyl-keyword-pattern nil)
(defvar find-keyword-pattern nil)
(defvar dylan-beginning-of-form-pattern nil)

(defun dylan-mode-init-keyword-patterns ()
  ;; Construct Dylan Mode keyword patterns (used for indenting and font-lock),
  ;; using the values of the various keyword list variables.
  
  ;; Concatenate various lists of words into other lists.
  (setq dyl-other-definition-words 
	(append dyl-unnamed-definition-words
		dyl-named-definition-words
		dyl-other-parameterized-definition-words))
  (setq dyl-definition-words 
	(append dyl-type-parameterized-definition-words
		dyl-other-definition-words))
  
  ;; Define regular expression patterns using the word lists.
  (setq dyl-type-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-type-parameterized-definition-words) "\\)"))
  (setq dyl-other-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-other-definition-words) "\\)"))
  (setq dyl-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-definition-words) "\\)"))
  (setq dyl-named-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-named-definition-words) "\\)"))
  (setq dyl-unnamed-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-unnamed-definition-words) "\\)"))
  (setq dyl-type-parameterized-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-type-parameterized-definition-words) "\\)"))
  (setq dyl-parameterized-definition-pattern
	(concat "\\("
		(apply 'make-pattern (append dyl-type-parameterized-definition-words
					     dyl-other-parameterized-definition-words))
		"\\)"))
  (setq dyl-keyword-pattern
	;; We disallow newlines in "define foo" patterns because it allows the
	;; actual keyword to be confused for a qualifier if another definition
	;; follows closely.
	(concat
	 (apply 'make-pattern
		(concat dyl-define-pattern dyl-definition-pattern)
		dyl-statement-words)
	 dyl-statement-prefixes))
  (setq dyl-end-keyword-pattern
	;; We intentionally disallow newlines in "end foo" constructs, because
	;; doing so makes it very difficult to deal with the keyword "end" in
	;; comments.
	(concat "\\bend\\b[ \t]*\\("
		(apply 'make-pattern
		       (append dyl-definition-words dyl-statement-words))
		dyl-statement-prefixes
		"\\)?"))
  (setq dyl-separator-word-pattern (apply 'make-pattern dyl-separator-words))
  (setq dyl-constant-simple-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-constant-simple-definition-words) "\\)"))
  (setq dyl-variable-simple-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-variable-simple-definition-words) "\\)"))
  (setq dyl-other-simple-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-other-simple-definition-words) "\\)"))
  (setq dyl-simple-definition-pattern
	(concat "\\(" (apply 'make-pattern
			     (append dyl-constant-simple-definition-words
				     dyl-variable-simple-definition-words
				     dyl-other-simple-definition-words)) "\\)"))
  (setq dyl-other-pattern
	(apply 'make-pattern
	       (concat "define\\([ \t\n]+\\w+\\)*[ \t\n]+"
		       dyl-simple-definition-pattern)
	       dyl-other-words))
  (setq dyl-start-expressions
	;; cpage 2007-04-06: Why are these listed here? Shouldn't we build these
	;; patterns from dyl-statement-words?
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
	  (,(concat "\\(" dyl-define-pattern "\\)?"
		    "\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*")
	   "[ \t\n]*=>[^;)]+;?")
	  (,(concat "\\(" dyl-define-pattern "\\)?"
		    "\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*")
	   "[ \t\n]*;")
	  ,(concat "define[ \t]+" dyl-named-definition-pattern
		   "[ \t\n]+[^ \t\n]+")
	  ,(concat "define[ \t]+" dyl-unnamed-definition-pattern)
	  (,(concat "\\(" dyl-define-pattern "\\)?"
		    dyl-parameterized-definition-pattern
		    "[ \t\n]+[^\( ]*[ \t\n]*")
	   "")
	  "begin"
	  "case"
	  ;; Since we don't know the syntax of all the "with(out)-" macros,
	  ;; just assume that the user has already split the line at
	  ;; the end of the header.
	  ,(concat dyl-with-statement-prefix "[^\n]*")
	  "[[({]"))
  (setq find-keyword-pattern (concat "[][)(}{\"']\\|\\bdefine\\b\\|"
				     dyl-end-keyword-pattern 
				     "\\|" dyl-keyword-pattern))
  (setq dylan-beginning-of-form-pattern (concat "[;,]\\|=>\\|"
						find-keyword-pattern
						"\\|" dyl-separator-word-pattern)))

(defun dylan-mode-init-font-lock-keywords ()
  ;; Construct Dylan Mode font-lock keyword variables, using the values of the
  ;; various pattern variables.
  
  ;; Decoration level 0: Don't highlight anything, currently -- mostly useful
  ;; for testing.  Think about how to best differentiate between 0 and 1 by
  ;; moving some keyword patterns from 1 to 0, or by moving 2 to 3 and moving
  ;; some from 1 to 2.  Also highlights anything the user adds with
  ;; dylan-add-keyword.
  (setq dylan-font-lock-keywords nil)
  
  ;; Decoration level 1: Most Dylan keywords
  (setq dylan-font-lock-keywords-1
	(append dylan-font-lock-keywords
		`(,dyl-end-keyword-pattern
		  ,dyl-keyword-pattern
		  ,dyl-separator-word-pattern
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
		  ,dyl-other-pattern
		  ;; Condition signaling function calls
		  (,(concat (regexp-opt
			     '("signal" "error" "cerror"
			       "break" "check-type" "abort")
			     'words)
			    "[ \t]*(")
		   1 font-lock-warning-face)
		  ;; Definition starts
		  (,(concat "\\b\\(" dyl-define-pattern
			    "\\(" dyl-constant-simple-definition-pattern "\\|"
			    dyl-variable-simple-definition-pattern "\\|"
			    dyl-other-simple-definition-pattern "\\)"
			    "\\)\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (7 (cond ((match-beginning 4) 'font-lock-constant-face)
			    ((match-beginning 5) 'font-lock-variable-name-face)
			    (t 'font-lock-function-name-face))))
		  (,(concat "\\b\\(" dyl-define-pattern
			    dyl-definition-pattern "\\)")
		   1 font-lock-keyword-face)
		  (,(concat "\\b\\(" dyl-define-pattern
			    "\\(" dyl-type-definition-pattern "\\|"
			    dyl-other-definition-pattern "\\)"
			    "\\)\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (6 (cond ((match-beginning 4) 'font-lock-type-face)
			    (t 'font-lock-function-name-face))))
		  ;; Local methods
		  ("method[ \t\n]+\\(\\w+\\)" 1 font-lock-function-name-face)
		  ;; Definition ends
		  (,(concat "\\bend[ \t]+\\("
			    dyl-type-definition-pattern
			    "\\|\\w*\\)\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
		   (3 (cond ((match-beginning 2) 'font-lock-type-face)
			    (t 'font-lock-function-name-face)))))))
  
  ;; Decoration level 2: Highlight all function and local variable definitions,
  ;; and, optionally, all function calls.
  (setq dylan-font-lock-keywords-2
	(append dylan-font-lock-keywords-1
		'(("slot[ \t\n]+\\(\\w+\\)" 1 font-lock-function-name-face)
		  ("block[ \t\n]+(\\([^)]+\\)" 1 font-lock-function-name-face)
		  ("let[ \t\n]+\\(\\w+\\)" 1 font-lock-variable-name-face)
		  ;; This highlights commas and whitespace separating the variable
		  ;; names. Try to find a way to highlight only the variable names.
		  ("let[ \t\n]+(\\([^)]+\\)" 1 font-lock-variable-name-face))))
  (when dylan-highlight-function-calls
    (setq dylan-font-lock-keywords-2
	  (append dylan-font-lock-keywords-2
		  ;; Function calls
		  '(("\\b\\(\\(\\s_\\|\\w\\)+\\)("
		     1 font-lock-function-name-face))))))

(defun dylan-mode-init-patterns-and-keywords ()
  ;; Construct regexp patterns and font-lock keywords, using the values of the
  ;; various keyword list variables.
  (dylan-mode-init-keyword-patterns)
  (when (fboundp 'font-lock-mode)
    (dylan-mode-init-font-lock-keywords)))

(defun look-back (regexp)
  "Attempt to find a match for REGEXP immediately preceding the
current point.  Returns t if a match was found, nil otherwise.
In order for this to work properly, the search string must end
with \"$\".  Also note that this will only work within the
current line."
  (save-excursion
    (save-restriction
      (let ((dot (point)))
	(beginning-of-line)
	(narrow-to-region dot (point))
	(re-search-forward regexp nil t)))))

(defvar find-keyword-pattern nil
  "A pattern that matches the beginnings and ends of various \"blocks\",
including parenthesized expressions.")

(defvar dylan-beginning-of-form-pattern nil
  "Like `find-keyword-pattern', but matches statement terminators as well.")

(defun dylan-header-end ()
  ;; Get the position of the end of the interchange file header. Dylan
  ;; interchange file headers end at the first empty line in the buffer
  ;; (containing no whitespace). Note that a well-formed header would match
  ;; 
  ;;   "\\`\\([a-zA-Z][-a-zA-Z0-9]*:.*\n\\([ \t]+.+\n\\)*\\)*\n"
  ;;   
  ;; but this function is only meant to partition the file into header and body
  ;; so we can handle them separately, whether they are well-formed or not.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (or (and (re-search-forward "^[ \t]*\\(\n\\|\\'\\)" nil t)
	       ;; in Emacs 18, the search just returns `t', not the point.
	       (point))
	  (point-max)))))

;; The next two routines are organized bletcherously because gnu-emacs
;; does no tail call optimization.  We used to recursively call
;; dylan-find-keyword if we found a spurious endpoint (i.e. a comma,
;; semicolon, etc.), but this exceeded the maximum emacs stack depth
;; (which is clearly pretty low).
;;
(defun dylan-find-keyword (&optional match-statement-end in-case no-commas
				     start)
  "Move the point backward to the beginning of the innermost
enclosing compound statement or set of parentheses.  Return t on
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
				 find-keyword-pattern) header-end t)
	   (cond (;; Skip backwards over eol comments.
		  (look-back dylan-comment-pattern)
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
		       (not (looking-at dyl-keyword-pattern)))
		  nil)
		 ;; Skip backward over blocks/statements that end with "end".
		 ((or (looking-at "end")	       ; Point is either before or
		      (and (look-back "\\bend[ \t]*$") ; after "end".
			   (backward-word 1)))
		  (dylan-find-keyword)	; Search for the start of the block.
		  ;; cpage 2007-05-17: Why does this check for "method" and
		  ;; "define"? Should it also check for "define...function"?
		  ;; What about "define...class", etc.?
		  (if (or (and (looking-at "method")
			       (look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$"))
			  (looking-at "define"))
		      nil
		    'not-found))
		 ;; cpage 2007-05-17: What is this for? Does it handle `until:'
		 ;; and `while:' within `for' iteration clauses? Shouldn't it
		 ;; test for the keywords with the colon?
		 ;; 
		 ;; hack for overloaded uses of "while" and "until" reserved words
		 ((or (looking-at "until") (looking-at "while"))
		  (if (save-excursion
			(condition-case nil
			    (progn 
			      (backward-up-list 1)
			      (backward-sexp 1)
			      (looking-at "for\\b")) (error nil)))
		      (backward-up-list 1))
		  t)
		 ;; Statement macro separator words.
		 ((and (looking-at dyl-separator-word-pattern)
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
compound statement or set of parentheses.  Returns t on success
and nil otherwise."
  (let ((result 'not-found))
    (while (eq result 'not-found)
      (setq
       result
       (if (re-search-forward (if match-statement-end
				  dylan-beginning-of-form-pattern
				find-keyword-pattern) nil t)
	   (let ((match-start (match-beginning 0)))
	     (cond ((look-back dylan-comment-pattern)
		    (forward-line)
		    'not-found)
		   ((look-back "[[({'\"]$")
		    (condition-case nil
			(progn 
			  (backward-char 1)
			  (forward-sexp 1)
			  'not-found)
		      (error nil)))
		   ((look-back "[])}]$") t)
		   ((look-back "define$") ; special case for top-level forms
		    (dylan-find-end t nil nil)
		    nil)
		   ((look-back "\\bend\\([ \t]+\\w+\\)?$")
		    (if (and (not (looking-at "[ \t]+\\(end\\|=>\\)\\b"))
			     (looking-at "[ \t]+\\w+"))
			(goto-char (match-end 0)))
		    t)
		   ;; hack for overloaded uses of "while" and "until" reserved
		   ;; words
		   ((look-back "until$\\|while$")
		    (if (save-excursion
			  (condition-case nil
			      (progn 
				(backward-up-list 1)
				(backward-sexp 1)
				(looking-at "for\\b")) (error nil)))
			(up-list 1))
		    t)
		   ((save-excursion (goto-char match-start)
				    (looking-at dyl-separator-word-pattern))
		    t)
		   ((look-back ";$")
		    (if (not match-statement-end)
			'not-found
		      t))
		   ((look-back ",$")
		    (if (or (not match-statement-end) no-commas)
			'not-found
		      t))
		   ((look-back "=>$")
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
  "Utility function for `dylan-skip-whitespace-backward'.  Find
beginning of enclosing \"/*\" comment.  Deals properly with
nested \"/*\" and with \"//\"."
  (re-search-backward "/\\*\\|\\*/")
  (while (cond ((look-back dylan-comment-pattern)
		(goto-char (match-beginning 0)))
	       ((looking-at "\\*/")
		(dylan-skip-star-comment-backward))
	       (t nil))
    (re-search-backward "/\\*\\|\\*/"))
  t)

(defun dylan-skip-star-comment-forward ()
  "Utility function for `dylan-skip-whitespace-forward'.  Find
end of enclosing \"/*\" comment.  Deals properly with nested
\"/*\" and with \"//\"."
  (re-search-forward "/\\*\\|\\*/")
  (while (cond ((look-back dylan-comment-pattern)
		(end-of-line))
	       ((look-back "/\\*$")
		(dylan-skip-star-comment-forward))
	       (t nil))
    (re-search-forward "/\\*\\|\\*/"))
  t)

(defvar non-whitespace-string
  "\\s_\\|\\s(\\|\\s\"\\|\\s$\\|\\s<\\|\\s/\\|\\sw\\|\\s.\\|\\s)\\|\\s'\\|\\s\\"
  "A magic search string that matches everything but whitespace.  Used
because old versions of emacs don't have `skip-syntax-backward'.")

(defun dylan-skip-whitespace-backward ()
  "Skips over both varieties of comments and other whitespace characters."
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end)))
    (unless (< (point) header-end)
      ;; skip syntactic whitespace
      (if (re-search-backward non-whitespace-string header-end t)
	  (forward-char)
	(goto-char header-end))
      ;; skip comments
      (while (cond ((look-back dylan-comment-pattern)
		    (goto-char (match-beginning 0)))
		   ((look-back "\\*/$")
		    (goto-char (match-beginning 0))
		    (dylan-skip-star-comment-backward))
		   (t nil))
	(if (re-search-backward non-whitespace-string header-end t)
	    (forward-char)
	  (goto-char header-end))))))

(defun dylan-skip-whitespace-forward ()
  "Skips over both varieties of comments and other whitespace characters."
  ;; skip syntactic whitespace
  (re-search-forward "\\(\\s \\|\\s>\\)*")
  ;; skip comments
  (while (cond ((looking-at dylan-comment-pattern)
		(goto-char (match-end 0))
		t)
	       ((looking-at "/\\*")
		(goto-char (match-end 0))
		(dylan-skip-star-comment-forward))
	       (t nil))
    (re-search-forward "\\(\\s \\|\\s>\\)*")))

(defun aux-find-body-start (clauses)
  "Helper function for `find-body-start'"
  (save-excursion
    (cond ((null clauses) (point))
	  ((looking-at (car clauses))
	   (if (null (cdr clauses))
	       (match-end 0)
	     (goto-char (match-end 0))
	     (and (looking-at "[[({]")
		  (condition-case nil (forward-list) (error nil))
		  (aux-find-body-start (cdr clauses))))))))

(defun find-body-start (exprs)
  "When passed `dyl-start-expressions', processes it to find the
beginning of the first statement in the compound statement that
starts at the current point."
  (cond ((null exprs) (point-max))
	((listp (car exprs))
	 (or (aux-find-body-start (car exprs)) (find-body-start (cdr exprs))))
	(t (if (looking-at (car exprs))
	       (match-end 0)
	     (find-body-start (cdr exprs))))))

(defun backward-dylan-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the previous statement
and the current one.  If we are already between statements, move back one 
more."
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end)))
    (unless (< (point) header-end)
      (unwind-protect
	  ;; Because "\b" doesn't work with "symbol-chars" we temporarily
	  ;; install a new syntax table and restore the old one when done
	  (set-syntax-table dylan-indent-syntax-table)
	(dylan-skip-whitespace-backward)
	(let* ((dot (point)))
	  ;; skip over "separator words"
	  (when (save-excursion
		  (and (re-search-backward dyl-separator-word-pattern header-end t)
		       (if (not (looking-at "exception\\|elseif"))
			   (forward-word 1)
			 (goto-char (match-end 0))
			 (condition-case nil (forward-list 1)
			   (error nil))
			 t)
		       (>= (point) dot)))
	    (re-search-backward dyl-separator-word-pattern header-end t)
	    (dylan-skip-whitespace-backward))
	  (if (look-back "[,;]$\\|=>$")
	      (backward-char))
	  (cond ((not (dylan-find-keyword t in-case no-commas))
		 (if (look-back "\\(define\\|local\\)[ \t]+") ; hack
		     (goto-char (match-beginning 0))))
		((looking-at dyl-separator-word-pattern)
		 (let ((start (point)))
		   (cond ((looking-at "\\(exception\\|elseif\\)[ \t\n]*(")
			  (goto-char (match-end 1))
			  (condition-case nil (forward-list 1)
			    (error nil)))
			 (t (forward-word 1)))
		   (if (>= (point) dot)
		       (progn (goto-char start)
			      (backward-dylan-statement in-case no-commas)))))
		((looking-at "[;,]\\|=>")
		 (goto-char (match-end 0)))
		(t
		 ;; check whether we were already at the first "form" in an
		 ;; enclosing block
		 (let ((first (find-body-start dyl-start-expressions)))
		   (if (< first dot)
		       (goto-char first)
		     (if (look-back "\\(define\\|local\\)[ \t]+") ; hack
			 (goto-char (match-beginning 0))))))))
	(set-syntax-table dylan-mode-syntax-table)))))

(defun dylan-beginning-of-form ()
  "Finds the beginning of the innermost statement that contains or
terminates at the current point."
  (interactive)
  (backward-dylan-statement)
  (dylan-skip-whitespace-forward))

(defun forward-dylan-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the current statement
and the next one.  If we are already between statements, move forward one 
more."
  (unwind-protect
      ;; Because "\b" doesn't work with "symbol-chars" we temporarily
      ;; install a new syntax table and restore the old one when done
      (progn
	(set-syntax-table dylan-indent-syntax-table)
	(dylan-skip-whitespace-forward)
	(let* ((dot (point)))
	  ;; skip over "separator words"
	  (if (looking-at dyl-separator-word-pattern)
	      (if (not (looking-at "exception\\|elseif"))
			 (forward-word 1)
		       (goto-char (match-end 0))
		       (condition-case nil (forward-list 1)
			 (error nil))))
	  (cond ((not (dylan-find-end t in-case no-commas))
		 (if (look-back "\\(define\\|local\\)[ \t]+")	; hack
		     (goto-char (match-beginning 0))))
		(t)))
	(cond ((looking-at "[,;]$") (forward-char))
	      ((looking-at "=>") (forward-word 1))))
    (set-syntax-table dylan-mode-syntax-table)))

(defun dylan-end-of-form ()
  "Finds the end of the innermost statement that contains or begins
at the current point."
  (interactive)
  (forward-dylan-statement))

(defun indent-if-continuation (term-char line-start block-start
					 &optional in-case in-paren)
  (save-excursion
    (goto-char line-start)
    (let ((arrow (looking-at "=>")))
      (dylan-skip-whitespace-backward)
      (if (look-back "finally$")	; special case -- this one is tricky
	  0				; because "for" can have empty bodies
	(let ((real-start (point)))
	  (backward-dylan-statement in-case)
	  (unless (= (point) real-start) ; make sure we actually went back a statement
	    (dylan-skip-whitespace-forward))
	  (cond ((and (= block-start 0) (not (looking-at "define")))
		 0)			; special case for beginning of file
		((= real-start block-start) 0)
		((< (point) block-start)
		 (+ dylan-indent (if (and arrow (not in-case)) -3 0)))
		;; Indent keyword args to line up with the first arg after #key.
		((looking-at "#key[ \t]+")
		 (- (match-end 0) (match-beginning 0)))
		;; What does this do?
		((< (save-excursion
		      (forward-dylan-statement in-case
					       (equal term-char ";"))
		      (point)) line-start)
		 0)
		;; Comma-separated lists of local methods should be indented to
		;; line up with the first method when it begins on the same line
		;; as "local".
		((looking-at "local[ \t]+")
		 (- (match-end 0) (match-beginning 0)))
		;; Give continuations of generic functions extra indentation to
		;; match what happens with method declarations.  This is an odd
		;; special case, but some folks like it.  If you don't, comment
		;; out the next 3 lines.
		((looking-at
		  "define\\([ \t\n]+\\w+\\)*[ \t]+generic")
		 (+ dylan-indent dylan-indent (if arrow -3 0)))
		(t dylan-indent)))))))

(defun dylan-indent-to-column (column)
  "Add or remove whitespace to indent the current line to column COLUMN."
  (unless (= column (current-indentation))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to-column column))))

(defun dylan-indent-line (&optional ignore-case extra-indent)
  "Indents a line of dylan code according to its nesting."
  ;; The "ignore-case" and "extra-indent" vars are used for recursive
  ;; calls so that the special code for handling case statements won't
  ;; recurse infinitely.
  (interactive)
  (setq extra-indent (or extra-indent 0))
  (unwind-protect
      ;; If we're inside the interchange file header, let the user indent as
      ;; they please. If we're indenting a region (i.e., if this function wasn't
      ;; called interactively), leave the header indenting as-is.
      (if (< (point) (dylan-header-end))
	  (when (interactive-p)
	    (when (<= (current-column) (current-indentation))
	      (back-to-indentation))
	    (insert-char ?\  dylan-indent)) ; Using "?\ " instead of "?\s" for
					    ; compatibility with older Emacsen.
	(save-excursion
	  ;; Because "\b" doesn't work with "symbol-chars" we temporarily
	  ;; install a new syntax table and restore the old one when done
	  (set-syntax-table dylan-indent-syntax-table)
	  ;; Move point to the end of the current indentation. This allows us to
	  ;; use looking-at to examine the start of the current line of code
	  ;; without having to put whitespace at the start of all the patterns.
	  (back-to-indentation)
	  (let* ((body-start)		; Beginning of "body" of enclosing
					; compound statement.
		 (in-paren)		; t if in parenthesized expression.
		 (paren-indent 0)       ; Indentation of first non-space after open paren.
		 (in-case)		; t if in "case" or "select" statement.
		 (block-indent		; Indentation of enclosing compound statement.
		  (save-excursion
		    (if (not (dylan-find-keyword))
			nil
		      (and (looking-at "method")
			   (look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$")
			   (goto-char (match-beginning 0)))
		      (and (looking-at "[[({]")
			   (setq in-paren t)
			   (save-excursion
			     (let ((dot (point)))
			       (forward-char)
			       (re-search-forward "[^ \t]")
			       (setq paren-indent (- (point) dot 1)))))
		      (and (looking-at "select\\|case") (setq in-case t))
		      (setq body-start (find-body-start dyl-start-expressions))
		      (+ (current-column) extra-indent))))
		 (indent		; correct indentation for this line
		  (cond ((not block-indent)
			 (indent-if-continuation ";" (point) 0))
			;; some keywords line up with start of comp. stmt 
			((looking-at dyl-separator-word-pattern) block-indent)
			;; end keywords line up with start of comp. stmt 
			((looking-at dyl-end-keyword-pattern) block-indent)
			;; parenthesized expressions (separated by commas)
			(in-case
			 ;; if the line is blank, we pick an arbitrary
			 ;; indentation for now.  We judge the "proper"
			 ;; indentation by how the statement is punctuated once
			 ;; it is finished
			 (cond ((looking-at "^$")
				(if (save-excursion
				      ;; Look for end of prev statement.  This
				      ;; is hairier than it should be because
				      ;; we may be at the end of the buffer
				      (let ((dot (point)))
					(forward-dylan-statement t)
					(dylan-skip-whitespace-backward)
					(if (> (point) dot)
					    (backward-dylan-statement t))
					(look-back ";$\\|=>$")))
				    (+ block-indent dylan-indent dylan-indent
				       (indent-if-continuation "," (point)
							       body-start t))
				  (+ block-indent dylan-indent 
				     (indent-if-continuation "," (point)
							     body-start t))))
			       ((save-excursion
				  (forward-dylan-statement t)
				  (look-back ",$\\|=>$"))
				(+ block-indent dylan-indent 
				   (indent-if-continuation "," (point)
							   body-start t)))
			       (t (+ block-indent dylan-indent dylan-indent
				     (indent-if-continuation "," (point)
							     body-start t)))))
			(in-paren (+ block-indent paren-indent
				     (indent-if-continuation "," (point)
							     body-start)))
			;; statements (separated by semi-colons)
			(t (+ block-indent dylan-indent
			      (indent-if-continuation ";" (point)
						      body-start))))))
	    (dylan-indent-to-column indent))))
    ;; put the cursor where the user is likely to want it.
    (let ((col (current-indentation)))
      (when (< (current-column) col)
	(move-to-column col)))
    (set-syntax-table dylan-mode-syntax-table)))

(defun in-case ()
  "Return t if point is immediately within a \"case\" or \"select\"
statement, nil otherwise.  Used to provide special re-indentation
for \",\", \";\", and \"=>\"."
  (save-excursion
    (dylan-find-keyword)
    (looking-at "case\\|select")))

(defun reindent-line ()
  (interactive)
  (save-excursion (funcall indent-line-function)))

(defun dylan-newline-and-indent ()
  "Insert a newline and then indent the new line."
  (interactive)
  (expand-abbrev)
  (newline-and-indent))

(if (and (boundp 'running-lemacs) running-lemacs)
    (defun this-command-chars ()
      (events-to-keys (this-command-keys)))
  (defun this-command-chars ()
    (this-command-keys)))

(defun dylan-insert-and-indent ()
  "Make \";\" and \",\" do re-indentation for case statements."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    ;; These things are finicky around EOF, so use "point-marker" instead
    ;; of "point" so that re-indentations won't yield infinite loops.
    (let ((dot (point-marker)))
      (beginning-of-line)
      (if (in-case)
	  (progn
	    (backward-dylan-statement t)
	    (dylan-skip-whitespace-forward)
	    (while (< (point) (marker-position dot))
	      (funcall indent-line-function)
	      (forward-line 1)))))))

(defun dylan-arrow-insert ()
  "Make \"=>\" do re-indentation for case statements and function declarations."
  (interactive)
  (if (not (= (preceding-char) ?=))
      (self-insert-command 1)
    (self-insert-command 1)
    (save-excursion
      (if (in-case)
	  (let ((dot (point-marker)))
	    (backward-dylan-statement t)
	    (dylan-skip-whitespace-forward)
	    (while (< (point) (marker-position dot))
	      (funcall indent-line-function)
	      (forward-line 1)))
	(funcall indent-line-function)))))


;; This intensely DWIMish function tries to insert whatever text is needed to
;; finish off the enclosing indentation context.
(defun dylan-insert-block-end ()
  "Insert whatever text is needed to finish off the enclosing indentation
context (e.g. \"end method foo;\").  Makes educated guesses about whether
newlines and closing punctuation are needed."
  (interactive)
  (let* ((here (point))
	 (terminator)
	 (need-newline)
	 (str
	  (unwind-protect
	      (save-excursion
		;; Because "\b" doesn't work with "symbol-chars" we temporarily
		;; install a new syntax table and restore the old one when done
		(set-syntax-table dylan-indent-syntax-table)
		(if (not (dylan-find-keyword))
		    (error "No nesting block."))
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
			     dyl-definition-pattern))	; find the actual word
		    (goto-char (match-beginning 2)))
		(cond ((looking-at "begin") (concat " end" terminator))
		      ((looking-at "\\[") "]")
		      ((looking-at "(") ")")
		      ((looking-at "{") "}")
		      ((or (looking-at "\\(method\\|function\\|class\\|C-subtype\\|C-mapped-subtype\\)\\([ \t]+\\w+\\)?")
			   (looking-at "\\(library\\|module\\)[ \t]+\\w+")
			   (looking-at "\\w+"))
		       (concat " end "
			       (buffer-substring (match-beginning 0)
						 (match-end 0))
			       terminator))
		      (t (concat " end" terminator))))
	    (set-syntax-table dylan-mode-syntax-table))))
    (if need-newline
	(progn
	  (beginning-of-line)
	  (if (looking-at "[ \t]*$")
	      (delete-horizontal-space)
	    (end-of-line)
	    (newline))))
    (let* ((start (point))
	   (end (progn (insert str) (point))))
      (goto-char start)
      (while (re-search-forward "[ \t\n]+" end t)
	(replace-match " "))
      (goto-char end)
      (reindent-line))))


;; Regular expression for finding the beginning of a definition.
(defvar dylan-defun-regexp)
(setq dylan-defun-regexp
      "^ *\\(define \\|module:\\|interface \\)")

(defvar dylan-defun-end-regexp) ; end of a routine
(setq dylan-defun-end-regexp "^ *end\\s-")

(defun dylan-beginning-of-defun (&optional arg)
  "Move backward to next beginning of definition.
With ARG, do this ARG times.  Returns t unless search stops due
to end of buffer."
  (interactive "p")
  ;; don't go back into the interchange file header
  (let ((header-end (dylan-header-end)))
    (unless (< (point) header-end)
      (and arg (< arg 0) (forward-char 1))
      (and (or (re-search-backward dylan-defun-regexp header-end t (or arg 1))
	       ;;(re-search-backward
	       ;;  "^\\(type\\|const\\|var\\|%include\\)\\s-"
	       ;;  nil 'move (or arg 1))
	       )
	   (progn (beginning-of-line) t)))))

(defun dylan-end-of-defun (&optional arg)
  "Move forward to next end of function."
  (interactive "p")
  (let ((dot (point)))
    (dylan-skip-whitespace-forward)
    (end-of-line)
    (let ((next-begin
	   (and (re-search-forward dylan-defun-regexp nil 'move 1)
		;; in Emacs 18, the search just returns `t', not the point.
		(point))))
      (if next-begin
	  (progn
	    (beginning-of-line)
	    (let ((last-end
		   (and (re-search-backward dylan-defun-end-regexp nil 'move 1)
			(point))))
	      (if (and last-end
		       (< last-end next-begin)
		       (> last-end dot))
		  (progn
		    (goto-char last-end)
		    (end-of-line)
		    (forward-char)
		    )
		(progn
		  (goto-char next-begin)
		  (beginning-of-line)
		  (backward-char)
		  )
		))
	    (if (and arg (> arg 1))
		(dylan-end-of-defun (1- arg)))
	    t)))))

(defun dylan-mark-function ()
  "Put mark at end of Dylan function, point at beginning."
  (interactive)
  (beginning-of-line)
  (dylan-end-of-defun)
  (push-mark (point) t t)
  (dylan-beginning-of-defun))


(defun dylan-font-lock-fontify-region (beg end loudly)
  "Dylan Mode font lock fontification. Handles fontification of
interchange file headers separately from the file body; they have
entirely separate character and keyword syntaxes.

This is particularly important since headers can contain
apostrophes, for example, that would otherwise confuse the
first-pass, character syntax-based fontification and cause it to
treat code in the file body as the interior of a string*.

*In fact, this is still a problem with older Emacsen that don't
 support the `font-lock-dont-widen' variable."
  (let ((header-end (dylan-header-end)))
    ;; If the region overlaps the header, fontify the header with the
    ;; appropriate keyword patterns and character syntax table.
    (when (< beg header-end)
      (let ((end (min end header-end))
	    (save-font-lock-dont-widen font-lock-dont-widen)
	    (save-font-lock-keywords font-lock-keywords)
	    (save-font-lock-keywords-only font-lock-keywords-only))
	(save-restriction
	  (narrow-to-region 1 end)
	  (setq font-lock-dont-widen t)
	  (setq font-lock-keywords dylan-font-lock-header-keywords)
	  (setq font-lock-keywords-only t)
	  (unwind-protect
	      (font-lock-default-fontify-region beg end loudly)
	    (setq font-lock-dont-widen save-font-lock-dont-widen)
	    (setq font-lock-keywords save-font-lock-keywords)
	    (setq font-lock-keywords-only save-font-lock-keywords-only)))))
    ;; Fontify the Dylan code. We narrow the buffer to exclude the header from
    ;; character syntactic fontification.
    (when (> end header-end)
      (let ((beg (max beg header-end))
	    (save-font-lock-dont-widen font-lock-dont-widen))
	(save-restriction
	  (narrow-to-region header-end (point-max))
	  (setq font-lock-dont-widen t)
	  (unwind-protect
	      (font-lock-default-fontify-region beg end loudly)
	    (setq font-lock-dont-widen save-font-lock-dont-widen)))))))

(defun dylan-find-slime-buffer-package ()
  (let ((case-fold-search t)
        (regexp "^module:[ \t]*\\([^ \n\r\t]+\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))


(defun dylan-mode-init-variables ()
  ;; Use value appropriate for font-lock mode now.  Reset after running hooks.
  ;; 
  ;; cpage 2007-04-23: Why do this?
  ;; 
  (when (fboundp 'font-lock-mode)
    (setq font-lock-keywords dylan-font-lock-keywords))
  (if (not (boundp 'font-lock-syntax-table))
      (set-syntax-table dylan-indent-syntax-table)
    (make-local-variable 'font-lock-syntax-table)
    (setq font-lock-syntax-table dylan-indent-syntax-table))
  (set-syntax-table dylan-indent-syntax-table)
  
  ;; font-lock-dont-widen is a feature in the post-21.3 version of emacs, so
  ;; make sure it exists before attempting to use it. As I am writing this,
  ;; 21.3 is the current version and this variable only exists in development
  ;; builds. Unfortunately, if it doesn't exist, we don't get proper
  ;; suppression of interchange file header fontification, but defining our
  ;; own at least makes dylan-font-lock-fontify-region silently overlook this
  ;; rather than fail with an "undefined variable" error.
  (unless (boundp 'font-lock-dont-widen)
    (make-local-variable 'font-lock-dont-widen)
    (setq font-lock-dont-widen nil))
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'dylan-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//+[ \t]*\\|/\\*[ \t]*")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq local-abbrev-table dylan-mode-abbrev-table)
  (if (not dylan-mode-for-emacs-21-and-later)
      (progn
	(make-local-variable 'after-change-function)
	(setq after-change-function nil))
    (progn
      (make-local-variable 'after-change-functions)
      (if (not (fboundp 'after-change-functions))
	  (setq after-change-functions nil))))
  (make-local-variable 'after-change-function)
  (setq after-change-function nil)
  (when dylan-mode-for-emacs-21-and-later
    (set-syntax-table dylan-indent-syntax-table)
    (dylan-set-up-syntax-tables)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
	  `((dylan-font-lock-keywords
	     dylan-font-lock-keywords-1
	     dylan-font-lock-keywords-2)
	    nil t nil nil
	    (font-lock-fontify-region-function
	     . dylan-font-lock-fontify-region)
	    (font-lock-syntax-table
	      . ,dylan-indent-syntax-table))))
  (make-local-variable 'slime-buffer-package)
  (setq slime-buffer-package (dylan-find-slime-buffer-package))
  (if (fboundp 'slime-mode)
      (slime-mode))
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'dylan-mode-hook)
    (run-hooks 'dylan-mode-hook))	; For compatibility with older Emacsen,
					; fall back to `run-hooks'.
  ;; This is the table the user should always see, even though the indent and
  ;; font lock code both reset it temporarily.
  (set-syntax-table dylan-mode-syntax-table))


;;; The command to invoke Dylan mode

(defun dylan-mode ()
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
  (interactive)
  (kill-all-local-variables)
  (use-local-map dylan-mode-map)
  (dylan-mode-init-patterns-and-keywords)
  (dylan-mode-init-variables)
  (setq major-mode 'dylan-mode
	mode-name "Dylan"
	local-abbrev-table dylan-mode-abbrev-table
	abbrev-mode t))


;;; Dylan mode load-time initialization

;; Map Dylan file extensions to Dylan Mode. (Is ".input" unique enough for us to
;; automatically map it to Dylan Mode, or should we omit it and let users add it
;; if desired?)
(add-to-list 'auto-mode-alist
	     '("\\.\\(dylan\\|intr\\|input\\)\\'" . dylan-mode))

;; We must use the "indentation" syntax table when doing font-lock
;; processing.  In modern EMACSen (xemacs 19.14, FSF 19.30), the
;; font-lock-syntax-table variable handles this for us.  In older
;; EMACSen, we need some pretty ugly hacks -- the only thing to be said
;; in their favor is that they often work.
(if (and (fboundp 'font-lock-mode)
	 (require 'font-lock)		; force load to test version
	 (not (boundp 'font-lock-syntax-table)))
    (progn
      (defvar old-after-change-function nil
	"Used to modify the behavior of Font Lock mode.")
      (defun dm-after-change-function (&rest args)
	(let ((old-syntax-table (syntax-table)))
	  (unwind-protect
	      (progn
		(set-syntax-table dylan-indent-syntax-table)
		(apply old-after-change-function args))
	    (set-syntax-table old-syntax-table))))
      ;; More hacks to magically switch syntax tables as necessary
      (if (boundp 'font-lock-after-fontify-buffer-hook)
	  (progn ; new way (in XEmacs 19.13)
	    (add-hook
	     'font-lock-mode-hook
	     '(lambda ()
		(if (not (eq major-mode 'dylan-mode))
		    nil
		  (setq font-lock-keywords dylan-font-lock-keywords-2)
		  ;; This is to handle fontification updates while editing:
		  (if (not dylan-mode-for-emacs-21-and-later)
		      (progn
			(make-local-variable 'old-after-change-function)
			(setq old-after-change-function
			      'font-lock-after-change-function)
			(make-local-variable 'after-change-function)
			(setq after-change-function 'dm-after-change-function))
		    (setq after-change-functions 
			  (cons 'dm-after-change-function after-change-functions)))
		  (make-local-variable 'old-after-change-function)
		  (setq old-after-change-function
			'font-lock-after-change-function)
		  (make-local-variable 'after-change-function)
		  (setq after-change-function 'dm-after-change-function)
		  ;; And this is for the initial processing of the file:
		  (set-syntax-table dylan-indent-syntax-table)
		  )))
	    (add-hook
	     'font-lock-after-fontify-buffer-hook
	     '(lambda ()
		(if (eq major-mode 'dylan-mode) ; restore normal syntax
		    (set-syntax-table dylan-mode-syntax-table)))
	     t)))))
	;; else older version of font-lock-mode
	;; (but this doesn't seem to work)
	;;(add-hook
	;; 'font-lock-mode-hook
	;; '(lambda ()
	;;    (if (not (eq major-mode 'dylan-mode))
	;;	nil
	;;      (setq font-lock-keywords dylan-font-lock-keywords)
	;;      (make-local-variable 'old-after-change-function)
	;;      (setq old-after-change-function 'font-lock-after-change-function)
	;;      (make-local-variable 'after-change-function)
	;;      (setq after-change-function 'dm-after-change-function)))))))

(provide 'dylan-mode)

;;; dylan-mode.el ends here.
