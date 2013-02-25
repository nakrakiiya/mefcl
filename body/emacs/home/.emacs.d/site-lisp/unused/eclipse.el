;       $Id: eclipse.el,v 7.46 2010/01/12 20:06:49 thorsten Exp $   

;;; eclipse.el --- major mode for editing and running ECLiPSe under Emacs

;; Copyright (C) 1986, 1987, 2001 - 2009
;;   Free Software Foundation, Inc.

;; Author: Thorsten Winterer <thorsten.winterer@acm.org>
;; based on the ECLiPSe mode from
;; Helmut Simonis <Helmut.Simonis@parc-technologies.com>
;; which was based on the prolog mode from
;; Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a major mode for editing ECLiPSe. It knows
;; about ECLiPSe syntax and comments, and can send regions to an inferior
;; ECLiPSe interpreter process.

;; The main difference to the previous Prolog mode is the extensive syntax
;; colouring done for ECLiPSe.
;; It supports in particular the comment/2 facility by highlighting
;; sections and pieces of the text

;; the system also knows about C-style comments
;; and indents properly after a ';'

;; Function keys:

;; TAB indents the current line.
;; M-C-\ indent a region
;; S-TAB inserts TAB character. Also \C-c<SPC>
;; C-c-p-TAB indents the current predicate
;; C-c-q-TAB indents the current clause
;; C-c-b-TAB indents the current clause

;; M-[, M-], C-c[, C-c] all call dabbrev-expand or dabbrev-completion,
;;    but check predefined ECLiPSe keywords first
;;    however, there are slight differences in there behaviour:
;; M-[ calls dabbrev-expand, and returns the predicate template
;; M-] calls dabbrev-completion, and returns the predicate template
;; C-c-[ calls dabbrev-expand, and returns the predicate name + (
;; C-c-] calls dabbrev-completion, and returns the predicate name + (

;; C-c-l loads all (local) project files mentioned in the current ECLiPSe
;;    buffer
;; C-c-C-l loads all (local) project files mentioned in any ECLiPSe buffer

;; C-l re-centres and re-fontifies
;; C-c-c comments out a region
;; C-c-r uncomments a region
;; C-c-i inverts the commenting of a region
;; C-c-C-f toggles the auto-line-break mode
;; C-c-C-j toggles the auto-indent mode
;; C-c-C-j toggles the auto-indent mode

;; C-c-m-b marks the buffer
;; C-c-m-p marks the predicate
;; C-c-m-q marks the clause

;; M-C-a jumps to the next beginning of a predicate
;; M-C-e jumps to the next end of a predicate
;; M-a jumps to the next beginning of a clause
;; M-e jumps to the next end of a clause
;; C-c-C-z toggles the quick-jumps-mode
;; C-c-C-r toogles the repeat / ! extra indentation

;; C-c-t inserts the template of the current predicate
;; C-c-s inserts the specs of the current predicate
;; M-RET inserts a new clause head
;; M-C-i inserts a new clause head without arguments

;; C-c-/ insert a short comment/2 template
;; C-c-\ insert a full comment/2 template

;; C-c-a anonymises the variables in the region
;; C-c-C-a replaces the variables in the region with anonymous variables

;; C-c-C-e starts an inferior ECLiPSe process
;; C-c-C-b compiles the buffer
;; C-c-C-v compiles a region
;; C-c-C-y compiles a region and switches to the ECLiPSe process
;; C-c-C-g passes a (region as) command to the ECLiPSe process
;; C-c-C-q stops the ECLiPSe process
;; C-c-C-k kills the ECLiPSe process
;; C-c-C-t starts the ECLiPSe-Tk-Tools
;; C-c-C-s toggles the source code tracing. This needs ECLiPSe 6.0 or higher
;; C-c-x toggles a breakpoint. This needs source code tracing to be switched on.

;; C-c-v-b checks the buffer for syntax errors by compiling
;;    it in an ECLiPSe process. The error messages are parsed and made
;;    clickable to link to the position in the source code
;;    however, this is not always possible
;; C-c-v-v checks the current region
;; C-c-v-p checks the current predicate
;; C-c-v-q checks the current clause
;; C-c-v-l checks the file in current buffer using the lint library

;; C-c-h highlights the current word
;; C-c-d removes any highlighting
;; C-c-> moves to next occurrence of highlighted word
;; C-c-< moves to last occurrence of highlighted word

;; C-c-C-h calls the ECLiPSe help function

;; C-c-l counts lines and comments in the current buffer
;; C-c-C-l counts lines and comments in all ECLiPSe buffers

;; C-c-@-@ marks the current outline subtree
;; C-c-@-n jumps to the next visible outline heading
;; C-c-@-p jumps to the previous visible outline heading
;; C-c-@-u jumps to the outline heading one level above
;; C-c-@-f jumps to the next outline heading on the same level
;; C-c-@-b jumps to the previous outline heading on the same level
;; C-c-@-h hides all predicates
;; C-c-@-t hides the current predicate
;; C-c-@-c hides all clause bodies
;; C-c-@-e hides the current clause body
;; C-c-@-l or C-c-- hides the current block
;; C-c-@-a shows all
;; C-c-@-s shows all predicates (synonymous with C-c-@-a)
;; C-c-@-r shows the current predicate
;; C-c-@-d shows all clauses (synonymous with C-c-@-a)
;; C-c-@-m shows the current clause
;; C-c-@-k or C-c-+ shows the current block

;; Variables:

;; eclipse-indent-width   Describes the number of space characters inserted
;;    when increasing the indentation. the default value is 4.
;; eclipse-esp-indent-width   Describes the number of space characters inserted
;;    at the beginning of the line in ESP mode. the default value is 2.
;; eclipse-tab-width   Describes the number of space characters inserted at
;;    the beginning of a line, if its indented. The default is 8.
;; eclipse-indent-closing-parenthesis-to-match-opening   If t, the closing
;;    parenthesis always is indented to the same column as the opening
;;    parenthesis. If nil, the closing parenthesis matches either the opening
;;    parenthesis if this is a 'stand-alone' parenthesis, or the column of
;;    the first letter of the corresponding predicate call. The default is t.
;; eclipse-indent-to-parenthesis   If non-nil, indentation of the body of
;;    if-then-clauses or for-loops is calculated from the preceding opening
;;    paranthesis. Otherwise is calculated from the column of the 
;;    if-clause/for-clause. The default is t.
;; eclipse-indent-after-repeat   If non-nil, text between repeat/0 and !/0
;;    will be indented an extra level
;; eclipse-tab-mode   If non-nil, tabs are used for indentation, otherwise
;;    space characters only. The default is nil.
;; eclipse-autolinebreak-selected   If non-nil, auto-line-break is used.
;;    The default is t.
;; eclipse-autoindent-selected   If non-nil, auto-indent is used.
;;    The default is t.
;; eclipse-quick-jumps-selected   If non-nil, quick jumps are used.
;;    The default is nil.
;; eclipse-font-lock-default  Contains the default level for the
;;    fontification. The default is 3.

;; There are more customisable variables, but they are less likely to be
;; changed. Check the customisation options for group eclipse.

;; There used to be a variable eclipse-backtab that described the
;;    the key-sequence for "backtab". This seems to depend on what Emacs
;;    and what GUI you use. With this key, additional tab characters
;;    (or equivalent space characters) are inserted. However, customisation
;;    did not work properly, so I removed the variable. 
;;    If [backtab] does not work, you now have to change the value directly
;;    in function eclipse-mode-commands in the line 
;;
;;    (define-key map [backtab] 'eclipse-insert-tab)
;;
;;    You can try [S-kp-tab] or [S-tab] instead. 

;; Running of ECLiPSe in an inferior mode has not been thoroughly tested,
;; I normally use the tkeclipse environment.

;; Opening the speedbar from within the ECLiPSe mode will automatically add
;; .ecl to the supported extensions.
;; If you want to load the speedbar automatically when starting Emacs, add
;;
;; (speedbar)
;; (speedbar-add-supported-extension ".ecl")
;;
;; to your .emacs file.
;; If you do not load speedbar automatically but open one before loading
;; the first ECLiPSe file, you have to add .ecl to the list of supported
;; extensions by either calling (speedbar-add-supported-extension ".ecl")
;; or add
;;
;;(custom-set-variables
;;  '(speedbar-supported-extension-expressions
;;     (quote (".ecl" <whatever is in the variable now>))))
;;
;; to your .emacs file.
;;
;;
;; Add the following lines to your .emacs, changing <PATH> to the path where
;; this eclipse.el can be found:
;;
;; (autoload 'eclipse-mode "<PATH>/eclipse.el" "ECLiPSe editing mode" t)
;; (autoload 'eclipse-esp-mode "<PATH>/eclipse.el" "ECLiPSe-ESP editing mode" t)
;; (setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.esp" . eclipse-esp-mode) auto-mode-alist))


;; This version has been tested on emacs 22.1.1 for Linux
;; Your mileage may vary.
;; Support for XEmacs has been removed. I don't use XEmacs, and I don't have the time
;; to keep the code current for more than one version of Emacs.


;;; NOTE: If there is a problem with entering commands in the inferior 
;;; ECLiPSe process window, disable the line
;;;               (define-key map "\r" 'eclipse-next-line)
;;; in the definition of function eclipse-mode-commands

;; TODO:
;; - simplify code


;;; Code:

;; what Emacs is it?

(defvar eclipse-emacs-21 (and (equal (substring (version) 0 9) "GNU Emacs")
			      (>= emacs-major-version 21)))
(defvar eclipse-emacs-22 (and (equal (substring (version) 0 9) "GNU Emacs")
			      (>= emacs-major-version 22)))

;;
;; Definitions
;;

(defvar eclipse-mode-syntax-table nil)
(defvar eclipse-esp-mode-syntax-table nil)
(defvar eclipse-mode-abbrev-table nil)
(defvar eclipse-mode-map nil)

(defgroup eclipse nil
  "Major mode for editing and running ECLiPSe under Emacs."
  :group 'languages)

(defconst eclipse-mode-version 7)

(defvar eclipse-version 0.0
  "Variable is set when ECLiPSe process is started")

;; path definitions and program calls

(defcustom eclipse-path ""
  "Path where ECLiPSe can be found.

Change only, if ECLiPSe path is not in environment variable PATH"
  :type 'string
  :group 'eclipse)

(defconst eclipse-program-name "eclipse"
  "Program name for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-tktools-name "tktools"
  "Program name for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-program-call
  (concat eclipse-path eclipse-program-name)
  "Program call for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-tktools-call
  (concat eclipse-path eclipse-tktools-name)
  "Program call for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-name "remote_tools"
  "ECLiPSe library for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-pred "lib"
  "How remote_tools.pl shall be loaded: as library (lib) or module
(use_module).")

(defconst eclipse-tktools-lib-call
  (concat eclipse-tktools-lib-pred "(" eclipse-tktools-lib-name "), ")
  "ECLiPSe tktools library call.")

(defconst eclipse-54-tktools-call 
  (concat eclipse-tktools-lib-call "attach_tools(Host/Port,block,writeln([Host,Port])).\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.4 and later.

The first parameter of attach_tools/3 returns the Host and Port, 
the second parameter is the timeout in seconds (or 'block' for no timeout),
the third parameter is a predicate called after establishing the connection.")

(defconst eclipse-53-tktools-call
  (concat eclipse-tktools-lib-call "attach_tools.\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.3 and earlier.")

(defconst eclipse-run-tktools-func 'eclipse-run-tktools
  "Elisp function to extract Host and Port values from output and start
tktools.

Is added to 'comint-preoutput-filter-functions, and must remove itself from
this list when the output line containing host and port is processed.")

(defconst eclipse-reconsult-string "reconsult(user).\n"
  "*(Re)Consult mode (for C-Prolog and Quintus Prolog).")

(defconst eclipse-consult-string "consult(user).\n"
  "*Consult mode.")

(defconst eclipse-compile-string "compile(user).\n"
  "*Compile mode.")

(defconst eclipse-eof-string "end_of_file.\n"
  "*String that represents end of file for eclipse.
nil means send actual operating system end of file.")

(defconst eclipse-halt-string "halt.\n"
  "*Command that stops the eclipse process.")

(defconst eclipse-help-call1 
  (concat eclipse-program-call " -e \"help(")
  "First part of help call to ECLiPSe system.")

(defconst eclipse-help-call2 ").\""
  "Second part of help call to ECLiPSe system.")

(defconst eclipse-lint-call "lib(lint).\n"
  "Command that loads the lint library.")

(defconst eclipse-lint-cmd1 "lint(\""
  "First part of lint command.")

(defconst eclipse-lint-cmd2 "\").\n"
  "Second part of lint command.")

(defconst eclipse-process-name "eclipse"
  "Name of ECLiPSe process")

(defconst eclipse-process-buffer (concat "*" eclipse-process-name "*")
  "Name of ECLiPSe process buffer")

(defconst eclipse-compile-process-name "eclipse-compile"
  "Name of ECLiPSe process")

(defconst eclipse-compile-process-buffer (concat "*" eclipse-compile-process-name "*")
  "Name of ECLiPSe process buffer")

(defvar eclipse-compile-buffer nil) ; needed for compilation
(defvar eclipse-compile-offset 0) ; needed for compilation

(defconst eclipse-tktools-name "tktools"
  "Name of TkTools process")

(defconst eclipse-tktools-buffer (concat "*" eclipse-tktools-name "*")
  "Name of TkTools process buffer")

(defconst eclipse-compilation-buffer "*eclipse-compilation*"
  "Name of ECLiPSe compilation results buffer")

(defconst eclipse-keywords-buffer "*eclipse-keywords*"
  "Name of ECLiPSe keywords buffer")

(defconst eclipse-help-buffer "*eclipse-help*"
  "Name of ECLiPSe help buffer")

(defconst eclipse-stats-buffer "*eclipse-stats*"
  "Name of ECLiPSe statistics buffer")

(defcustom eclipse-source-tracing nil
  "If set to t, tracing a program in an ECLiPSe process buffer 
will trace the source in a second frame. This only works with 
ECLiPSe 6.0 or higher."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-extensions '(".ecl" ".pl" ".esp")
  "List of recognized ECLiPSe file extemsions."
  :type 'list
  :group 'eclipse)

(defvar eclipse-process-parent-window nil)
  ;; active frame when inferior ECLiPSe was started

(defvar eclipse-source-tracing-counter 0)
  ;; needed to process source info output

(defvar eclipse-predicate-template nil)
  ;; needed for eclipse-goto-predicate

;; indentation definitions

(defcustom eclipse-indent-width 4
  "Standard additional indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-esp-indent-width 2
  "Standard indentation in ECLiPSe-ESP buffers."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-tab-width 8
  "Minimum indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defvar eclipse-old-tab-width eclipse-tab-width)

(defcustom eclipse-indent-mode nil
  "If set to t, indentation will always increase/decrease by
`eclipse-indent-width'."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-closing-parenthesis-to-match-opening t
  "If set to t, indentation will indent closing parentheses to the
same column as the matching opening parentheses."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-to-parenthesis t
  "Indentation of if-then-clauses and for-loops calculated from column of
preceding opening parenthesis."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-after-repeat t
  "If set to t, text between repeat/0 and !/0 will be intented one extra 
level. Note that repeat or ! as term arguments cannot be distinguished 
from repeat/0 and !/0."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-first-line-std-indent t
  "Always indent the first line of a predicate using `eclipse-tab-width'."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-tab-mode nil
  "Indentation in ECLiPSe buffers with spaces or tabs?
Set this variable to nil to insert only space characters.
To change the behaviour during editing, use \\[eclipse-tab-mode-toggle]."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-autolinebreak-selected t
  "Automatic line-break in ECLiPSe buffer."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-autoindent-selected t
  "Automatic indentation in ECLiPSe buffer."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-autoindent-no-string-splitting t
  "No string splitting during automatic indentation in ECLiPSe buffer."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-clause-heads nil
  "If t, clause heads will be indented to column 0.
If nil, indentation of clause heads will not be changed."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-quick-jumps-selected nil
  "If t, the 'go to' commands determine the place to jump to by the next
empty line. If nil, the correct place to jump to is computed correctly, but
this may be slow if the buffer text is long."
  :type 'boolean
  :group 'eclipse)

(defcustom eclipse-indent-timeout 2
  "Timeout for indentation in seconds."
  :type 'integer
  :group 'eclipse)

;; speedbar support definitions

(defvar eclipse-speedbar-selected nil)
  ;; Variable to store the status of the speedbar.
  ;; If t, the speedbar is running, if nil, the speedbar is off.

(defvar eclipse-speedbar-supported nil)
  ;; Variable is t, if speedbar supports .ecl extension.

(defconst eclipse-imenu-generic-expression
  (list (list nil (purecopy "^\\([a-z].*\\)\\([:?]-\\|\n\\)") 1)
 	(list "Directives" "^\\([ \t]*[:?]-[ \t]*\\)\\([a-z].*\\)\\(.\\|\n\\)" 2))
  "Imenu generic expression for ECLiPSe mode. See `imenu-generic-expression'.")

(defconst eclipse-imenu-prev-index-position-function
  ;; my own function to find the previous index function
  'eclipse-goto-prev-index-position)

(defconst eclipse-imenu-create-index-function
  ;; my own function to create the imenu index
  'eclipse-create-index)

(defconst eclipse-imenu-extract-index-name-function
  ;; my own function to extract the name for the index function
  'eclipse-extract-index-name)

;; these three will always be overwritten with local variables
(defvar eclipse-local-tab-width eclipse-tab-width)
(defvar eclipse-local-old-tab-width eclipse-old-tab-width)
(defvar eclipse-esp-selected nil)

;; highlighting definitions

(defvar eclipse-overlays nil)
  ;; list of highlighting overlays

(defvar eclipse-highlighted nil)
  ;; currently highlighted word

(defcustom eclipse-highlight-face-bg-val "cornflower blue"
    "Type face background for highlighting."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-highlight-face-fg-val "white"
    "Type face foreground for highlighting."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-breakpoint-face-bg-val "red"
    "Type face background for breakpoints."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-breakpoint-face-fg-val "white"
    "Type face foreground for breakpoints."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-tracepoint-face-bg-val "darkseagreen2"
    "Type face background for breakpoints."
    :type 'color
    :group 'eclipse)

(defcustom eclipse-tracepoint-fail-face-bg-val "firebrick"
    "Type face background for breakpoints."
    :type 'color
    :group 'eclipse)

;; make face for highlighting
(make-face 'eclipse-highlight-face)
(set-face-background 'eclipse-highlight-face eclipse-highlight-face-bg-val)
(set-face-foreground 'eclipse-highlight-face eclipse-highlight-face-fg-val)

;; make face for breakpoints
(make-face 'eclipse-breakpoint-face)
(set-face-background 'eclipse-breakpoint-face eclipse-breakpoint-face-bg-val)
(set-face-foreground 'eclipse-breakpoint-face eclipse-breakpoint-face-fg-val)

;; make face for tracepoints
(make-face 'eclipse-tracepoint-face)
(make-face 'eclipse-tracepoint-fail-face)
(set-face-background 'eclipse-tracepoint-face eclipse-tracepoint-face-bg-val)
(set-face-background 'eclipse-tracepoint-fail-face eclipse-tracepoint-fail-face-bg-val)

(defvar eclipse-bp-flag nil)
  ;; Variable is t when breakpoint is to be set.

;; make face for input
(make-face 'eclipse-input-face)
(set-face-attribute 'eclipse-input-face nil :bold t)

;; font lock definitions

(defcustom eclipse-font-lock-default 3
  "The default level for the fontification."
  :type 'integer
  :group 'eclipse)

;; create syntax table
(defun eclipse-create-syntax-table (esp-flag)
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?\' "." table)
    (cond (esp-flag
	   (modify-syntax-entry ?\? ". 14b" table)
	   (modify-syntax-entry ?< ". 3b" table)
	   (modify-syntax-entry ?> ". 2b" table)
	   (modify-syntax-entry ?/ "." table)
	   (modify-syntax-entry ?* "." table))
	  (t
	   (modify-syntax-entry ?/ ". 14b" table)
	   (modify-syntax-entry ?* ". 23b" table)
	   (modify-syntax-entry ?< "." table)
	   (modify-syntax-entry ?> "." table)))
    table))

(unless eclipse-mode-syntax-table
  (setq eclipse-mode-syntax-table (eclipse-create-syntax-table nil)))

(unless eclipse-esp-mode-syntax-table
  (setq eclipse-esp-mode-syntax-table (eclipse-create-syntax-table t)))

(define-abbrev-table 'eclipse-mode-abbrev-table ())

;;
;; Font lock regexps
;;

(defconst eclipse-font-lock-colondash
  (list 
   ;; :- not necessarily at end of line
   '("[:?]-" 0 font-lock-builtin-face)))

(defconst eclipse-font-lock-basic
  (list
   ;; quoted atoms
   '("\\([^0-9]\\)\\('\\(\\\\'\\|[^'\n]\\)*'\\)" 2 font-lock-string-face)
   ;; show variables
   '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-variable-name-face)
   ;; predicate definitions (that can be fontified) always start at bol
   '("^[a-z][a-zA-Z0-9_]*" 0 font-lock-function-name-face)
   ;; critical builtins with arity 0 or prefix op
   ;; special for cut, not   (\\?-\\s-*$)???
   '("\\(\\<\\(abort\\|exit\\|fa\\(il\\|lse\\)\\|halt\\|make\\|not\\|pause\\)\\>\\|!\\|\\\\\\+\\)" 0 font-lock-warning-face)
   '("[ \t(]\\(~\\)[ \t(]" 1 font-lock-warning-face)
   ;; true
   '("\\<true\\>" 0 font-lock-constant-face)))

(defconst eclipse-font-lock-basic-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\([:?]-\\)\\s-*$\\|\\<is\\>\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|>=\\|=<\\|<=\\|<\\|=\\|~=\\|[ \t(]\\([-*+/^]\\|//\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(-?>\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\(\\<do\\|[:?]-\\)\\s-*$\\|\\<is\\>\\|[*]?->\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|#<=>\\|#<=\\|#\\\\\\+\\|[#$&]?\\(>=\\|>\\|=<\\|<\\|=\\)\\|@\\(>=\\|>\\|=<\\|<\\)\\|[#$&]\\\\=\\|#\\(#\\|\\\\/\\|/\\\\\\|::\\)\\|#\\|[$&`]::\\|::\\|`<>\\|`\\(<\\|=\\)\\|~=<\\|~=\\|[ \t(]\\([-*+/^~]\\|//\\|/\\\\\\|<<\\|>>\\|\\\\/\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(->\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\<\\(once\\|nl\\|do\\)\\>" 0 font-lock-builtin-face)
   '("\\<\\(a\\(bs\\|cos\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\)?\\)\\|b\\(ag\\(_\\(create\\|dissolve\\|enter\\)\\|of\\)\\|etween\\|ind\\|lock\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_c\\)?\\|nonical_path_name\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\)\\|o\\(mp\\(are\\|ile\\(_\\(stream\\|term\\)\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|reate_module\\)\\|d\\(ate\\|e\\(layed_goals\\(_number\\)?\\)\\|mon\\|i\\(m\\|splay\\)\\|omain\\)\\|e\\(val\\|x\\(\\|i\\(sts\\|t_block\\)\\|p\\(and_goal\\)?\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(et\\(_\\(char\\|flag\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\)?\\|round\\)\\|help\\|i\\(n\\(stance\\|teger\\(_atom\\)?\\)\\)\\|join_string\\|k\\(eysort\\|ill\\)\\|l\\(ength\\|n\\|o\\(ad\\|oop_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\)\\|in\\|od\\|ultifor\\)\\|n\\(l\\|o\\(n\\(ground\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_string\\)?\\)\\)\\|o\\(pen\\|nce\\)\\|p\\(a\\(ram\\|thname\\)\\|ipe\\|lus\\|r\\(ed\\|intf?\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\)\\|e\\(a\\(d\\(_\\(directory\\|string\\|token\\)\\|var\\)\\|[dl]\\)\\|corded\\(_list\\)?\\|verse\\)\\|ound\\)\\|s\\(etof\\|h\\(elf_\\(abolish\\|create\\|get\\|set\\)\\)?\\|in\\|ort\\|plit_string\\|qrt\\|t\\(ring\\(_l\\(ength\\|ist\\)\\)?\\)\\|u\\(b\\(call\\|string\\)\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(string\\|variables\\)\\|ype_of\\)\\|var\\|w\\(rite\\(clause\\|ln\\|q\\)?\\)\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-medium-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\([ \t]^[ \t]\\|\\<\\(env\\|garbage_collect\\|listing\\|once\\|nl\\|do\\|peer_multitask_\\(confirm\\|terminate\\)\\|statistics\\|trimcore\\)\\>\\)" 0 font-lock-builtin-face)
   '("\\<\\('C'\\|a\\(bs\\|c\\(cept\\|os\\|yclic_term\\)\\|dd_attribute\\|l\\(arm\\|s\\)\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\|tached_suspensions\\)?\\)\\|b\\(ag\\(_\\(abolish\\|c\\(ount\\|reate\\)\\|dissolve\\|e\\(nter\\|rase\\)\\|retrieve\\)\\|of\\)\\|etween\\|ind\\|lock\\|real\\(_\\(bounds\\|from_bounds\\|m\\(ax\\|in\\)\\)\\)?\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_\\(c\\|priority\\)\\)?\\|n\\(cel_after_event\\|onical_path_name\\)\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\|rbit\\)\\|o\\(mp\\(are\\(_instances\\)?\\|ile\\(_\\(stream\\|term\\)\\|d_stream\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|urrent_\\(a\\(fter_events?\\|rray\\|tom\\)\\|built_in\\|compiled_file\\|domain\\|error\\|functor\\|host\\|interrupt\\|m\\(acro\\|odule\\(_predicate\\)?\\)\\|op\\|pr\\(agma\\|edicate\\)\\|record\\|s\\(t\\(ore\\|r\\(eam\\|uct\\)\\)\\|uspension\\)\\|trigger\\)\\)\\|d\\(ate\\|e\\(layed_goals\\(_number\\)?\\|mon\\|nominator\\|precated\\)\\|i\\(m\\|splay\\|v\\)\\|omain_index\\)\\|e\\(nter_suspension_list\\|rr\\(no_id\\|or_id\\)\\|v\\(al\\|vent\\(_\\(create\\|disable\\|enable\\|retrieve\\)\\|s_\\(defer\\|nodefer\\)\\)\\)\\|x\\(ec\\(_group\\)?\\|i\\(st\\(ing_file\\|s\\)\\|t_block\\)\\|p\\(and_\\(clause\\|goal\\|macros\\)\\)?\\|ternal\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(atten\\(_array\\)?\\|o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\|k\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(cd\\|et\\(_\\(ch\\(ar\\|tab\\)\\|e\\(rror_handler\\|vent_handler\\)\\|f\\(ile_info\\|lag\\)\\|interrupt_handler\\|leash\\|module_info\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\|bit\\|cwd\\|env\\|val\\)?\\|round\\)\\|help\\|i\\(n\\(stance\\|teger\\(s\\|_atom\\)?\\)\\|s_\\(built_in\\|dynamic\\|event\\|handle\\|list\\|predicate\\|record\\|suspension\\)\\)\\|join_string\\|keysort\\|l\\(cm\\|ength\\|isten\\|n\\|o\\(ad\\|c\\(al_time\\(_string\\)?\\|k\\)\\|op_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\|ta\\(_\\(attribute\\|bind\\)\\)?\\)\\|in\\|kdir\\|od\\|sort\\|u\\(ltifor\\|tex\\(_init\\)?\\)\\)\\|n\\(ame\\|ew_socket_server\\|l\\|o\\(n\\(ground\\|member\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_\\(merge\\|s\\(ort\\|tring\\)\\)\\)?\\|erator\\)\\)\\|o\\(ccurs\\|nce\\|pen\\|s_file_name\\)\\|p\\(a\\(ram\\|thname\\)\\|eer\\(_\\(d\\(eregister_multitask\\|o_multitask\\)\\|get_property\\|multitask_\\(confirm\\|terminate\\)\\|queue_\\(c\\(lose\\|reate\\)\\|get_property\\)\\|register_multitask\\)\\)?\\|hrase\\|ipe\\|lus\\|ortray_\\(goal\\|term\\)\\|r\\(ed\\|intf?\\|ofile\\|une_instances\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\(ize\\)?\\)\\|e\\(a\\(d\\(_\\(annotated\\|directory\\|exdr\\|string\\|t\\(erm\\|oken\\)\\)\\|var\\)?\\|l\\)\\|corded\\(_list\\)?\\|ferenced_record\\|m\\(ote_\\(connect\\(_\\(accept\\|setup\\)\\)?\\|disconnect\\|yield\\)\\)?\\|name\\|verse\\)\\|ound\\)\\|s\\(e\\(e[dk]\\|lect\\|t\\(_stream\\(_property\\)?\\|of\\)\\)\\|gn\\|h\\(elf_\\(abolish\\|create\\|dec\\|get\\|inc\\|set\\)\\)?\\|in\\|leep\\|o\\(cket\\|rt\\)\\|plit_string\\|qrt\\|t\\(atistics\\|ore\\(_\\(c\\(o\\(ntains\\|unt\\)\\|reate\\)\\|delete\\|erase\\|get\\|inc\\|set\\)\\|d_keys\\(_and_values\\)?\\)?\\|r\\(eam_truncate\\|ing\\(_\\(code\\|l\\(ength\\|ist\\)\\)\\)?\\)\\)\\|u\\(b\\(call\\|s\\(cript\\|tr\\(act\\|ing\\)\\)\\)\\|cc\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(hash\\|string\\|to_bytes\\|variables\\)\\|imes\\|ool_body\\|r\\(_if_suspend\\|uncate\\)\\|y\\(pe_of\\|[io]\\)\\)\\|un\\(get\\|lock\\)\\|var\\(ia\\(ble\\|nt\\)\\)?\\|w\\(ait\\|rite\\(_\\(canonical\\|exdr\\|term\\)\\|clause\\|ln\\|q\\)?\\)\\|xor\\|yield\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-low
  (list
   ;; critical builtins
   '("\\<\\(a\\(ssert[az]?\\|ttach_suspensions\\)\\|call_priority\\|de\\(cval\\|fine_macro\\|lete\\)\\|e\\(r\\(ase\\(_\\(a\\(ll\\|rray\\)\\|m\\(acro\\|odule\\)\\)\\)?\\|ror\\)\\|vent\\(_\\(after\\(_every\\)?\\|create\\|retrieve\\)\\|s_after\\)?\\)\\|in\\(c\\(lude\\|val\\)\\|it_suspension_list\\|sert_suspension\\)\\|kill\\(_suspension\\)?\\|m\\(ake_suspension\\|erge_suspension_lists\\)\\|notify_constrained\\|re\\(cord[az]?\\|record\\|set_e\\(vent\\|rror\\)_handler\\|tract\\(_all\\)?\\)\\|s\\(chedule_suspensions\\|et\\(_\\(chtab\\|default_error_handler\\|e\\(rror_handler\\|vent_handler\\)\\|env\\|flag\\|interrupt_handler\\|suspension_data\\|var_bounds\\)\\|arg\\|bit\\|val\\)\\|uspend\\)\\|t\\(est_and_setval\\|rigger\\)\\|update_struct\\|x\\(get\\|set\\)\\)(" 1 font-lock-warning-face)
   '("\\<\\(reset_error_handlers\\|trimcore\\)\\>" 0 font-lock-warning-face)))

(defconst eclipse-font-lock-medium
  (list
   ;; base operator
   '("\\([0-9]\\)\\('\\)\\(.+\\)" 2 font-lock-builtin-face)
   ;; directives
   '("^:-\\s-*\\(comment\\|d\\(emon\\|ynamic\\)\\|export\\|import\\|p\\(arallel\\|ragma\\)\\|reexport\\)\\>" 0 font-lock-keyword-face)
   '("\\<\\(comment\\|d\\(emon\\|ynamic\\)\\|export\\|import\\|p\\(arallel\\|ragma\\)\\|reexport\\|local\\|global\\)(" 1 font-lock-keyword-face)
   ;; highlight mode/tool declaration
   '("^\\(:-\\s-*mode\\)\\([^.]*\\)\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
   '("^\\(:-\\s-*\\(tool\\|discontiguous\\)\\)(\\([a-z][a-zA-Z0-9_]*/[0-9]+\\),[ ]*\\([a-z][a-zA-Z0-9_]*/[0-9]+\\))\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face)
     (3 font-lock-constant-face))
   '("^:-\\s-*\\(ensure_loaded\\|inline\\|mode\\|tool\\)\\>" 0 font-lock-keyword-face)
   ;; module names & structures
   '("^\\(:-\\s-*\\(module\\|module_interface\\|begin_module\\|create_module\\|use_module\\|lib\\)\\)(\\([a-zA-Z_0-9]+\\))"
     (1 font-lock-keyword-face) (3 font-lock-constant-face))
   '("\\<\\(module\\|module_interface\\|begin_module\\|create_module\\|use_module\\|lib\\)(\\([a-zA-Z_0-9]+\\))"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
;   '("^\\(:-\\s-*\\(ensure_loaded\\)(\\([a-zA-Z_0-9. ']+\\))"
;     (1 font-lock-keyword-face)) ; (2 font-lock-constant-face))
   '("^\\(:-\\s-*\\(local\\|global\\|export\\)\\)[ \t\n]*\\(s\\(helf\\|t\\(ore\\|ruct\\)\\|yntax_option\\)\\|macro\\|op\\|portray\\|re\\(cord\\|ference\\)\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face t) (3 font-lock-keyword-face t))
   '("\\<\\(s\\(helf\\|truct\\|yntax_option\\)\\|macro\\|op\\|portray\\|reference\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face t))
   '("\\<\\(store\\|record\\)([a-z][a-zA-Z0-9_]*)"
     (1 font-lock-keyword-face t))
   '("^\\(:-\\s-*local\\|global\\)[ \t]"
     (1 font-lock-keyword-face))
   ;; import from module
   '("\\(from\\)\\s-+\\([a-z0-9_]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
   ;; special case for structures   
   '("\\<[a-z][a-zA-Z0-9_]*[ \t]+\\(of [a-z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-constant-face)
   '("\\<[a-z][a-zA-Z0-9_]*[ \t]+with\\>" 0 font-lock-constant-face)
   '("\\<\\([a-z][a-zA-Z0-9_]*\\){" 1 font-lock-constant-face)
   ;; structure elements and calls from other module
   '("\\<[a-z][a-zA-Z0-9_]*:" 0 font-lock-constant-face)
   ;; calls "as if inside other module"
   '("@[ \t]*[A-Za-z0-9_]+" 0 font-lock-constant-face)
   ;; critical builtins with arity 0 or prefix op
   '("\\(\\<\\(abolish\\|re\\(peat\\|set_error_handlers\\)\\|wake\\)\\>\\)[^(/]" 1 font-lock-warning-face)
   '("\\(-->\\|-\\?->\\|\\?-\\)" 0 font-lock-warning-face)
   ;; critical builtins
   '("\\<\\(a\\(bolish_\\(op\\|record\\)\\|rr_create\\)\\|current_array\\|erase_array\\|fail_if\\|local_array\\|meta_attribute\\)(" 1 font-lock-warning-face)
   ;; debugging
   '("\\<\\(debug\\(_\\(compile\\|reset\\)\\|ging\\)?\\|no\\(debug\\|spy\\|trace\\)\\|s\\(kipped\\|py\\(_\\(term\\|var\\)\\)?\\)\\|trace\\(_exit_port\\)?\\)\\>" 0 font-lock-constant-face)
   '("\\<\\(debug\\|[gs]et_leash\\|leash\\|\\(kill\\|make\\)_display_matrix\\|trace\\(_\\(call\\|parent\\|point\\)_port\\|able\\)?\\|un\\(skipped\\|traceable\\)\\)(" 1 font-lock-constant-face)
   ;; some other stuff: constants, etc.
   '("\\(\\<\\(a\\(fter_event_timer\\|ll\\(sols\\|_dynamic\\)\\|nti_first_fail\\|rrays\\)\\|b\\(ased_bignums\\|bs\\|lanks_in_nil\\|rea\\(k_level\\|l_exceptions\\)\\)\\|c\\(o\\(mplete\\|ntrol\\|routine\\)\\|redit\\|wd\\)\\|d\\(bs\\|e\\(bug\\(_compile\\|ging\\)\\|fault_language\\|nse_output\\)\\|oubled_quote_is_quote\\)\\|e\\(clipse_\\(info\\|object\\)_suffix\\|n\\(able_interrupts\\|d_of_file\\)\\|xtension\\)\\|f\\(irst_fail\\|loat_precision\\)\\|g\\(c\\(_\\(interval\\(_dict\\)?\\|policy\\)\\)?\\|oal_expansion\\)\\|host\\(arch\\|id\\|name\\)\\|i\\(gnore_eof\\|n\\(domain\\(_\\(interval\\|m\\(ax\\|edian\\|i\\(ddle\\|n\\)\\)\\|random\\|split\\)\\)?\\|put_order\\|stallation_directory\\)\\|so_\\(base_prefix\\|escapes\\)\\)\\|l\\(a\\(rgest\\|st_errno\\)\\|ds\\|i\\(brary_path\\|mit_arg_precedence\\)\\|oaded_library\\)\\|m\\(a\\(cro_expansion\\|x_\\(global_trail\\|local_control\\|predicate_arity\\|regret\\)\\)\\|ost_constrained\\)\\|n\\(ested_commentsl_in_quotes\\|o_\\(array_subscripts\\|blanks\\)\\)\\|o\\(bject_suffix\\|ccur\\(_check\\|rence\\)\\|utput_mode\\)\\|p\\(p?id\\|r\\(efer_rationals\\|int_depth\\|olog_suffix\\)\\)\\|remote_protocol_version\\|s\\(mallest\\|yntax_option\\)\\|t\\(mp_dir\\|oplevel_module\\)\\|unix_time\\|v\\(ariable_names\\|ersion\\(_as_list\\)?\\)\\|w\\(m_window\\|orker\\(ids\\|s\\)?\\)\\)\\)\\([ \t\n,)]\\)" 1 font-lock-constant-face)
   ;; 'with attributes'/2
   '("'with attributes'" 0 font-lock-builtin-face t)))

(defconst eclipse-comment-font-lock
  (list
   ;; fontification of comment predicate
   '("\\(^\\|comment(.*\\)\\s-*[[]?\\s-*\\(a\\(lias\\|mode\\|rgs\\|uthor\\)\\|copyright\\|d\\(ate\\|esc\\)\\|e\\(g\\|xceptions\\)\\|f\\(ail_if\\|ields\\)\\|in\\(clude\\|dex\\)\\|resat\\|s\\(ee_also\\|tatus\\|ummary\\)\\|template\\)\\s-*[,:]" 2 font-lock-type-face)
   ;; special case for comment/2 predicate
   '("^:-\\s-*comment(\\([a-z_][a-z0-9_]*\\s-*/\\s-*[0-9]+\\)" 1 font-lock-function-name-face)
   ;; predicate definitions in comment/2
   '("\\(</?[Pp]>\\|<[Bb][Rr]?>\\|</?[Bb]>\\|</?[Ll][Ii]>\\|</?[UuOo][Ll]>\\|<[Aa][^>]*>\\|</[Aa]>\\|</?[Ii]>\\|</?[Dd][LlTtDd]>\\|</?[Tt][Tt]>\\|</?[Ee][Mm]>\\|</?[Pp][Rr][Ee]>\\|</?[Ss][Tt][Rr][Oo][Nn][Gg]>\\)" 0 font-lock-function-name-face t)
   ;; override html markup in strings and comments
   ;; show variables in args field of comment, overrides comments
   '("\\(^\\s-*\\|[[]\\)\"\\([_A-Z][a-zA-Z0-9_]*\\)\"\\s-*:" 2 font-lock-variable-name-face t))
  "Font lock description of additional comment/2 expressions.")

(defconst eclipse-esp-font-lock
  (list
   '("^\\s-*\\(<[?]\\)\\(esp\\)\\(:[^ \t\n]*\\)[ \t\n]"
     (1 font-lock-comment-face t) (2 font-lock-function-name-face t)
     (3 font-lock-constant-face t))
   '("^\\s-*\\(<[?]\\)\\(esp\\)[ \t\n]"
     (1 font-lock-comment-face t) (2 font-lock-function-name-face t)))
  "Font lock description of esp expressions.")

(defconst eclipse-font-lock-keywords-1
  (append
   eclipse-font-lock-basic
   eclipse-font-lock-basic-infix
   eclipse-font-lock-colondash)
  "Basic (Prolog) expressions for font-lock mode.")

(defconst eclipse-font-lock-keywords-2
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-low-builtins
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)
  "Essential ECLiPSe expressions for font lock mode.")

(defconst eclipse-font-lock-keywords-3
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-medium-builtins
   eclipse-font-lock-medium
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)   
  "Highlights ECLiPSe expressions except comment/2.")

(defconst eclipse-font-lock-keywords-4
 (append
   eclipse-font-lock-keywords-3
   eclipse-comment-font-lock)
  "Highlights all ECLiPSe expressions.")

(defconst eclipse-font-lock-keywords-5
  (append
   eclipse-font-lock-keywords-3
   eclipse-esp-font-lock)
  "Highlights ECLiPSe expressions in ESM mode.")

(defconst eclipse-font-lock-keywords
   (cond ((= eclipse-font-lock-default 0) nil)
	 ((= eclipse-font-lock-default 1) eclipse-font-lock-keywords-1)
	 ((= eclipse-font-lock-default 2) eclipse-font-lock-keywords-2)
	 ((= eclipse-font-lock-default 3) eclipse-font-lock-keywords-3)
	 ((= eclipse-font-lock-default 4) eclipse-font-lock-keywords-4)
	 ((= eclipse-font-lock-default 5) eclipse-font-lock-keywords-5))
  "Additional expressions to highlight in ECLiPSe mode.")

(put 'eclipse-mode 'font-lock-defaults '(eclipse-font-lock-keywords nil))

;;
;; Mode map
;;

(defun eclipse-mode-variables (&optional esp)
  (if esp
      (set-syntax-table eclipse-esp-mode-syntax-table)
    (set-syntax-table eclipse-mode-syntax-table))
  (setq local-abbrev-table eclipse-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function) 'eclipse-indent-line)
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (setq case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression) eclipse-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist) '(("+-*/.<>=?!$%_&~^:" . "w")))
  (set (make-local-variable 'imenu-prev-index-position-function) eclipse-imenu-prev-index-position-function)
  (set (make-local-variable 'imenu-extract-index-name-function) eclipse-imenu-extract-index-name-function)
  (set (make-local-variable 'comment-indent-function) 'eclipse-indent-line)
  (set (make-local-variable 'imenu-create-index-function) eclipse-imenu-create-index-function)
  (setq imenu-sort-function 'imenu--sort-by-name))

(defun eclipse-mode-commands (map)
  "Contains the key-bindings for the major ECLiPSe mode.
The following commands are available:

\\{eclipse-mode-map}"
  (define-key map "\r" 'eclipse-next-line)
  (define-key map "\t" 'eclipse-indent-line)
  (define-key map "\M-\C-\\" 'eclipse-indent-region)
; M-TAB seems to be the same as C-M-i !?
;  (define-key map "\M-\t" 'eclipse-indent-region)
  (define-key map "\M-[" 'eclipse-dabbrev-expand)
  (define-key map "\M-/" 'eclipse-dabbrev-expand0)
  (define-key map "\M-]" 'eclipse-dabbrev-completion)
  (define-key map "\C-c[" 'eclipse-dabbrev-expand1)
  (define-key map "\C-c]" 'eclipse-dabbrev-completion1)
  (define-key map "\C-cm\C-l" 'eclipse-load-all-modules)
  (define-key map "\C-cml" 'eclipse-load-modules)
  (define-key map "\C-cb\t" 'eclipse-indent-buffer)
  (define-key map "\C-cp\t" 'eclipse-indent-predicate)
  (define-key map "\C-cq\t" 'eclipse-indent-clause)
; <backtab> seems to be defined differently in almost every window
; manager. so you've got to customize it...
  (define-key map "\C-c " 'eclipse-insert-tab)
  (define-key map [backtab] 'eclipse-insert-tab)
  (define-key map "\C-c\C-e" 'run-eclipse)
  (define-key map "\C-c\C-q" 'stop-eclipse)
  (define-key map "\C-c\C-k" 'kill-eclipse)
  (define-key map "\C-c\C-t" 'eclipse-start-tools)
  (define-key map "\C-c\C-g" 'eclipse-run-region)
  (define-key map "\C-c\C-b" 'eclipse-compile-buffer)
  (define-key map "\C-c\C-v" 'eclipse-compile-region)
  (define-key map "\C-c\C-y" 'eclipse-compile-region-and-go)
  (define-key map "\C-c\C-s" 'eclipse-source-tracing-toggle)
  (define-key map "\C-cx" 'eclipse-toggle-source-breakpoint)
  (define-key map "\C-cvb" 'eclipse-check-buffer)
  (define-key map "\C-cvv" 'eclipse-check-region)
  (define-key map "\C-cvp" 'eclipse-check-predicate)
  (define-key map "\C-cvq" 'eclipse-check-clause)
  (define-key map "\C-cvl" 'eclipse-lint-buffer)
  (define-key map "\C-c\C-h" 'eclipse-call-help)
  (define-key map "\C-cc" 'eclipse-comment-region)
  (define-key map "\C-cr" 'eclipse-uncomment-region)
  (define-key map "\C-ci" 'eclipse-invert-comment-region)
  (define-key map "\C-c\C-f" 'eclipse-autolinebreak-toggle)
  (define-key map "\C-c\C-j" 'eclipse-autoindent-toggle)
  (define-key map "\C-c\C-r" 'eclipse-indent-repeat-toggle)
  (define-key map "\C-ca" 'eclipse-anonymise-variables)
  (define-key map "\C-c\C-a" 'eclipse-anonymous-variables)
  (define-key map "\C-cmb" 'eclipse-mark-buffer)
  (define-key map "\C-cmp" 'eclipse-mark-predicate)
  (define-key map "\C-cmq" 'eclipse-mark-clause)
  (define-key map "\M-\C-a" 'eclipse-goto-predicate-begin)
  (define-key map "\M-\C-e" 'eclipse-goto-predicate-end)
  (define-key map "\M-a" 'eclipse-goto-clause-begin)
  (define-key map "\M-e" 'eclipse-goto-clause-end)
  (define-key map "\C-c\C-z" 'eclipse-quick-jumps-toggle)
  (define-key map "\C-ct" 'eclipse-insert-predicate-template)
  (define-key map "\C-cs" 'eclipse-insert-predicate-spec)
  (define-key map "\C-c/" 'eclipse-insert-comment-pred-short)
  (define-key map "\C-c\\" 'eclipse-insert-comment-pred-full)
  (define-key map "\M-\C-m" 'eclipse-insert-clause-head)
  (define-key map "\M-\C-i" 'eclipse-insert-clause-head-empty)
  (define-key map "\C-ch" 'eclipse-highlight)
  (define-key map "\C-cd" 'eclipse-dehighlight)
  (define-key map "\C-c>" 'eclipse-goto-highlight-forward)
  (define-key map "\C-c<" 'eclipse-goto-highlight-backward)
  (define-key map "\C-cl" 'eclipse-display-metrics)
  (define-key map "\C-c\C-l" 'eclipse-display-metrics-all))

(defun eclipse-outline-define-map (map)
  (define-key map "\C-c@@" 'eclipse-outline-mark-subtree)
  (define-key map "\C-c@n" 'eclipse-outline-next-visible-heading)
  (define-key map "\C-c@p" 'eclipse-outline-previous-visible-heading)
  (define-key map "\C-c@u" 'eclipse-outline-up-heading)
  (define-key map "\C-c@f" 'eclipse-outline-forward-same-level)
  (define-key map "\C-c@b" 'eclipse-outline-backward-same-level)
  (define-key map "\C-c@h" 'eclipse-hide-predicates)
  (define-key map "\C-c@t" 'eclipse-hide-predicate)
  (define-key map "\C-c@c" 'eclipse-hide-clauses)
  (define-key map "\C-c@e" 'eclipse-hide-clause)
  (define-key map "\C-c@l" 'eclipse-hide-block)
  (define-key map "\C-c-" 'eclipse-hide-block)
  (define-key map "\C-c@a" 'eclipse-show-all)
  (define-key map "\C-c@s" 'eclipse-show-predicates)
  (define-key map "\C-c@r" 'eclipse-show-predicate)
  (define-key map "\C-c@d" 'eclipse-show-clauses)
  (define-key map "\C-c@m" 'eclipse-show-clause)
  (define-key map "\C-c@k" 'eclipse-show-block)
  (define-key map "\C-c+" 'eclipse-show-block))

(unless eclipse-mode-map
  (setq eclipse-mode-map (make-sparse-keymap))
  (eclipse-mode-commands eclipse-mode-map))

;;
;; Menu definitions
;;

(easy-menu-define
 eclipse-process-menu eclipse-mode-map
 "ECLiPSe-Process Menu in ECLiPSe mode.
Contains commands that are associated with an inferior ECLiPSe process."
 '("ECLiPSe-Process"
   ["Run ECLiPSe" run-eclipse t]
   ["Stop ECLiPSe" stop-eclipse t]
   ["Kill ECLiPSe" kill-eclipse t]
   "--"
   ["Compile buffer" eclipse-compile-buffer (not eclipse-esp-selected)]
   ["Compile region" eclipse-compile-region (not eclipse-esp-selected)]
   ["Compile region and switch" eclipse-compile-region-and-go (not eclipse-esp-selected)]
   ["Run region (as command)" eclipse-run-region (not eclipse-esp-selected)]
   "--"
   ["Start TkTools" eclipse-start-tools t]
   "--"
   ["Source tracing on/off" eclipse-source-tracing-toggle
    :active (>= eclipse-version 5.11)
    :style toggle
    :selected eclipse-source-tracing]
   "--"
   ["Call ECLiPSe help" eclipse-call-help t]))

(easy-menu-define
 eclipse-edit-menu eclipse-mode-map
 "ECLiPSe-Edit Menu in ECLiPSe mode.
Contains commands that are associated with editing an ECLiPSe file."
 '("ECLiPSe-Edit"
   ("Indent"
    ["Indent line" eclipse-indent-line t]
    ["Indent region" eclipse-indent-region t]
    ["Indent buffer" eclipse-indent-buffer t]
    ["Indent predicate" eclipse-indent-predicate (not eclipse-esp-selected)]
    ["Indent clause" eclipse-indent-clause t])
   ("Mark"
    ["Mark buffer" eclipse-mark-buffer t]
    ["Mark predicate" eclipse-mark-predicate (not eclipse-esp-selected)]
    ["Mark clause" eclipse-mark-clause t])
   ("Comment"
    ["Comment out region" eclipse-comment-region t]
    ["Uncomment region" eclipse-uncomment-region t]
    ["Invert commenting of region" eclipse-invert-comment-region t])
   ("Text"
    ["Go to beginning of predicate" eclipse-goto-predicate-begin (not eclipse-esp-selected)]
    ["Go to end of predicate" eclipse-goto-predicate-end (not eclipse-esp-selected)]
    ["Go to beginning of clause" eclipse-goto-clause-begin t]
    ["Go to end of clause" eclipse-goto-clause-end t]
    "--"
    ["Highlight current word" eclipse-highlight t]
    ["Remove highlighting" eclipse-dehighlight t]
    ["Go to next" eclipse-goto-highlight-forward t]
    ["Go to previous" eclipse-goto-highlight-backward t]
    "--"
    ["Show metrics" eclipse-display-metrics t]
    ["Show metrics (all buffers)" eclipse-display-metrics-all t]
    "--"
    ["Re-center" recenter t]
    ["Switch to HTML editing mode" eclipse-toggle-html-mode eclipse-esp-selected])
   ("Edit"
    ["Anonymise variables in region" eclipse-anonymise-variables t]
    ["Replace with anonymous variables" eclipse-anonymous-variables t]
    "--"
    ["Insert predicate template" eclipse-insert-predicate-template (not eclipse-esp-selected)]
    ["Insert predicate specification" eclipse-insert-predicate-spec (not eclipse-esp-selected)]
    ["Insert clause head" eclipse-insert-clause-head (not eclipse-esp-selected)]
    "--"
    ["Insert comment/2 template" eclipse-insert-comment-pred-short (not eclipse-esp-selected)]
    ["Insert comment/2 template with arguments" eclipse-insert-comment-pred-full (not eclipse-esp-selected)]
    "--"
    ["Load project files" eclipse-load-modules (not eclipse-esp-selected)]
    ["Load all project files" eclipse-load-all-modules (not eclipse-esp-selected)]
    "--"
    ["Auto-expand to name" eclipse-dabbrev-expand0 t]
    ["Auto-expand to template" eclipse-dabbrev-expand t]
    ["List expansion templates" eclipse-dabbrev-completion t]
    ["Auto-expand to name + (" eclipse-dabbrev-expand1 t]
    ["List expansion names" eclipse-dabbrev-completion1 t])
   ("Check"
    ["Check buffer" eclipse-check-buffer (and (not eclipse-esp-selected) eclipse-emacs-22)]
    ["Check region" eclipse-check-region (and (not eclipse-esp-selected) eclipse-emacs-22)]
    ["Check predicate" eclipse-check-predicate (and (not eclipse-esp-selected) eclipse-emacs-22)]
    ["Check clause" eclipse-check-clause (and (not eclipse-esp-selected) eclipse-emacs-22)]
    "--"
    ["Check buffer with lint library" eclipse-lint-buffer (and (not eclipse-esp-selected) eclipse-emacs-22)])
   ("Outline"
    ("Hide"
     ["Hide All Predicates" eclipse-hide-predicates (not eclipse-esp-selected)]
     ["Hide Predicate" eclipse-hide-predicate (not eclipse-esp-selected)]
     ["Hide All Clauses" eclipse-hide-clauses (not eclipse-esp-selected)]
     ["Hide Clause" eclipse-hide-clause (not eclipse-esp-selected)]
     ["Hide Block" eclipse-hide-block (not eclipse-esp-selected)])
    ("Show"
     ["Show All Predicates" eclipse-show-predicates (not eclipse-esp-selected)]
     ["Show Predicate" eclipse-show-predicate (not eclipse-esp-selected)]
     ["Show All Clauses" eclipse-show-clauses (not eclipse-esp-selected)]
     ["Show Clause" eclipse-show-clause (not eclipse-esp-selected)]
     ["Show Block" eclipse-show-block (not eclipse-esp-selected)]
     ["Show All" eclipse-show-all (not eclipse-esp-selected)])
    ("Headings"
     ["Previous Same Level" eclipse-outline-backward-same-level (not eclipse-esp-selected)]
     ["Next Same Level" eclipse-outline-forward-same-level (not eclipse-esp-selected)]
     ["Previous" eclipse-outline-previous-visible-heading (not eclipse-esp-selected)]
     ["Next" eclipse-outline-next-visible-heading (not eclipse-esp-selected)]
     ["Up" eclipse-outline-up-heading (not eclipse-esp-selected)])
    "--"
    ["Speedbar on/off" eclipse-speedbar-toggle
     :style toggle
     :selected eclipse-speedbar-selected])
   "--"
   ("Preferences"
     ["Auto-line-break on/off" eclipse-autolinebreak-toggle
      :style toggle
      :selected eclipse-autolinebreak-selected]
     ["Auto-indent on/off" eclipse-autoindent-toggle
      :style toggle
      :selected eclipse-autoindent-selected]
     ["No auto-indent string splitting on/off" eclipse-autoindent-string-splitting-toggle
      :style toggle
      :selected eclipse-autoindent-no-string-splitting]
     ["'Classic' indentation on/off" eclipse-indent-toggle
      :style toggle
      :selected eclipse-indent-mode]
     ["Extra indentation after repeat/0 on/off" eclipse-indent-repeat-toggle
      :style toggle
      :selected eclipse-indent-after-repeat]
     ["Match parentheses on/off" eclipse-match-parenthesis-toggle
      :style toggle
      :selected eclipse-indent-closing-parenthesis-to-match-opening]
     ["Quick jumps on/off" eclipse-quick-jumps-toggle
      :style toggle
      :selected eclipse-quick-jumps-selected]
     "--"
     ("Font Lock"
      ["Font Lock Off" eclipse-font-lock-0 t]
      ["Font Lock Level 1 (Basic)" eclipse-font-lock-1 t]
      ["Font Lock Level 2 (Low)" eclipse-font-lock-2 t]
      ["Font Lock Level 3 (Medium)" eclipse-font-lock-3 t]
      ["Font Lock Level 4 (High)" eclipse-font-lock-4 t])
     "--"
     ["Customize" eclipse-customize-group t])))

;;;###autoload
(defun eclipse-mode ()
  "Major mode for editing ECLiPSe code.

Commands:
\\{eclipse-mode-map}

Entry to this mode calls the value of `eclipse-mode-hook' if that value is
non-nil.

The auto-line-break mode is set to on on start-up. It can be toggled by
calling \\[eclipse-autolinebreak-toggle], or customised by setting the variable 
`eclipse-autolinebreak-selected'.
A non-nil value means that auto-line-break is on.

The auto-indent mode is set to on on start-up. It can be toggled by calling
\\[eclipse-autoindent-toggle], or customised by setting the variable
`eclipse-autoindent-selected'. A non-nil value means that auto-indent is on.
The auto-indent mode can be set to split strings automatically, i.e., add \" 
at the line end and start the indented line with \". It can be toggled by calling
\\[eclipse-autoindent-string-splitting-toggle], or customised by setting the variable
`eclipse-autoindent-no-string-splitting'. A non-nil value means that auto-indent will 
not split strings.

The tab mode is set to \"use space characters\" on start-up. It can be
toggled by calling \\[eclipse-tab-mode-toggle], or customised by setting the
variable `eclipse-tab-mode'. A non-nil value means that tab characters are
used is possible, otherwise space characters only.

The width of the initial indentation at the beginning of a line is stored in
the variable `eclipse-tab-width'. Further indentations, after open brackets,
use the value stored in `eclipse-indent-width'.

The text between repeat/0 and !/0 will be indented one extra level if the
variable `eclipse-indent-after-repeat' is set to t. This can be toggled by 
calling \\[eclipse-indent-repeat-toggle].

If `eclipse-indent-mode' is set to a non-nil value, indentation will always
increase/decrease by `eclipse-indent-width'. The default is nil.
Toggling the variable will also set `eclipse-tab-width' to
`eclipse-indent-width'.

If `eclipse-first-line-std-indent' is set to a non-nil value, the first line
in a clause will always be indented using `eclipse-tab-width'. The default
value is nil.

You can insert additional tab characters (or the equivalent number of
space characters) with \\[eclipse-insert-tab].

Text can be indented using \\[eclipse-indent-line], \\[eclipse-indent-region], \\[eclipse-indent-predicate], and \\[eclipse-indent-clause].
Note that the indentation of regions can be slow.

Regions can be marked using \\[eclipse-mark-buffer], \\[eclipse-mark-predicate], and \\[eclipse-mark-clause].

The text can be navigated with \\[eclipse-goto-clause-begin], \\[eclipse-goto-clause-end], \\[eclipse-goto-predicate-begin], and \\[eclipse-goto-predicate-end].
If `eclipse-quick-jumps-selected' is non-nil, the functions jump to the next
empty line. Otherwise, the correct position for the jump is computed. Since
this may be slow, the default value for the variable is t.

You can highlight the current word with \\[eclipse-highlight]. The highlighting will persist
during editing. You can navigate between highlighted areas by using \\[eclipse-goto-highlight-forward] to
jump to the next highlighted area, and \\[eclipse-goto-highlight-backward] to jump to the previous.
\\[eclipse-dehighlight] removes the highlighting.

\\[recenter] re-centers the buffer.

Regions can be commented out with \\[eclipse-comment-region].
\\[eclipse-uncomment-region] deletes leading '%' characters, and
\\[eclipse-invert-comment-region] combines the two previous actions.

Variables in a region can be anonymised with \\[eclipse-anonymise-variables]: '_' will be added to each
variable. Variables in a region can be replaced by '_' with \\[eclipse-anonymous-variables].

The function \\[eclipse-insert-predicate-template] adds a template for the preceding predicate in the style
'foo(,,)' and jumps to the position of the first ',' (if present).
\\[eclipse-insert-predicate-spec] adds the specification for the preceding predicate in the style 'foo/3'.

\\[eclipse-insert-clause-head] adds an empty clause head for the current (preceding) predicate in the
style 'foo(,,) :-' and jumps to the first ',' (if present).

\\[eclipse-insert-comment-pred-short] insert ':- comment(,).'.
\\[eclipse-insert-comment-pred-full] inserts a full 'comment/2' entry, including the arguments, for the
following predicate.

\\[eclipse-load-modules] recursively loads all files listed in compile, use_module, inlude, and
ensure_loaded commands, starting with the current buffer. \\[eclipse-load-all-modules] repeats the
same for all ECLiPSe buffers.

The ECLiPSe mode can pass code to ECLiPSe for syntax checking. The compiler
output will be parsed and passed to a compile mode buffer. These errors and
warnings will be made clickable and linked to the position in the source code
that caused the error (or where the error was detected). This does not work
for errors detected during the module system initialisation phase.

To check the syntax of the whole buffer content, use \\[eclipse-check-buffer]. \\[eclipse-check-region] checks
the region, \\[eclipse-check-predicate] the current predicate, and \\[eclipse-check-clause] the current clause.
Look in the compile mode description for more information.

The ECLiPSe help can be called with \\[eclipse-call-help]. As a default, the current word
will be looked up. Information on other keywords can be looked up by entering
the predicate specification, e.g. 'foo/2'.

The fontification can be set to four different levels:
\\[eclipse-font-lock-1], \\[eclipse-font-lock-2], \\[eclipse-font-lock-3],
\\[eclipse-font-lock-4], or shut off using \\[eclipse-font-lock-0].
The default value is stored in `eclipse-font-lock-default'. The default value
is 3.


The ECLiPSe mode has support for the speedbar. The speedbar can be started by
calling \\[eclipse-speedbar-toggle]. For ECLiPSe and Prolog programs, the
speedbar will list all predicates and directives.


The ECLiPSe mode uses dabbrev for automatic expansion:

\\[eclipse-dabbrev-expand] expands the current word to a full predicate template. It will search the
output of the ECLiPSe help command and the content of the current and other
ECLiPSe buffers. Using \\[eclipse-dabbrev-expand] will return the next possible expansion.
\\[eclipse-dabbrev-expand0] only expands the keyword, without parameters. \\[eclipse-dabbrev-completion] returns the list of
possible expansions. \\[eclipse-dabbrev-expand1] works like \\[eclipse-dabbrev-expand0] but will add an opening parenthesis
if the predicate takes arguments. \\[eclipse-dabbrev-completion1] will return the list of such
expansions.


The ECLiPSe mode supports outlining text:

To hide text use \\[eclipse-hide-predicates] (hide all predicates), \\[eclipse-hide-predicate] (hide current
predicate), \\[eclipse-hide-clauses] (hide all clauses), \\[eclipse-hide-clause] (hide current clause), and
\\[eclipse-hide-block] (hide current block).

To show text use \\[eclipse-show-all] (show the whole buffer), \\[eclipse-show-predicate] (show the current
predicate), \\[eclipse-show-clause] (show the current clause), and \\[eclipse-show-block] (show the current
block).

\\[eclipse-outline-mark-subtree] marks the current subtree, and \\[eclipse-outline-headers-as-kill]
copies the headers in the region onto the kill ring.

To navigate between headings use \\[eclipse-outline-next-visible-heading] (next visible heading),
\\[eclipse-outline-previous-visible-heading] (previous visible heading), \\[eclipse-outline-up-heading] (previous heading of upper level),
\\[eclipse-outline-forward-same-level] (next heading of same level), and \\[eclipse-outline-backward-same-level] (previous heading of same
level).


The ECLiPSe mode also offers an inferior mode to run an ECLiPSe process:

\\[run-eclipse] opens inferior process buffer (if not already open) and starts ECLiPSe.
\\[stop-eclipse] interrupts the shell or its current subjob if any.
\\[kill-eclipse] sends a quit signal and closes the process
buffer.

\\[eclipse-start-tools] starts the TkTools program.

You can send text to the inferior ECLiPSe process from other buffers using
the commands \\[eclipse-compile-buffer] and \\[eclipse-compile-region].
\\[eclipse-compile-region-and-go] sends the region to the inferior process and switches to the process
buffer. Use \\[eclipse-run-region] to send text as ECLiPSe commands.

When using ECLiPSe 6.0 or higher, source code tracing can be switched on.
Use \\[eclipse-source-tracing-toggle] to toggle source code tracing.
When source code tracing is switched on, a breakpoint can be set/deleted using
\\[eclipse-toggle-source-breakpoint] in the buffer that contains the .ecl file.
Note that the file must already be loaded in the ECLiPSe process in order to
allow setting the breakpoint.


The variable settings for the ECLiPSe mode can be customised with
\\[eclipse-customize-group].
"
  (interactive)
  (eclipse-mode-common 'eclipse-mode "Eclipse")
  ;; start outline minor mode
  (eclipse-outline eclipse-mode-map)
  (run-hooks 'eclipse-mode-hook))

(defun eclipse-esp-mode ()
  "Major mode for editing ECLiPSe Server Pages code.

Commands:
\\{eclipse-mode-map}

For detailed description of variables and functions cf. \\[eclipse-mode]."
  (interactive)
  (eclipse-mode-common 'eclipse-esp-mode "Eclipse-ESP")
  ;; tab-width must be set to esp ident width
  (setq eclipse-local-tab-width eclipse-esp-indent-width
	eclipse-local-old-tab-width eclipse-tab-width)
  ;; switch to ESP mode
  (setq eclipse-esp-selected t)
  (eclipse-font-lock-5)
  (run-hooks 'eclipse-mode-hook))

(defun eclipse-mode-common (mode name)
  (kill-all-local-variables)
  (use-local-map eclipse-mode-map)
  (setq major-mode mode
	mode-name name)
  (eclipse-mode-variables (string-equal name "Eclipse-ESP"))
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(eclipse-font-lock-keywords nil)
	indent-tabs-mode eclipse-tab-mode)
  ;; add menus
  (easy-menu-add eclipse-edit-menu)
  (easy-menu-add eclipse-process-menu)
  ;; always start with auto-line-break mode
  (when eclipse-autolinebreak-selected
    (auto-fill-mode 1)
    (setq auto-fill-function 'eclipse-auto-fill-function))
  ;; start speedbar, if selected
  (eclipse-check-speedbar-supported)
  (eclipse-start-speedbar-if-selected)
  ;; use local variables for tab width
  (make-local-variable 'eclipse-local-tab-width)
  (make-local-variable 'eclipse-local-old-tab-width)
  (setq eclipse-local-tab-width eclipse-tab-width
	eclipse-local-old-tab-width eclipse-old-tab-width)
  ;; use local variable to indicate esp mode
  (make-local-variable 'eclipse-esp-selected)
  (setq eclipse-esp-selected nil))

;;
;; Customisation
;;

(defun eclipse-customize-group ()
  "Customize the ECLiPSe group variables."
  (interactive)
  (customize-group 'eclipse))

;;
;; Font lock toggle
;;

(defun eclipse-font-lock-0 ()
  "Switch font lock off."
  (interactive)
  (font-lock-mode 0))

(defun eclipse-do-font-lock (level)
  ;; switch to font-lock-level level
  (font-lock-mode 1)
  (setq font-lock-keywords level)
  (font-lock-fontify-buffer))
  
(defun eclipse-font-lock-1 ()
  "Switch font lock to basic level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-1))

(defun eclipse-font-lock-2 ()
  "Switch font lock to low level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-2))

(defun eclipse-font-lock-3 ()
  "Switch font lock to medium level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-3))

(defun eclipse-font-lock-4 ()
  "Switch font lock to high level."
  (interactive)
  (eclipse-do-font-lock eclipse-font-lock-keywords-4))

(defun eclipse-font-lock-5 ()
  "Switch font lock to ESP mode."
  (eclipse-do-font-lock eclipse-font-lock-keywords-5))

(defun eclipse-toggle-html-mode()
  "Switches to HTML mode when editing a .esp file"
  ;; TO DO: this works only when done the first time
  ;; subsequent switches do not succeed properly
  (interactive)
  (unless (not eclipse-esp-selected)
    (html-mode)
    ;; add menu entry for switching back to ECLiPSe ESP mode
    (easy-menu-define
      eclipse-html-menu sgml-mode-map
      "Switches back to ECLiPSe mode"
      '("ECLiPSe"
	["Back to ECLiPSe Mode" eclipse-esp-mode t]))))

;;
;; ECLiPSe speedbar support
;;

(defun eclipse-speedbar-toggle()
  "Toggle speedbar on/off.

If necessary, the extension '.ecl' is added to the list of supported
extensions."
  (interactive)
  (require 'speedbar)
  (setq eclipse-speedbar-selected (not eclipse-speedbar-selected))
  (if (not eclipse-speedbar-selected)
      (speedbar -1)
    (speedbar 1)
    (eclipse-add-speedbar-support)))

(defun eclipse-start-speedbar-if-selected ()
  ;; start speedbar if variable eclipse-speedbar-selected is t at startup
  ;; if speedbar does not support .ecl extensions, add support
  (cond ((eclipse-speedbar-loaded)
	 (setq eclipse-speedbar-selected t))
	(eclipse-speedbar-selected
	 (speedbar 1)))
  (when eclipse-speedbar-selected
    (eclipse-add-speedbar-support)))

(defun eclipse-speedbar-loaded ()
  ;; check if speedbar is already loaded
  (condition-case nil
      (if speedbar-buffer
	  t
	nil)
    (error nil)))

(defun eclipse-add-speedbar-support ()
  ;; add .ecl to list of supported extensions if not in list
  (unless (eclipse-check-speedbar-supported)
    (speedbar-add-supported-extension eclipse-extensions)
    (setq eclipse-speedbar-supported t)
    (speedbar-refresh)))

(defun eclipse-check-speedbar-supported ()
  ;; check if .ecl is supported speedbar extension
  (interactive)
  (if (not (eclipse-speedbar-loaded))
      t ; speedbar not loaded: do nothing
    (member 0 (mapcar '(lambda (el)
			 (string-match (concat "\\" el)
				       (car eclipse-extensions)))
		      speedbar-supported-extension-expressions))))

;;
;; ECLiPSe mode auto-fill
;;

(defun eclipse-autolinebreak-toggle ()
  "Toggle auto-line-break on/off in ECLiPSe mode.

When auto-line-break is on, strings, comments, and expressions are broken up
if they get too long to fit into a line."
  (interactive)
  (setq eclipse-autolinebreak-selected (not eclipse-autolinebreak-selected))
  (if (not auto-fill-function)
      (setq auto-fill-function 'eclipse-auto-fill-function)
    (setq auto-fill-function nil))
  (force-mode-line-update))

(defun eclipse-autoindent-toggle ()
  "Toggle auto-indent on/off in ECLiPSe mode.

When auto-indent is on, lines are automatically indented after pressing <RET>."
  (interactive)
  (setq eclipse-autoindent-selected (not eclipse-autoindent-selected)))

(defun eclipse-autoindent-string-splitting-toggle ()
  "Toggle string splitting inhibition during auto-indent on/off in ECLiPSe mode.

When string splitting is allowed, strings are automatically split by adding \" 
at the end of the current line and \" at the beginning of the indented new line 
after pressing <RET>."
  (interactive)
  (setq eclipse-autoindent-no-string-splitting
	(not eclipse-autoindent-no-string-splitting)))

(defun eclipse-indent-repeat-toggle ()
  "Toggle extra indentation after repeat/0 on/off in ECLiPSe mode."
  (interactive)
  (setq eclipse-indent-after-repeat (not eclipse-indent-after-repeat)))

(defun eclipse-indent-toggle ()
  "Toggle fixed indentation indent on/off in ECLiPSe mode.

When fixed indentation is on, indentation increases/decreases by
`eclipse-indent-width' columns. When toggled on, `eclipse-local-tab-width' is set
to `eclipse-indent-width', when toggled off, the variable is set to its
previous value."
  (interactive)
  (setq eclipse-indent-mode (not eclipse-indent-mode))
  (if eclipse-indent-mode
      (setq eclipse-local-old-tab-width eclipse-local-tab-width
	    eclipse-local-tab-width eclipse-indent-width)
    (setq eclipse-local-tab-width eclipse-local-old-tab-width)))

(defun eclipse-quick-jumps-toggle ()
  "Toggle quick jumps on/off in ECLiPSe mode.

When quick jumps are on, the 'go to' commands jump to the next empty lines.
Otherwise, the correct target for the jump is computed, which can be quite
slow."
  (interactive)
  (setq eclipse-quick-jumps-selected (not eclipse-quick-jumps-selected)))

(defun eclipse-match-parenthesis-toggle ()
   "Toggle parentheses matching mode.

When on, closing parentheses are always indented to the same column as the
matching opening parentheses."
   (interactive)
   (setq eclipse-indent-closing-parenthesis-to-match-opening
	 (not eclipse-indent-closing-parenthesis-to-match-opening)))

(defun eclipse-next-line ()
  "This function is called when <RET> is pressed.
If auto-indent is on, the next line is automatically indented."
  ;; handle process marks myself, since older comint.el do not have
  ;; functions comint-set-process-mark and comint-goto-process-mark
  (interactive)
  (let (string (flag nil) proc beg aux end (name (buffer-name)))
    (cond ((string-equal name eclipse-process-buffer)
	   ;; in eclipse process buffer?
	   (cond
	    ;; normal eclipse process command? (ends in '.')
	    ((save-excursion
	       (goto-char (point-max))
	       (backward-char)
	       (skip-chars-backward " \t\n")
	       (setq beg (point))
	       (backward-char)
	       (looking-at "[^.]\\."))
	     (goto-char (+ 1 beg))
	     ;; this is the end of the input
	     (setq end (point))
	     ;; goto last process marker
	     (goto-char (process-mark (get-buffer-process (current-buffer))))
	     (setq beg (point))
	     ;; goto end of command
	     (eclipse-end-of-clause-line)
	     (setq string (buffer-substring beg (point)))
	     ;; could be several commands, so repeat until end of input reached
	     (while (> end (point))
	       (forward-line)
	       (beginning-of-line)
	       (skip-chars-forward " \t\n")
	       (setq aux (point))
	       (eclipse-end-of-clause-line)
	       (setq string (concat string (buffer-substring aux (point)))))
	     (insert "\n")
	     ;; send command to eclipse, fontify, etc.
	     (eclipse-set-process-mark)
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (process-send-string eclipse-process-name (concat string "\n"))
	     (if eclipse-emacs-21
		 (comint-add-to-input-history string)
	       (ring-insert comint-input-ring string))
	     (setq comint-input-ring-index nil))
	    ;; asks for more?
	    ((save-excursion
	       (forward-line 0)
	       (looking-at ".*maybe more) \?"))
	     (save-excursion
	       (re-search-backward " \\?")
	       (forward-char 3)
	       (setq beg (point)))
	     (cond ((eq beg (point))
		    ;; if empty input, send CR to eclipse
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name "\x0d"))
		   (t
		    ;; else fontify and send command to eclipse
		    (setq string (buffer-substring beg (point)))
		    (when eclipse-emacs-21 (eclipse-change-face beg))
		    (insert "\n")
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name string))))        
	    ;; eclipse debugger command?
	    ((save-excursion
	       (forward-line 0)
	       (looking-at ".*%>"))
	     ;; goto begin of debugger command
	     (save-excursion
	       (re-search-backward "%>")
	       (forward-char 3)
	       (setq beg (point)))
	     (cond ((eq beg (point))
		    ;; if empty input, send CR to eclipse
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name "\x0d"))
		   (t
		    ;; else fontify and send command to eclipse
		    (setq string (buffer-substring beg (point)))
		    (when eclipse-emacs-21 (eclipse-change-face beg))
		    (insert "\n")
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name string))))
	    ;; eclipse interruption command?
	    ((save-excursion
	       (forward-line 0)
	       (looking-at "interruption"))
	     ;; get answer, fontify, and send to eclipse
	     (save-excursion
	       (search-backward "?")
	       (forward-char 2)
	       (setq beg (point)))
	     (setq string (buffer-substring beg (point)))
	     (when eclipse-emacs-21 (eclipse-change-face beg))
	     (insert "\n")
	     (eclipse-set-process-mark)
	     (process-send-string eclipse-process-name string))
	    ;; eclipse tracer command?
	    ((save-excursion
	       (forward-line 0)
	       (looking-at ".*[]:?]")
	       (unless (looking-at ".*\\[")
		 (when (looking-at "\\(set\\|.*[?]\\)")
		   (setq flag t)))
	       (when (looking-at "jump")
		 (setq flag t))
	       (setq proc (get-buffer-process (current-buffer))
		     beg (process-mark proc)))
	     ;; get answer, fontify, and send to eclipse
	     (setq string (buffer-substring beg (point)))
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (insert "\n")
	     (setq string (concat string "\n"))
             (eclipse-set-process-mark)
	     (process-send-string eclipse-process-name string))
	    ;; else: regular line, just insert new line
	    (t
	     (insert "\n")
	     (if eclipse-emacs-21
		 (insert "\t") ;; if emacs 21, do not attempt to indent
	       (eclipse-indent-line)))))
	  (t
	   ;; else in eclipse code buffer
	   (newline)
	   (if eclipse-autoindent-selected (eclipse-indent-line t))))))

;; the next two functions are copied & adapted from comint.el --- general
;; command interpreter in a window stuff
;; Copyright (C) 1988, 90, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;	Free Software Foundation, Inc.
;; Authors: Olin Shivers <shivers@cs.cmu.edu>
;;	    Simon Marshall <simon@gnu.org>)
(defun eclipse-set-process-mark ()
  (let ((proc (get-buffer-process (current-buffer))))
    (set-marker (process-mark proc) (point))))

(defun eclipse-change-face (beg)
  (let ((over (make-overlay beg (1- (point)) nil nil t)))
    (overlay-put over 'field 'input)
    (overlay-put over 'face 'eclipse-input-face)))

;; The autofill function was copied & adapted from simple.el --- basic
;; editing commands for Emacs
;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;        Free Software Foundation, Inc.
;;
;; the breaking-up of strings, quoted atoms, and comments needs special
;;   handling
;; comments partly deleted: look up in simple.el
(defun eclipse-auto-fill-function ()
  "Auto-fill function for ECLiPSe mode.

Example:
pred(Var) :-
        another_pred(
                        \"This is a very long string that will be\"
                        \" automatically wrapped around\", %% This is a
                                                         %% comment
                        A + Really + Very + Long + And + Nonsensical +
                        Expression
                    )."
  (let (fc give-up (ep nil))
    (if (or (null (setq fc (current-fill-column)))
	    (<= (current-column) fc))
	nil ;; Auto-filling not required
      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (bol-point first-break-point atom-break-point
	       (fill-point
		(let ((opoint (point)))
		  (save-excursion
		    (beginning-of-line)
		    (setq bol-point (point))
		    (move-to-column (1+ fc))
		    ;; Move back to the point where we can break the line.
		    (re-search-backward "[-/+*=<> \t,;]" bol-point t)
		    (cond ((looking-at "[-/+*=<>]") ; math operator
			   (re-search-forward "[^-/+*=<>]" (point-max) t)
			   (backward-char))
			  ((looking-at "[,;]") ; and/or
			   (forward-char))
			  (t t))
		    ;; check if we're inside a quoted atom
		    ;; if so, break before the start of the quoted atom
		    (setq first-break-point (point))
		    (beginning-of-line)
		    (let ((cc nil) (ac nil) (sc nil))
		      (while (< (point) first-break-point)
			(forward-char 1)
			(cond ((looking-at "\"")
			       (or ac cc (setq sc (not sc))))
			      ((looking-at "%")
			       (or sc ac (setq cc t)))
			      ((looking-at "'")
			       (or sc cc
				   (progn
				     (setq ac (not ac))
				     (or (not ac)
					 (save-excursion
					   (re-search-backward "[-/+*=<> \t,;]")
					   (when (looking-at "[-/+*=<>]")
					     (re-search-forward "[^-/*+=<>]" (point-max) t)
					     (backward-char))
					   (setq atom-break-point (point)))))))
			      (t t)))
		      (if ac (goto-char atom-break-point)))
		    ;; If we find nowhere on the line to break it,
		    ;; break after one word.
		    (cond ((bolp)
			   (re-search-forward "[ \t]" opoint t))
			  ((looking-at "[ \t]")
			   ;; Break the line at word boundary.
			   (skip-chars-backward " \t"))
			  ((looking-at "[-/*+=<>]")
			   (re-search-forward "[^-/+*=<>]" (point-max) t)
			   (backward-char))
			  ((eq (point) first-break-point) t) ;; same point: break here
			  (t (forward-char))) ;; Break the line after/before \c|.
		    (if (and enable-multibyte-characters
			     (not (and (eq (charset-after (1- (point))) 'ascii)
				       (eq (charset-after (point)) 'ascii))))
			;; special function for the charset of non-ascii
			;; character to find the correct break point.
			(fill-find-break-point bol-point))
		    ;; move back before any whitespace here.
		    (skip-chars-backward " \t")
		    ;; that's the fill-point
		    (point)))))
	  ;; See whether the place we found is any good.
	  (if (and (not give-up)
		   (save-excursion
		     (goto-char fill-point)
		     (and (not (bolp))
			  (not (save-excursion (skip-chars-forward " \t")
					       (eolp))))))
	      ;; There is no use breaking at end of line...
	      ;; ...or beginning of line.
	      ;; (test for comment start deleted)
	      ;; Ok, we have a useful place to break the line.  Do it.
	      (let (counter (colmn nil) (prev-column (current-column)))
		;; now we must determine, if the break-point is
		;; (a) in a comment, or
		;; (b) in a string, or
		;; (c) in a regular line
		;; if (a), break the line, insert spaces until the beginning
		;;         of the comment, and insert as many percentage signs
		;; if (b), add \", break the line, indent, add \"
		;; if (c), break the line and indent
		;; quoted atoms have been dealt with while finding the
		;; break point. dealing with comments should be done at that
		;; point, too...
		(cond ((save-excursion
			 ;; inside a comment?
			 (beginning-of-line)
			 (setq colmn nil)
			 (while (and (< (point) fill-point) (not colmn))
			   (cond ((looking-at "\"")
				  (eclipse-goto-end-of-string))
				 ((looking-at "'")
				  (eclipse-goto-end-of-quote))
				 ((looking-at "%")
				  (setq colmn (point)))
				 (t (forward-char))))
			 colmn)
		       ;; continue comment in next line
		       (if (not eclipse-autoindent-selected)
			   (save-excursion
			     (goto-char fill-point)
			     (insert "\n% "))
			 (save-excursion
			   (goto-char colmn)
			   (setq colmn (current-column)
				 counter 0)
			   (while (looking-at "%")
			     (forward-char)
			     (setq counter (+ counter 1)))
			   (goto-char fill-point)
			   (insert "\n")
			   (indent-to colmn)
			   (insert (make-string counter 37)))))
		      ((save-excursion
			 ;; inside a string?
			 (goto-char fill-point)
			 (setq counter 0)
			 (while (not (bolp))
			   (when (looking-at "\"")
			     (setq counter (+ counter 1)))
			   (backward-char))
			 (and (> counter 0)
			      (= (mod counter 2) 1)))
		       ;; close string before fill point,
		       ;; open string anew after indenting
		       (cond ((not eclipse-autoindent-selected)
			      (save-excursion
				(goto-char fill-point)
				(insert "\n")))
			     (eclipse-autoindent-no-string-splitting
			      (save-excursion
				(goto-char fill-point)
				(insert "\n"))
			      (eclipse-indent-line nil t))
			     (t
			      (save-excursion
				(goto-char fill-point)
				(if (save-excursion
				      (backward-char)
				      (looking-at "\""))
				    (progn (backward-char 2) (insert "\n"))
				  (insert "\"\n\"")))
			      (eclipse-indent-line nil t)
			      (skip-chars-forward " \t"))))
		      (t
		       (save-excursion
			 (goto-char fill-point)
			 (insert "\n")
			 (if eclipse-autoindent-selected
			     (eclipse-indent-line nil t))
			 (setq ep (point)))))
		;; If making the new line didn't reduce the hpos of
		;; the end of the line, then give up now.
		(if (>= (current-column) prev-column) (setq give-up t)))
	    ;; No good place to break => stop trying.
	    (setq give-up t))))
      (if (and ep (< (point) ep)) (goto-char ep))))) 

;;
;; ECLiPSe mode commenting in & out
;;

(defun eclipse-change-comment-region (mode)
  ;; change commenting of current region
  (let ((pos (point)) (regionend (region-end)) (max 0) type_str msg)
    (cond ((= mode 1) (setq type_str "Commenting out"))
	  ((= mode 2) (setq type_str "Un-commenting"))
	  ((= mode 3) (setq type_str "Inverting commenting of")))
    (setq msg (concat type_str " region..."))
    (message msg)
    (goto-char (region-beginning))
    ;; only change complete lines
    (beginning-of-line)
    (while (< (point) regionend)
      (cond ((and (or (= mode 2) (= mode 3)) (looking-at "%% "))
	     (delete-char 3)
	     (setq max (+ max 3)
		   regionend (- regionend 3)))
	    ((and (or (= mode 2) (= mode 3)) (looking-at "%%\t"))
	     (delete-char 2)
	     (setq max (+ max 2)
		   regionend (- regionend 2)))
	    ((and (or (= mode 2) (= mode 3)) (looking-at "%[ \t]"))
	     (delete-char 1)
	     (setq max (+ max 1)
		   regionend (- regionend 1)))
	    ((or (= mode 1) (= mode 3))
	     (insert "%% ")
	     (setq max (- max 3)
		   regionend (+ regionend 3)))
	    (t t))
      (forward-line)
      (unless (eobp)
	(beginning-of-line)))
    (message (concat msg "done"))
    (goto-char (- pos max))))

(defun eclipse-comment-region ()
  "Comment out current region."
  (interactive)
  (eclipse-change-comment-region 1))

(defun eclipse-uncomment-region ()
  "Uncomment current region."
  (interactive)
  (eclipse-change-comment-region 2))

(defun eclipse-invert-comment-region ()
  "Invert commenting of current region."
  (interactive)
  (eclipse-change-comment-region 3))

;;
;; ECLiPSe mode indentation
;;

(defun eclipse-tab-mode-toggle ()
  "Toggle tab-mode on/off in ECLiPSe mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)
	eclipse-tab-mode (not eclipse-tab-mode)))

(defun eclipse-insert-tab ()
  "Insert a tab character, or, if eclipse-tab-mode is off,
`eclipse-indent-width' many space characters."
  (interactive)
  (if eclipse-tab-mode (insert "\t")
    (insert (make-string eclipse-indent-width 32))))

(defun eclipse-indent-line (&optional af flag)
  "Indent current line as ECLiPSe code."
  (interactive)
  (let* ((quotes (eclipse-count-quotes))
         (pos (- (point-max) (point))) beg)	
    ;; if inside string and auto-line-break is on,
    ;; break string and insert additional \"
    (when (and af eclipse-autolinebreak-selected quotes
	       (not eclipse-autoindent-no-string-splitting))
      (save-excursion
	(forward-line -1)
	(end-of-line)
	(insert "\"")))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (when (and af eclipse-autolinebreak-selected quotes
	       (not eclipse-autoindent-no-string-splitting))
      (insert "\"")
      (backward-char))
    (eclipse-indent-region-as-block beg (+ (point) 1) flag)
    (eclipse-backward-char)
    (goto-char (max (point) (- (point-max) pos)))))

(defun eclipse-indent-region-line (indent1)
  ;;Indent current line as ECLiPSe code.
  (let ((pos (- (point-max) (point))) beg
	(indent (if (not indent1) 0 indent1)))
    (beginning-of-line)
    (setq beg (point))
    (cond ((and (not eclipse-tab-mode) (looking-at " *\t[ \t]*"))
	   ;; change tabs to spaces if eclipse-tab-mode is nil
	   (skip-chars-forward " \t")
	   (delete-region beg (point))
	   (indent-to indent))
	  (t
	   (skip-chars-forward " \t")
	   (unless (zerop (- indent (current-column)))
	     (delete-region beg (point))
	     (indent-to indent))))
    (goto-char (max (point) (- (point-max) pos)))))

(defun eclipse-indent-region (&optional msg)
  "Indent current region as ECLiPSe code."
  (interactive)
  (let ((rb (region-beginning)) (re (region-end)) rbegin rend msg-str)
    (goto-char (min rb re))
    (beginning-of-line)
    (setq rbegin (point)
	  rend (max rb re))
    (if (not msg) (setq msg-str "Indenting region...")
      (setq msg-str (concat "Indenting " msg "...")))
    (message msg-str)
    (eclipse-indent-region-as-block rbegin rend)
    (message (concat msg-str "done"))))

(defun eclipse-indent-buffer (&optional msg)
  "Indent buffer as ECLiPSe code."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max))
  (eclipse-indent-region msg))

(defun eclipse-indent-region-as-block (begin end &optional afflag)
  ;; Indent current region as ECLiPSe code
  (let ((stack '())    ; stack for parsing
	pos            ; current position
	(nlflag nil)   ; new line flag, t if at beginning of new line
	(lstnl t)      ; nl flag, t if last read element relevant for indentation was "\n"
	(cmtflag nil)  ; comment flag
	(n 0)          ; match length
	(indnt 0)      ; indentation value
	(flflag 0)     ; first line flag: 0 = head, 1 = first body line, 2 = further body line
	(level 0)      ; nesting level
	aux            ; auxiliary variable
	first-type first-clmn first-level ; values of first stack element
	(idtflag nil)  ; indent flag, t if line is to be indented
	clmn           ; current column
	(cmt2flag nil) ; comment/2 flag, t if inside comment/2 directive
	auxst          ; auxiliary stack
	(eobfl 0)      ; end of buffer flag, to distinguish between empty last buffer lines
	               ; and lines that contain code
	(time (+ eclipse-indent-timeout (cadr (current-time)) 1))
                       ; maxtime for indentation
	(timeflag nil) ; flag, t if indentation should time out
	str)           ; message string
    ;; goto beginning of current clause, the parse down to position begin,
    ;; to find out what the indentation at that position should be,
    ;; then indent every line down to position end
    (eclipse-goto-clause-begin t)
    (while (and (< (point) end) (< eobfl 2) (not (eq timeflag 2)))
      (unless timeflag
	;; check for timeout
	(when (<= time (cadr (current-time)))
	  ;; timeout reached (...it's a very long clause!?)
	  ;; ask if the user wants to continue
	  (beep)
	  (setq str (read-from-minibuffer
		     (concat "Indentation takes more than "
			     (number-to-string eclipse-indent-timeout)
			     " second(s). Continue? (y/n/new timeout) ")))
	  (cond ((= (string-to-char str) 121)
		 (setq timeflag 1)
		 (message "Indenting..."))
		((> (string-to-number str) 0)
		 (message "Do you want to save the new timeout in your custom-set variables? (y/n) ")
		 (when (= (read-char) 121)
		   (require 'cus-edit)
		   (customize-save-variable 'eclipse-indent-timeout (string-to-number str)))
		 (setq eclipse-indent-timeout (string-to-number str)
		       time (+ (1+ eclipse-indent-timeout) (cadr (current-time))))
		 (message "Indenting..."))
		(t
		 (message "Do you want to switch auto-indent off? (y/n) ")
		 (when (= (read-char) 121)
		   (setq eclipse-autoindent-selected nil))
		 (setq timeflag 2)
		 (goto-char end)))))
      (unless idtflag
	;; are we there yet?
	(when (>= (point) begin)
	  ;; we now want to indent!
	  (setq idtflag t)))
      (if (and nlflag cmtflag (looking-at "[ \t]*%"))
	  (progn
	    ;; looking at a comment, or inside one: special treatment
	    (skip-chars-forward " \t")
	    (when idtflag
	      (setq pos (point))
	      (eclipse-indent-region-line (nth 1 (car stack)))
	      (setq end (eclipse-set-end end pos)))
	    (forward-line)
	    (unless (eobp)
	      (beginning-of-line)))
	;; no comment: regular indentation
	(when nlflag
	  ;; when in new line, reset flags
	  (setq nlflag nil
		cmtflag nil
		lstnl t
		n 0))
	(skip-chars-forward " \t")
	(setq pos (point))
	(when (looking-at ":- comment(")
	  ;; comment/2 directive: set flags
	  (setq cmt2flag t
		flflag 0))
	(when (equal (nth 0 (car stack)) 'cmt)
	  ;; if first element from stack is a comment:
	  ;; remove it, it does not influence indentation
	  (setq stack (cdr stack)))
	(if (null stack)
	    ;; empty stack: type of "first" element is 'nul
	    (setq first-type 'nul
		  first-clmn 0
		  first-level 0)
	  ;; else get first element from stack
	  (setq first-type (nth 0 (car stack))
		first-clmn (nth 1 (car stack))
		first-level (nth 2 (car stack))))
	(cond ((and cmt2flag (= flflag 2) (not (null stack)))
	       ;; inside a comment/2 directive
	       (when idtflag
		 (eclipse-indent-region-line eclipse-local-tab-width)
		 (setq end (eclipse-set-end end pos)))
	       (when (equal first-type 'st)
		 (setq stack (cons (list 'st (current-column) level)
				   (cons (list 'el (current-column) level) stack))))
	       ;; indent rest as usual
	       (setq cmt2flag nil))
	      ((or (looking-at "\n") (eobp))
	       ;; at end of line or end of buffer
	       (when (and lstnl idtflag) ; indent line when required
		 (cond ((null stack)
			;; remove indentation if stack is empty
			(eclipse-indent-region-line 0))
		       ((member first-type '(el st))
			;; after regular element, no comma:
			;; indent like last level of opening bracket or similar
			(setq auxst stack
			      stack (eclipse-get-last-type stack '(rb sb cb cd mt rpt)))
			(unless (equal (nth 0 (cadr stack)) 'cd)
			  (setq stack (cdr stack)))
			(if (and (member (nth 0 (car stack)) '(rb sb cb rpt))
				 (not eclipse-indent-closing-parenthesis-to-match-opening))
			    (setq indnt (nth 1 (cadr stack))
				  level (nth 2 (cadr stack))
				  stack (cdr stack))
			  (setq indnt (nth 1 (car stack))
				level (nth 2 (car stack))
				stack (cdr stack)))
			(when eclipse-indent-mode
			  (setq indnt (* level eclipse-indent-width)))
			(if (not indnt)
			    (eclipse-indent-region-line eclipse-local-tab-width)
			  (eclipse-indent-region-line indnt))
			(setq stack auxst))
		       ((and eclipse-esp-selected (equal first-type 'cd))
			;; after :- in esp mode
			(setq indnt (nth 1 (car stack)))
			(when eclipse-indent-mode
			  (setq indnt (* level eclipse-indent-width)))
			(eclipse-indent-region-line indnt))
		       (t
			;; after "special" element: indent as required
			(eclipse-indent-region3 stack flflag)))
		 (setq end (eclipse-set-end end pos)))
	       (setq nlflag t)
	       (when (= flflag 1)
		 ;; this was the first clause body line
		 (setq flflag 2))
	       (forward-line)
	       (unless (eobp)
		 (beginning-of-line)))
	      ((looking-at ",")
	       ;; comma
	       (cond (lstnl ; if at beginning of new line
		      (unless (= level 1) ; adjust level
			(setq level (- level 1)))
		      (when idtflag ; indent when required
			;; cut auxiliary stack back to last element on this level to get the indentation
			(setq auxst (eclipse-get-last-b (eclipse-get-last-level-stack stack level)))
			(cond (eclipse-indent-mode
			       (eclipse-indent-region-line (* level eclipse-indent-width)))
			      ((or (= level 1) (equal (nth 0 (cadr auxst)) 'cd))
			       (eclipse-indent-region-line (nth 1 (car auxst))))
			      (t (eclipse-indent-region-line (nth 1 (cadr auxst)))))
			(setq end (eclipse-set-end end pos)))
		      (setq level (+ level 1)
			    stack (cons (list 'co (current-column) -1) stack))) ; add to stack
		     (t ; else cut stack back to last comma on same level
		      (setq aux (eclipse-get-last-comma stack level)
			    stack (nth 0 aux)
			    level (nth 1 aux)
			    first-type (nth 0 (car stack))
			    first-clmn (nth 1 (car stack))
			    first-level (nth 2 (car stack)))
		      (unless (equal first-type 'co)
			(setq stack (cons (list 'co (current-column) level) stack)))))
	       (forward-char))
	      ((looking-at ";")
	       ;; semicolon
	       (when lstnl ; if at beginning of new line
		 (unless (= level 1) ; adjust level
		   (setq level (- level 1)))
		 (when idtflag ; indent when required
		   ;; cut auxiliary stack back to last element on this level to get the indentation
		   (setq auxst (eclipse-get-last-b (eclipse-get-last-level-stack stack level)))
		   (cond (eclipse-indent-mode
			  (eclipse-indent-region-line (* level eclipse-indent-width)))
			 ((or (= level 1) (equal (nth 0 (cadr auxst)) 'cd))
			  (eclipse-indent-region-line (nth 1 (car auxst))))
			 (t (eclipse-indent-region-line (nth 1 (cadr auxst)))))
		   (setq end (eclipse-set-end end pos))))
	       (setq level (+ level 1)
		     stack (cons (list 'sc (current-column) -1) stack)) ; add to stack
	       (forward-char))
	      ((looking-at "\\.[ \t\n]")
	       ;; end of clause
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (setq stack '() ; reset stack and flags
		     level 0
		     flflag 0
		     cmt2flag nil)
	       (forward-char))
	      ((and (looking-at "<\\?esp:[^ \t\n]+") eclipse-esp-selected)
	       ;; esp command
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-indent-region-line 0)
		 (setq end (eclipse-set-end end pos)))
	       (setq level 1 ; reset stack and flags
		     flflag 1
		     cmt2flag nil
		     stack '((cd 0 0)))
	       (forward-char (length (match-string 0))))
	      ((and (looking-at "<\\?esp") eclipse-esp-selected)
	       ;; esp command
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-indent-region-line 0)
		 (setq end (eclipse-set-end end pos)))
	       (setq level 1 ; reset stack and flags
		     flflag 1
		     cmt2flag nil
		     stack '((cd 2 0)))
	       (forward-char (length (match-string 0))))
	      ((and (looking-at "\\(is\\|with\\)[ \t\n]")
		    (save-excursion
		      (eclipse-backward-char)
		      (looking-at (concat "[ \t]" (match-string 0)))))
	       ;; "is" or "with"
	       (setq n (1- (length (match-string 0))))
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless eclipse-indent-mode
		 (setq level (+ level 1)))
	       (setq stack (cons (list 'inf (current-column) -1) stack)) ; add to stack
	       (forward-char n))
	      ((and (looking-at "do[ \t\n]")
		    (save-excursion
		      (eclipse-backward-char)
		      (looking-at "[ \t]do[ \t\n]")))
	       ;; "do"
	       (when lstnl
		 (unless eclipse-indent-mode
		   (setq level (max 1 (- level 1))))
		 ;; cut stack back to last level
		 (setq stack (eclipse-get-last-level-stack stack level))
		 (when idtflag ; indent when required
		   (eclipse-standard-indent1 stack level (nth 1 (car stack)))
		   (setq end (eclipse-set-end end pos))))
	       (setq stack (cons (list 'do (current-column) level) stack)) ; add to stack
	       (unless eclipse-indent-mode
		 (setq level (+ level 1)))
	       (forward-char 2))
	      ((and eclipse-indent-after-repeat (looking-at "repeat,"))
	       ;; repeat: just like opening bracket
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (setq stack (cons (list 'rpt (current-column) level) stack) ; add to stack
		     level (+ level 1)) ; and increase level
	       (forward-char 7))
	      ((looking-at "\\([0-9]?\\.[0-9]+\\|[a-zA-Z0-9_]+\\|(\\.)\\)")
	       ;; word or number
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 ;; add to stack, unless last element is already some regular 'el element
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char n)) ; jump over word or number
	      ((looking-at "[#.](")
	       ;; "#" or "." with opening parenthesis: almost like normal word or number...
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char)) ; ...but we only jump ahead one character: we want to catch the "("
	      ((looking-at "-\\?->")
	       ;; matching operator
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent1 stack level eclipse-local-tab-width)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (setq stack (cons (list 'mt (current-column) level) stack)) ; add to stack
	       (forward-char n))
	      ((looking-at "\\(:- mode \\|[:?]-\\|-->\\)")
	       ;; colon-dash and related things
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag) ; indent when required
		 (if eclipse-esp-selected
		     (eclipse-standard-indent1 stack level 0)
		   (eclipse-standard-indent1 stack level eclipse-local-tab-width))
		 (setq end (eclipse-set-end end pos)))
	       (setq stack (cons (list 'cd (current-column) level) stack) ; add to stack
		     flflag 1            ; next line will be first body line
		     level (+ level 1))
	       (forward-char n))
	      ((looking-at "[*]?->")
	       ;; "if-then" operator (well, mostly)
	       (setq n (length (match-string 0)))
	       (when lstnl ; if at beginning of new line
		 ;; cut back stack to last opening bracket or semicolon
		 (setq level (- level 1)
		       stack (cdr (eclipse-get-last-sc stack)))
		 (when idtflag ; indent when required
		   (eclipse-standard-indent1 stack level (nth 1 (car stack)))
		   (setq end (eclipse-set-end end pos))))
	       (if (eclipse-in-list stack) ; if in list, it's an operator
		   (setq stack (cons (list 'inf (current-column) -1) stack))
		 (setq stack (cons (list 'if (current-column) level) stack))) ; else it's an "if-then"
	       (when (or lstnl (not eclipse-indent-mode))
		 (setq level (+ level 1)))
	       (forward-char n))
	      ((looking-at "#<?=>")
	       ;; constraints #=> and #<=>
	       (setq n (length (match-string 0)))
	       (when lstnl ; if at beginning of new line
		 (unless eclipse-indent-mode
		   (setq level (- level 1)))
		 ;; cut back stack to last level
		 (setq stack (eclipse-get-last-level-stack stack level))
		 (when idtflag ; indent when required
		   (eclipse-standard-indent1 stack level (nth 1 (car stack)))
		   (setq end (eclipse-set-end end pos))))
	       (setq stack (cons (list 'if (current-column) level) stack)) ; add to stack
	       (unless eclipse-indent-mode
		 (setq level (+ level 1)))
	       (forward-char n))
	      ((looking-at "'")
	       ;; inverted comma: start of quoted atom
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (unless (eclipse-goto-end-of-quote) ; jump to end of quote
		 (setq stack (cons (list 'qu (current-column) level) stack)))) ; add to stack
	      ((looking-at "\"")
	       ;; beginning of string
	       (when (and lstnl idtflag) ; indent when required
		 (cond (eclipse-indent-mode
			(eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
		       ((equal first-type 'st) ; multi-line string
			(eclipse-indent-region-line (nth 1 (car stack))))
		       (t (eclipse-indent-region3 stack flflag)))
		 (setq end (eclipse-set-end end pos)))
	       (cond ((equal (nth 0 (car stack)) 'st) t)
		     ((equal (nth 0 (car stack)) 'el)
		      (setq stack (cons (list 'st (current-column) level) stack)))
		     (t
		      (setq stack (cons (list 'st (current-column) level)
					(cons (list 'el (current-column) level) stack)))))
	       (eclipse-goto-end-of-string)) ; add to stack
	      ((looking-at "%+")
	       ;; comment
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag)
		 (if (looking-at "%[^%]")
		     (eclipse-standard-indent2 stack level flflag); single "%": indent as code
		   (eclipse-indent-region-line 0)) ;; multiple "%": indent to column 0
		 (setq end (eclipse-set-end end pos)))
	       (setq stack (cons (list 'cmt (current-column) level n) stack) ; add to stack
		     nlflag t
		     cmtflag t)
	       (forward-line) ; foward, since rest of line will be comment
	       (unless (eobp)
		 (beginning-of-line)))
	      ((looking-at "/\\*")
	       ;; beginning of c-style comment: do nothing
	       (unless (re-search-forward "\\*/" (point-max) t) ; is it closed? then jump there
		 (goto-char (point-max))))
	      ((and (looking-at "\\?>") eclipse-esp-selected)
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-indent-region-line 0)
		 (setq end (eclipse-set-end end pos)))
	       ;; beginning of HTML-section in ESP page: do nothing
	       (if (re-search-forward "<\\?esp" (point-max) t) ; is it closed? then jump there
		   (backward-char (length (match-string 0)))
		 (goto-char (point-max))))
	      ((looking-at "[({[]")
	       ;; opening bracket
	       (let ((symbol (cond ((looking-at "(") 'rb)
				   ((looking-at "\\[") 'sb)
				   (t 'cb))))
		 (when (and lstnl idtflag) ; indent when required
		   (eclipse-standard-indent2 stack level flflag)
		   (setq end (eclipse-set-end end pos)))
		 (unless (equal (nth 0 (car stack)) 'el)
		   (setq stack (cons (list 'el (current-column) level) stack)))
		 (setq stack (cons (list symbol (current-column) level) stack) ; add to stack
		       level (+ level 1)) ; and increase level
		 (forward-char)))
	      ((looking-at "[])}]")
	       ;; closing bracket
	       (let* ((symbol (cond ((looking-at ")") 'rb)
				    ((looking-at "\\]") 'sb)
				    (t 'cb)))
		      (auxl (cond ((looking-at ")") '(sb cb))
				  ((looking-at "\\]") '(rb cb))
				  (t '(rb sb))))
		      (auxfl (member first-type (append '(co sc inf op do if) auxl))))
		 (when auxfl ; if last element was some kind of operator, indent now...
		   (when (and lstnl idtflag) ; ...when required
		     (eclipse-standard-indent2 stack level flflag)
		     (setq end (eclipse-set-end end pos))))
		 ;; cut stack back to matching opening bracket
		 (setq stack (eclipse-get-last symbol stack))
		 (when (null stack) ; oops...
		   (error "Empty stack. Check for '.' instead of ','"))
		 (if eclipse-indent-closing-parenthesis-to-match-opening
		     (setq indnt (nth 1 (car stack))
			   level (nth 2 (car stack))
			   stack (cdr stack))
		   (setq indnt (nth 1 (cadr stack))
			 level (nth 2 (cadr stack))
			 stack (cdr stack)))
		 ;;(when (eq level 0)
		 ;; setq indnt 0))
		 (when (not auxfl) ; if last element was no operator, indent now...
		   (when (and lstnl idtflag) ; ...when required
		     (eclipse-standard-indent1 stack level indnt)
		     (setq end (eclipse-set-end end pos))))
		 (forward-char)))
	      ((and eclipse-indent-after-repeat (looking-at "!"))
	       ;; cut
	       (let* (aux_stack)
		 ;; cut stack back to matching opening bracket
		 (setq aux_stack (eclipse-get-last 'rpt stack))
		 (cond ((null aux_stack)
			;; cut was stand-alone
			;; just the same as word or number...
			(when (and lstnl idtflag)
			  (eclipse-standard-indent2 stack level flflag)
			  (setq end (eclipse-set-end end pos)))
			(unless (equal (nth 0 (car stack)) 'el)
			  (setq stack (cons (list 'el (current-column) level) stack))))
		       (t
			;; found a repeat:
			;; treat just like brackets
			(setq stack aux_stack)
			(if eclipse-indent-closing-parenthesis-to-match-opening
			    (setq indnt (nth 1 (car stack))
				  level (nth 2 (car stack))
				  stack (cdr stack))
			  (setq indnt (nth 1 (cadr stack))
				level (nth 2 (cadr stack))
				stack (cdr stack)))
			(when (and lstnl idtflag)
			  (eclipse-standard-indent1 stack level indnt)
			  (setq end (eclipse-set-end end pos)))))
		 (forward-char)))
	      ((looking-at "|")
	       ;; tail operator
	       (when (and lstnl idtflag)
		 (cond ((member first-type '(co sc inf op do if rb sb cb rpt))
			;; if last element on stack is operator or bracket, indent normally
		    	(eclipse-standard-indent2 stack level flflag)
			(setq end (eclipse-set-end end pos)))
		       (t
			;; else search last bracket and use that indentation
		      	(eclipse-standard-indent1 stack level (nth 1 (cadr (eclipse-get-last-b stack))))
			(setq end (eclipse-set-end end pos)))))
	       (setq stack (cons (list 'rs (current-column) level) stack)) ; add to stack
	       (forward-char))
	      ((looking-at "\\(!\\|\\\\\\+\\)")
	       ;; cut or negation
	       ;; just the same as word or number...
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char n))
	      ((looking-at "\\(\\+[),]\\|~[ \t(]\\)")
	       ;; "+" before ")" or ",", or sound negation
	       ;; ...the same, but we know it's only one charater to jump ahead
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char))
	      ((looking-at "\\(#\\(::\\|#\\|=<?\\|<=?\\|>=?\\|\\\\\\(\\+\\|=\\)\\)\\|$\\(::\\|=<?\\|<=?\\|>\\|\\\\=\\)\\|`\\(::\\|<>?\\|=\\)\\|\\*\\(=<?\\|>=\\)\\|&\\(::\\|=<?\\|<\\|>=?\\|\\\\=\\)\\|`\\(::\\|<>?\\|=\\)\\|\\*\\(=<?\\|>=\\)\\|@\\(=?<\\|<\\|>=?\\)\\|::?\\|=\\.\\.\\|[@^&]\\|=[:\\]?=\\|[~]?=<?\\|<<?\\|>[=>]?\\|\\\\==?\\)")
	       ;; comparison or constraint operators
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag) ; indent when required
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (setq clmn (current-column))
	       (forward-char n)
	       (if (or (looking-at "(") (equal (nth 0 (car stack)) 'co))
		   ;; if used as predicate name, treat as regular 'el element
		   (unless (equal (nth 0 (car stack)) 'el)
		     (setq stack (cons (list 'el clmn level) stack)))
		 ;; else treat as 'inf: infix operator
		 (unless eclipse-indent-mode
		   (setq level (+ level 1)))
		 (setq stack (cons (list 'inf clmn -1) stack))))
	      ((looking-at "#\\(\\\\/\\|/\\\\\\)")
	       ;; "#\/" or "#/\" : almost the same...
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (setq clmn (current-column))
	       (forward-char n)
	       (if (or (looking-at "(") (equal (nth 0 (car stack)) 'co))
		   (unless (equal (nth 0 (car stack)) 'el)
		     (setq stack (cons (list 'el clmn level) stack)))
		 ;; ...but it's like a semicolon
		 (unless eclipse-indent-mode
		   (setq level (+ level 1)))
		 (setq stack (cons (list 'sc clmn -1) stack))))
	      ((looking-at "\\(\\*\\|\\+\\|-\\|/[/\\]?\\|\\\\/?\\)")
	       ;; math operator: again almost the same...
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (setq clmn (current-column))
	       (forward-char n)
	       (if (or (looking-at "(") (equal (nth 0 (car stack)) 'co))
		   (unless (equal (nth 0 (car stack)) 'el)
		     (setq stack (cons (list 'el clmn level) stack)))
		 ;; ...but it's an 'op opreator
		 (setq stack (cons (list 'op clmn -1) stack))))
	      ((looking-at "\\.\\.\\.+")
	       ;; three or more dots: treat like a word
	       (setq n (length (match-string 0)))
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char n))
	      ((looking-at "\\.\\.")
	       ;; two dots: infix operator
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless eclipse-indent-mode
		 (setq level (+ level 1)))
	       (setq stack (cons (list 'inf (current-column) -1) stack))
	       (forward-char 2))
	      ((looking-at "\\.[^ \t\n,]")
	       ;; one dot, but not followed by whitespace, newline, or comma
	       ;; treat like word
	       (when (and lstnl idtflag)
		 (eclipse-standard-indent2 stack level flflag)
		 (setq end (eclipse-set-end end pos)))
	       (unless (equal (nth 0 (car stack)) 'el)
		 (setq stack (cons (list 'el (current-column) level) stack)))
	       (forward-char))
	      (t (forward-char))) ; else go to next character
	(setq lstnl nil))
      (cond ((and (eobp) (bolp) (not (null stack))) (setq eobfl 1)) ; end of buffer
	    ((eobp) (setq eobfl 2)) ; end of buffer, but line not empty
	    (t t)))
    (goto-char end)
    (cond ((equal timeflag 2)
	   ;; indentation timed out: just indent like preceding line
	   (backward-char)
	   (save-excursion
	     (forward-line -1)
	     (beginning-of-line)
	     (skip-chars-forward " \t")
	     (setq indnt (current-column)))
	   (eclipse-standard-indent1 stack level indnt))
	  ((equal timeflag 1)
	   (message "Indenting...done"))
	  (t t))))

(defun eclipse-standard-indent1 (stack level width)
  ;; standard indent cond block 1
  (cond ((null stack)
	 ;; empty stack: set indentation to 0, if clause heads are subject to indentation
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 ;; "classic" indentation
	 (eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
	(width (eclipse-indent-region-line width)) ; indentation is known
	(t (eclipse-indent-region-line eclipse-local-tab-width)))) ; else indent to standard tab-width

(defun eclipse-standard-indent2 (stack level flflag)
  ;; standard indent cond block 2: cases 1 and 2 as in eclipse-standard-indent1
  (cond ((null stack)
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 (eclipse-indent-region-line (max eclipse-local-tab-width (* level eclipse-indent-width))))
        ;((eq level 1)
        ; (eclipse-indent-region-line eclipse-local-tab-width))
	(t (eclipse-indent-region3 stack flflag)))) ; indent according to stack

(defun eclipse-set-end (end pos)
  ;; update end of region
  (+ end (- (point) pos)))

(defun eclipse-indent-region3 (stack flag)
  ;; standard indentation function for lines in region
  (let ((typ (nth 0 (car stack))) (column (nth 1 (car stack)))
	(level (nth 2 (car stack))) (auxst stack))
    ;; what's the type of the top element in the stack?
    ;; i.e., the element put on the stack before the one that we now want to indent
    (cond ((member typ '(cd mt)) ; colon-dash or matching operator: tab-width
	   (eclipse-indent-region-line eclipse-local-tab-width))
	  ((member typ '(if do)) ; "->" or "do": like last bracket, colon-dash, or semicolon
	   (setq auxst (eclipse-get-last-sc auxst))
	   (when (and eclipse-indent-to-parenthesis (not (eq (nth 0 (cadr auxst)) 'cd)))
	     (setq auxst (cdr auxst)))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'sc) ; semicolon : like last bracket, colon-dash, or "->"
	   (setq auxst (eclipse-get-last-if auxst))
	   (eclipse-indent-region-line (nth 1 (car auxst))))
	  ((member typ '(rb sb cb rpt)) ; opening bracket: increase indentation by indent-width
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'co) ; comma
	   (cond ((and (= flag 2) eclipse-first-line-std-indent (= level 1))
		  ;; on level 1: like the indentation of the other lines on that level
		  (setq column (min eclipse-local-tab-width (nth 1 (cadr stack))))
		  (eclipse-indent-region-line column))
		 (t
		  ;; else like the last 'el element in the stack
 		  (setq column (nth 1 (cadr stack)))
 		  (eclipse-indent-region-line column))))
	  ((member typ '(el st qu))
	   ;; regular 'el element, or string or quoted atom: like last element of previous level
	   (setq auxst (eclipse-get-last-level-stack auxst (- level 1)))
	   (setq column (nth 1 (car stack)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'rs)
	   ;; "|": like last bracket or colon-dash
	   (setq auxst (eclipse-get-last-b auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'inf)
	   ;; infix operator: like last bracket or colon-dash or regular element
	   (setq auxst (eclipse-get-last-b-or-el auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'op)
	   ;; math operator: like last bracket or colon-dash or infix
	   (setq auxst (eclipse-get-last-inf auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'cmt)
	   ;; comment: ignore
	   (setq auxst (cdr auxst))
	   (eclipse-indent-region3 auxst flag)))))

(defun eclipse-in-list (stack)
  ;; returns t if stack contains element of type 'sb
  (cond ((null stack) nil)
	((equal (nth 0 (car stack)) 'sb) t)
	(t (eclipse-in-list (cdr stack)))))

(defun eclipse-get-last-comma (stack level)
  ;; return stack as it was at last level
  (let* ((last nil) (el (nth 0 (car stack)))
	 (found (member el '(co do if sc mt cd rb sb cb rpt))))
    (while (not (or (null stack) found))
      (setq last (car stack)
	    stack (cdr stack)
	    el (nth 0 (car stack)))
      (cond ((and (equal el 'inf) (not eclipse-indent-mode))
	     (setq level (- level 1)))
	    ((member el '(co do if sc mt cd rb sb cb rpt))
	     (setq found t))
	    (t t)))
    (while (not (or (null stack) (< (nth 2 (car stack)) level)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	(list stack level)
      (list (cons last stack) level))))

(defun eclipse-get-last-level-stack (stack level)
  ;; return stack as it was at last level
  (while (not (or (null stack) (= (nth 2 (car stack)) level)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last (typ stack)
  ;; return stack as it was at last typ
  (while (not (or (null stack) (equal (nth 0 (car stack)) typ)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last-type (stack typelist)
  ;; return stack as it was at point after last element of type in list
  (let ((last nil))
    (while (not (or (null stack) (member (nth 0 (car stack)) typelist)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	stack
      (cons last stack))))

(defun eclipse-get-last-b (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack '(rb sb cb cd mt rpt)))

(defun eclipse-get-last-b-or-el (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack '(rb sb cb cd mt rpt el)))

(defun eclipse-get-last-inf (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack '(rb sb cb cd mt rpt inf)))

(defun eclipse-get-last-if (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack '(rb sb cb cd mt rpt if)))

(defun eclipse-get-last-sc (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack '(rb sb cb cd mt rpt sc)))

(defun eclipse-indent-predicate ()
  "Indent current predicate as ECLiPSe code."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-mark-predicate)
    (eclipse-indent-region "predicate")
    (beginning-of-line)
    (skip-chars-forward " \t")))

(defun eclipse-indent-clause ()
  "Indent current clause as ECLiPSe code."
  (interactive)
  (eclipse-mark-clause)
  (eclipse-indent-region "clause")
  (beginning-of-line)
  (skip-chars-forward " \t"))

;;
;; Mark regions
;;

(defun eclipse-mark-buffer ()
  "Mark complete buffer."
  (interactive)
  (push-mark (point-min))
  (goto-char (point-max)))

(defun eclipse-mark-predicate ()
  "Mark current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-goto-clause-end)
    (eclipse-goto-predicate-begin)
    (push-mark (point))
    (eclipse-goto-predicate-end)))

(defun eclipse-mark-clause ()
  "Mark current clause."
  (interactive)
  (eclipse-goto-clause-end)
  (let ((p (point)))
    (eclipse-goto-clause-begin)
    (push-mark (point))
    (goto-char p)))

;;
;; Auxiliary functions
;;

(defun eclipse-check-clause-begin ()
  ;; check if at beginning of clause
  (let ((pnt (point)))
    (save-excursion
      (eclipse-goto-clause-end)
      (eclipse-goto-clause-begin)
      (eq pnt (point)))))

(defun eclipse-check-left-empty ()
  ;; check if rest of the current line to the left is empty
  (let ((flag t))
    (save-excursion
      (while (and flag (not (bolp)))
	(backward-char)
	(when (not (looking-at "[ \t]"))
	  (setq flag nil))))
    flag))

(defun eclipse-backward-char (&optional n)
  ;; safe backward-char. no error on bumping into beginning of buffer
  (backward-char (min (1- (point)) (or n 1))))

(defun eclipse-count-quotes ()
  ;; return start column of string, if there is one open at end of line
  ;; else return nil
  (let ((quotes nil) (editpoint (1- (point))) (sq nil) (dq nil) (cmt nil))
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (while (and (not (>= (point) editpoint)) (not cmt))
	(cond ((and (looking-at "[^\\]\"") (not sq))
	       (if quotes
		   (setq quotes nil)
		 (setq quotes (current-column)))
	       (setq dq (not dq)))
	      ((and (looking-at "0'[^\n]") (not sq))
	       (forward-char 2))
	      ((looking-at "[1-9]'[0-9a-zA-Z]+")
	       (forward-char 2))
	      ((and (looking-at "[^\\]'") (not dq))
	       (forward-char 2)
	       (setq sq (not sq)))
	      ((and (looking-at "%") (not dq) (not sq))
	       (setq cmt t))
	      ((and (looking-at "/\\*") (not dq) (not sq))
	       (forward-char 2)
	       (if (looking-at "[^\n]*\\*/")
		   (re-search-forward "\\*/" (point-max) t)
		 (setq cmt t)))
	      (t t))
	(forward-char)))
    quotes))

(defun eclipse-end-of-clause-line ()
  ;; go to end of goals in this line
  (let* ((eolpos (save-excursion (end-of-line) (point)))
	 (comment nil) (empty t)
	 (strmacro '(lambda (symbol eolpos0 comment0)
		      (let ((flag nil))
			(forward-char)
			(cond ((search-forward symbol eolpos0 t)
			       (backward-char)
			       (while (not (or flag comment0))
				 (cond ((eq eolpos0 (point))
					(setq flag t))
				       ((save-excursion
					  (eclipse-backward-char)
					  (looking-at (concat "\\\\" symbol)))
					(forward-char)
					(when (search-forward symbol eolpos0 t)
					  (backward-char)))
				       ((looking-at "[ \t]*\n")
					(setq comment0 t))
				       (t
					(setq flag t)
					(forward-char)))))
			      (t (setq comment0 t)))
			comment0))))
    (beginning-of-line)
    (while (and (not (= (point) eolpos)) (not comment))
      (cond ((looking-at "0'") ; base operator: jump ahead
	     (forward-char 2)
	     (or (looking-at "\n")
		 (forward-char)))
	    ((looking-at "[1-9]+'[0-9a-zA-Z]+") ; base operator: jump ahead
	     (re-search-forward "[^0-9a-zA-Z']" (point-max) t)
	     (backward-char))
 	    ((looking-at "'") ; quoted atom: jump to end
	     (setq comment (funcall strmacro "'" eolpos comment)))
 	    ((looking-at "\"") ; string: jump to end
	     (setq comment (funcall strmacro "\"" eolpos comment)))
	    ((looking-at "%") (setq comment t)) ; comment
	    ((looking-at "\\(/\\*[^\n]*\\*/\\)?[ \t]*\n") ; empty to end of line
	     (setq comment t))
	    ((looking-at "/\\*[^\n]*\\*/") ; jump over short c-style comments
	     (re-search-forward "\\*/" (point-max) t))
	    ((looking-at "/\\*") (setq comment t)) ; beginning of multi-line comment
	    ((looking-at "\\*/[ \t]*\n")
	     ;; end of a multi-line comment: find beginning
	     (re-search-backward "/\\*" (point-min) t)
	     (cond ((save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*/\\*"))
		    (while empty
		      (forward-line -1)
		      (beginning-of-line)
		      (if (bobp) (setq empty nil)
			(skip-chars-forward " \t")
			(or (looking-at "\\(%\\|\n\\)")
			    (setq empty nil))))
		    (setq empty t
			  eolpos (save-excursion (end-of-line) (point))))
		   (t (setq comment t))))
	    (t (forward-char))))
    (skip-chars-backward " \t")))

(defun eclipse-jump-over-strings (&optional mcflag eobflag cmtflag)
  ;; jump over constructs "...", '...', /*...*/, ?>...<?esp, %..., and whitespace
  ;; mcflag : if t, we will jump over multi-line c-style comments
  ;; eobflag : if t, we will position pointer at end if jumping beyond fails
  ;; cmtflag : if t, we don't jump over quotes
  (let ((found nil))
    (while (and (looking-at "[ \t\"'/%\n]") (not found))
      (skip-chars-forward " \t\n")
      (cond ((looking-at "%")
	     (forward-line)
	     (if (eobp)
		 (setq found t)
	       (beginning-of-line)))
	    ((looking-at "/\\*")
	     (cond (mcflag
		    (unless (re-search-forward "\\*/" (point-max) t)
		      (if eobflag (goto-char (point-max)))
		      (setq found t)))
		   ((looking-at "[^\n]*\\*/")
		    (forward-char 2)
		    (re-search-forward "\\*/" (point-max) t))
		   (t (setq found t))))
	    ((looking-at "\\?>")
	     (unless (re-search-forward "<\\?esp" (point-max) t)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((and mcflag (looking-at "\""))
	     (eclipse-goto-end-of-string))
	    ((and mcflag (looking-at "'"))
	     (if (not cmtflag)
		 (eclipse-goto-end-of-quote)
	       (setq found t)))
	    (t (setq found t))))))

(defun eclipse-goto-end-of-quote ()
  ;; goto to the end of the current quoted atom
  (eclipse-backward-char)
  (cond ((looking-at "0'") ; actually, just a base operator
	 (forward-char 2)
	 (or (looking-at "\n")
	     (forward-char)))
	((looking-at "[1-9]'[0-9a-zA-Z]") ; ditto
	 (forward-char 3))
	(t
	 (forward-char)
	 (eclipse-goto-end-of "'"))))

(defun eclipse-goto-end-of-string ()
  ;; goto to the end of the current string
  (eclipse-goto-end-of "\""))

(defun eclipse-goto-end-of (str)
  ;; goto to the end of the current string or quoted atom
  (let ((str1 (concat "[^\\]" str)))
    (if (re-search-forward str1 (point-max) t)
	t
      (goto-char (point-max))
      nil)))

(defun eclipse-percent-message (str length last &optional base)
  ;; print a message "Str... (XX%)"
  (let ((percent (truncate (* 100 (/ (* 1.0 (if base base (point)))
				     (* 1.0 length))))))
    (cond ((>= percent (+ last 10))
	   (message (concat str "... (" (number-to-string percent) "%%)"))
	   percent)
	  (t last))))

;;
;; Go-to commands
;;

(defun eclipse-goto-clause-begin (&optional flag)
  "Goto the beginning of the current clause."
  (interactive)
  (cond (eclipse-esp-selected
	 (let ((last (point)) (pnt (point)) (found nil) maxpnt)
	   ;; else go to beginning of buffer, search all clause"<?esp:" tags until we are past pnt
	   ;; the last "<?esp:" tag we found is the one we are looking for
	   (goto-char (point-min))
	   (beginning-of-line)
	   (setq last (point))
	   (if (<= pnt last)
	       (setq maxpnt (point-max))
	     (setq maxpnt pnt))
	   (while (and (not found) (not (eobp)))
	     (cond ((> (point) maxpnt)
		    (goto-char pnt)
		    (setq found t))
		   ((re-search-forward "<\\?esp" maxpnt t)
		    ;;(re-search-forward "<\\?esp\\(:[^ \t\n]+\\)?" maxpnt t)
		    (cond ((< (point) pnt)
			   (setq last (- (point) 5)))
			  (t (setq found t))))
		   (t
		    (goto-char pnt)
		    (setq found t))))
	   (goto-char last)))
	((and eclipse-quick-jumps-selected (not flag))
	 ;; if quick jumps selected, assume clause begins at previous empty line
	 (let ((found nil))
	   (if (bolp) (eclipse-backward-char))
	   (while (and (not found) (not (bobp))) ; first jump over any white lines
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (forward-line -1)
	       (setq found t)))
	   (setq found nil)
	   (while (and (not found) (not (bobp))) ; then look for next white line
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (setq found t)
	       (forward-line -1)))
	   (or (and (bobp) (not (looking-at "[ \t]*\n")))
	       (progn (forward-line) (beginning-of-line)))))
	(t
	 (let ((last (point)) (pnt (point)) (found nil) maxpnt auxpnt)
	   ;; else go to beginning of buffer, search all clause beginnings until we are past pnt
	   ;; the last clause beginning we found is the one we are looking for
	   (goto-char (point-min))
	   (beginning-of-line)
	   (eclipse-jump-over-strings t nil t)
	   (setq last (point))
	   (if (<= pnt last)
	       (setq maxpnt (point-max))
	     (setq maxpnt pnt))
	   (while (and (not found) (not (eobp)))
	     (cond ((> (point) maxpnt)
		    (goto-char pnt)
		    (setq found t))
		   ((eclipse-goto-clause-end t)
		    (cond ((>= (point) maxpnt)
			   (goto-char pnt)
			   (setq found t))
			  (t
			   (setq auxpnt (point))
			   (eclipse-jump-over-strings t nil t)
			   (if (not (save-excursion
				      (re-search-forward "[.\"'%/]" (point-max) t)))
			       (setq found t)
			     (cond ((eq auxpnt (point))
				    (setq found t))
				   ((< (point) pnt)
				    (setq last (point)))
				   (t (setq found t)))))))
		   (t
		    (goto-char pnt)
		    (setq found t))))
	   (goto-char last)))))

(defun eclipse-goto-clause-end (&optional called-from-head)
  "Goto the end of the current clause."
  (interactive)
  (cond (eclipse-esp-selected
	 (if (looking-at "\\?>")
	     (forward-char 2))
	 (if (re-search-forward "\\?>" (point-max) t)
	     (backward-char 2)))
	(eclipse-quick-jumps-selected
	 ;; if quick jumps selected, assume clause ends at next empty line
	 (let ((found nil))
	   (if (save-excursion (eclipse-backward-char) (looking-at ","))
	       (eclipse-jump-over-strings t t)
	     (if (save-excursion
		   (eclipse-backward-char 2)
		   (looking-at "[^.].[^.0-9]"))   ; should we update the regexp to include "." as atom?
		 (forward-line))
	     (beginning-of-line)
	     (eclipse-jump-over-strings t t))
	   (while (and (not found) (not (eobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (forward-line)
	       (setq found t)))
	   (setq found nil)
	   (while (and (not found) (not (eobp)))
	     (beginning-of-line)
	     (if (looking-at "[ \t]*\n")
		 (setq found t)
	       (forward-line)))
	   (eclipse-end-of-clause-line)
	   (if (eobp)
	       (skip-chars-backward " \t\n")
	     (forward-line -1)
	     (eclipse-end-of-clause-line))))
	(t
	 (let ((found nil) (pnt (point)))
	   ;; else go to beginning of clause and search for end
	   (eclipse-jump-over-strings t t)
	   (unless called-from-head
	     (eclipse-goto-clause-begin))
	   (eclipse-goto-predicate t pnt))))
  t)

(defun eclipse-goto-predicate (&optional endflag clauseflag)
  ;; common loop for goto-predicate functions
  (let ((found nil) (last nil) (pnt (point)))
    (while (not found)
      (if (not (re-search-forward "[.\"'%/]" (point-max) t))
	  (setq found t)
	(eclipse-backward-char)
	(eclipse-jump-over-strings t endflag)
	(cond ((and endflag (eobp))
	       (setq found t))
	      ((looking-at "[.][.]+")
	       (forward-char (length (match-string 0))))
	      ((looking-at "[.][^ \t\n]")
	       (forward-char))
	      ((looking-at "[.]")
	       (forward-char)
	       (cond (clauseflag ; searching for clause end
		      (unless (<= (point) clauseflag)
			(setq found t)))
		     (endflag ; searching for predicate end
		      (setq last (point))
		      (eclipse-jump-over-strings t t)
		      (unless (string-equal eclipse-predicate-template
					    (eclipse-get-current-predicate-template))
			(setq found t)
			(goto-char last)))
		     (t ; searching for predicate begin
		      (eclipse-jump-over-strings t nil t)
		      (if (string-equal eclipse-predicate-template
					(eclipse-get-current-predicate-template))
			  (setq found t)
			(forward-char)))))
	      (t (forward-char)))))))

(defun eclipse-goto-predicate-begin ()
  "Goto the beginning of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (if eclipse-quick-jumps-selected
	;; if quick jumps selected, go to clause begin, get template
	;; iterate until template does not match anymore, then return to last match
	(let ((found nil) (last nil) (template nil))
	  (eclipse-goto-clause-begin)
	  (setq template (eclipse-get-current-predicate-template)
		last (point))
	  (while (and (not found) (not (bobp)))
	    (eclipse-goto-clause-begin)
	    (if (string-equal template (eclipse-get-current-predicate-template))
		(setq last (point))
	      (setq found t)))
	  (or (not found) (goto-char last)))
      (let ((found nil) pnt)
	;; else extract current template, go to beginning of buffer,
	;; search all clause beginnings until we find the first clause with matching template
	(eclipse-goto-clause-begin)
	(unless (looking-at "[:?]-")
	  (setq pnt (point)
		eclipse-predicate-template (eclipse-get-current-predicate-template))
	  (goto-char (point-min))
	  (eclipse-jump-over-strings t nil t)
	  (if (string-equal eclipse-predicate-template
			    (eclipse-get-current-predicate-template)) t
	    (eclipse-jump-over-strings t nil)
	    (eclipse-goto-predicate)
	    (if (> (point) pnt) (goto-char pnt))))))))
  
(defun eclipse-goto-predicate-end ()
  "Goto the end of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (if eclipse-quick-jumps-selected
	;; if quick jumps selected, go to clause begin, get template
	;; then go to next clause end, jump to next clause begin and check template,
	;; iterate until template does not match anymore, then return to last clause end
	(let ((found nil) (last nil) (template nil))
	  (eclipse-goto-clause-end)
	  (setq last (point))
	  (save-excursion
	    (eclipse-goto-clause-begin)
	    (setq template (eclipse-get-current-predicate-template)))
	  (while (and (not found) (not (eobp)))
	    (eclipse-goto-clause-end)
	    (cond ((eq last (point)) (setq found t))
		  ((save-excursion
		     (eclipse-goto-clause-begin)
		     (string-equal template (eclipse-get-current-predicate-template)))
		   (setq last (point)))
		  (t (setq found t))))
	  (or (not found) (goto-char last)))
      (let ((found nil) (last nil))
	;; else extract current template, go to end of clause,
	;; search all following clause beginnings until we find the
	;; first clause with different template, then return to last clause end
	(eclipse-jump-over-strings t t)
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(skip-chars-forward " \t")
	(if (looking-at "[:?]-")
	    (eclipse-goto-clause-end)
	  (setq eclipse-predicate-template (eclipse-get-current-predicate-template))
	  (eclipse-goto-predicate t))))))

;;
;; Speedbar support
;;

(defun eclipse-goto-prev-index-position ()
  ;; go to the previous entry in the index
  (beginning-of-line)
  (if (bobp)
      nil
    (let ((now (point)))
      (eclipse-goto-predicate-begin)
      (not (eq now (point))))))

(defun eclipse-create-index ()
  ;; creates an index for the speedbar.
  ;; this function scans the buffer top-down, which is faster than scanning
  ;; bottom-up, as is standard in speedbar/imenu, since we can use
  ;; the information that the point is always at the beginning of a
  ;; predicate when the next predicate is searched
  (save-excursion
    (let ((index-alist '()) (index-dir-alist '())
	  (length (- (point-max) (point-min))) (pos -1) (pc 0) name
	  entry)
      (message "Indexing...")
      (goto-char (point-min))
      (eclipse-goto-clause-begin)   ;; quick and dirty...
      ;; Search for the function
      (while (and (not (eobp)) (< (point) (point-max)) (not (eq pos (point))))
	(setq pc (eclipse-percent-message "Indexing" length pc)
	      pos (if imenu-use-markers (point-marker) (point))
	      name (eclipse-extract-index-name)
	      entry (cons name pos))
	(if (looking-at "[:?]-")
	    ;; if directive, at to list of directives
	    (setq index-dir-alist (cons entry index-dir-alist))
	  ;; else add to the list of normal entries
	  (setq index-alist (cons entry index-alist)))
	;; go to next predicate
	(eclipse-goto-predicate-end)
	(eclipse-jump-over-strings t t t)
	(skip-chars-forward " \t"))
      (message "Indexing...done.")
      (and index-dir-alist
	   (setq index-alist (cons (cons "Directives" index-dir-alist) index-alist)))
      index-alist)))

(defun eclipse-extract-index-name ()
  ;; get the name to be listed in the index
  (let (start name)
    (save-excursion
      (cond ((looking-at "[ \t]*[:?]-")
	     ;; directive: extract all of it
	     (skip-chars-forward " \t")
	     (forward-char 2)
	     (re-search-forward "[a-z]" (point-max) t)
	     (setq start (- (point) 1))
	     (re-search-forward "[.\n%]" (point-max) t)
	     (backward-char)
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    ((looking-at "[A-Z(]")
	     ;; variable: probably a clause head with infix operator
	     ;; extract until colon-dash or end of line
	     (setq start (point))
	     (re-search-forward "\\(\n\\|[:?]-\\)" (point-max) t)
	     (backward-char (length (match-string 0)))
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    (t ; else extract predicate template
	     (setq name (eclipse-get-current-predicate-template t)))))
    name))

;;
;; Predicate template & args and other "edit" functions
;;

(defun eclipse-get-current-predicate-template (&optional specflag)
  ;; return the template for the current predicate
  ;; if specflag = t, return the specification for the current predicate
  ;; problem: cannot handle operators in clause heads:
  ;; X = Y :- ...
  ;; ++ X :- ...
  ;; X ++ :- ...
  ;; since Emacs doesn't know about the operator definitions. And since the
  ;; arguments in the clause heads may be atoms (just like the operators),
  ;; it is impossible to guarantee the correct behaviour in this case!
  (let ((fb (point)) fe functor args (cc 0) (found nil) (bc 0))
    ;; we are at the beginning of a clause...
    (save-excursion
      ;; search end of predicate name
      (re-search-forward "\\([\n({.]\\|[:?]-\\)" (point-max) t)
      (eclipse-backward-char (length (match-string 0)))
      (skip-chars-backward " \t")
      (setq fe (point)
	    functor (buffer-substring-no-properties fb fe))
      (cond ((looking-at "(")
	     ;; if there are any arguments, count them
	     (while (not found)
	       (cond ((eobp)
		      (beep)
		      (message "Cannot extract predicate template: end of buffer reached")
		      (setq cc -1
			    found t))
		     ((looking-at "'")
		      (eclipse-goto-end-of-quote))
		     ((looking-at "\"")
		      (eclipse-goto-end-of-string))
		     ((looking-at "%")
		      (forward-line)
		      (unless (eobp)
			(beginning-of-line)))
		     ((looking-at "/\\*")
		      (unless (re-search-forward "\\*/" (point-max) t)
			(goto-char (point-max))))
		     ((looking-at "[({[]")
		      (forward-char)
		      (setq bc (+ bc 1)))
		     ((looking-at "[]})]")
		      (forward-char)
		      (setq bc (- bc 1))
		      (when (zerop bc)
			(setq found t)))
		     ((looking-at ",")
		      (forward-char)
		      (when (= bc 1)
			(setq cc (+ cc 1))))
		     (t (forward-char))))
	     (if (= cc -1)
		 (if specflag
		     (setq args "/?")
		   (setq args "(?)"))
	       (if specflag
		   (setq args (concat "/" (number-to-string (+ cc 1))))
		 (setq args (concat "(" (make-string cc 44) ")")))))
	    ((looking-at "{")
	     (if specflag
		 (setq args "/?")
	       (setq args "{}")))
	    (t
	     (if specflag
		 (setq args "/0")
	       (setq args ""))))
      ;; return template
      (concat functor args))))

(defun eclipse-get-current-predicate-args ()
  ;; return the arguments for the current term
  ;; this should be improved, so that comments get stripped automatically
  ;; also, this function and eclipse-get-current-predicate-template should
  ;; be rolled into one
  (let ((args '()) fb fe (found nil) (bc 0) next (arg nil))
    ;; we are at the beginning of a clause...
    (save-excursion
      ;; search end of predicate name
      (or (re-search-forward "[ \t\n({:]" (point-max) t)
	  (search-forward "."))
      (eclipse-backward-char)
      ;; if there are any arguments...
      (when (looking-at "(")
	(forward-char)
	(setq bc 1
	      fb (point))
	;; extract the arguments, jumping over comments
	(while (not found)
	  (cond ((eobp) ; end of buffer: finish search
		 (setq found t
		       fe (point)
		       next (buffer-substring-no-properties fb fe)
		       args (append args (list (concat arg next)))
		       arg nil))
		((looking-at "'") ; quoted atom: go to end
		 (eclipse-goto-end-of-quote))
		((looking-at "\"") ; string: go to end
		 (eclipse-goto-end-of-string))
		((looking-at "%") ; comment: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe)
		       arg (concat arg next))
		 (forward-line)
		 (unless (eobp)
		   (beginning-of-line))
		 (eclipse-jump-over-strings nil t)
		 (setq fb (point)))
		((looking-at "/\\*") ; c-style comment: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe)
		       arg (concat arg next))
		 (unless (re-search-forward "\\*/" (point-max) t)
		   (goto-char (point-max)))
		 (eclipse-jump-over-strings nil t)
		 (setq fb (point)))
		((looking-at "[({[]") ; increase bracket counter
		 (forward-char)
		 (setq bc (+ bc 1)))
		((looking-at "[]})]") ; decrease bracket counter
		 (setq bc (- bc 1))
		 (when (zerop bc)
		   (setq found t
			 fe (point)
			 next (buffer-substring-no-properties fb fe)
			 args (append args (list (concat arg next)))
			 arg nil))
		 (forward-char))
		((looking-at ",")
		 (cond ((= bc 1)
			;; next argument found, add it to list
			(setq fe (point)
			      next(buffer-substring-no-properties fb fe)
			      args (append args (list (concat arg next)))
			      arg nil)
			(forward-char)
			(eclipse-jump-over-strings nil t)
			(setq fb (point)))
		       (t (forward-char))))
		((looking-at "[ \t\n]") ; whitespace or new line: strip away
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe)
		       arg (concat arg next))
		 (eclipse-jump-over-strings nil t)
		 (setq fb (point)))
		(t (forward-char)))))
      args)))

(defun eclipse-insert-predicate-template ()
  "Insert the template of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil))
      (save-excursion
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(setq template (eclipse-get-current-predicate-template)))
      (insert template)
      (when (save-excursion (backward-char) (looking-at ")"))
	(search-backward "(")
	(forward-char)))))

(defun eclipse-insert-predicate-spec ()
  "Insert the specification of the current predicate."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil))
      (save-excursion
	(unless (eclipse-check-clause-begin)
	  (eclipse-goto-clause-begin))
	(setq template (eclipse-get-current-predicate-template t)))
      (insert template))))

(defun eclipse-insert-clause-head ()
  "Insert a new clause head of the current predicate with the arguments of the last clause."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil) (this (point)) spec vars functor arity aux next
	  (iscomment nil))
      (save-excursion
	(eclipse-goto-clause-begin)
	(if (looking-at ":-")
	    (setq iscomment t)
	  (setq spec (eclipse-get-current-predicate-template t)
		vars (eclipse-get-current-predicate-args)
		aux (split-string spec "/")
		functor (nth 0 aux)
		arity (unless (string-equal (nth 1 aux) "?")
			(string-to-number (nth 1 aux))))))
      (unless iscomment
	(insert (concat "\n" functor))
	(cond ((string-equal (nth 1 aux) "?")
	       (insert "{} :-\n")
	       (goto-char this)
	       (search-forward "{"))
	      (t
	       (unless (zerop arity)
		 (insert "(")
		 (while (car vars)
		   (setq next (car vars)
			 vars (cdr vars))
		   (insert next)
		   (if (car vars) (insert ",")))
		 (insert ")"))
	       (insert " :-\n")
	       (goto-char this)
	       (re-search-forward "[(:]" (point-max) t)
	       (backward-char)
	       (if (looking-at "(")
		   (forward-char)
		 (forward-char 3)
		 (eclipse-indent-line))))))))
  
(defun eclipse-insert-clause-head-empty ()
  "Insert a new clause head of the current predicate without arguments."
  (interactive)
  (unless eclipse-esp-selected
    (let ((template nil) (this (point)))
      (save-excursion
	(eclipse-goto-clause-begin)
	(setq template (eclipse-get-current-predicate-template)))
      (unless (string-equal "" template)
	(insert (concat "\n" template " :-\n"))
	(goto-char this)
	(re-search-forward "[(:]" (point-max) t)
	(backward-char)
	(if (looking-at "(")
	    (forward-char)
	  (forward-char 3)
	  (eclipse-indent-line))))))
  
(defun eclipse-anonymise-variables ()
  "Add _ to all variables in the current region."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    ;; go to first non-word after rbegin (we don't want to insert a "_" in the middle of a word!)
    (eclipse-anonymise-loop rbegin 0)
    ;; anonymise the rest of the variables until rend is reached
    (eclipse-anonymise-loop rend 1)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymous-variables ()
  "Replaces the variables in the current region with anonymous variables."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    ;; goto first non-word after rbegin (we don't want to replace the end of a word!)
    (eclipse-anonymise-loop rbegin 0)
    ;; replace the rest of the variables until rend is reached
    (eclipse-anonymise-loop rend 2)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymise-loop (pos flag)
  ;; if flag = 0, do nothing
  ;; if flag = 1, anonymise variables
  ;; if flag = 2, replace with anonymous variables ("_")
  ;; loop until first non-word at or after pos is reached
  (while (not (or (> (point) pos) (eobp)))
    (cond ((looking-at "[_A-Z]")
	   (unless (or (looking-at "_") (zerop flag))
	     (insert "_")
	     (setq pos (+ pos 1)))
	   (when (= flag 2)
	     (when (looking-at "[_a-zA-Z0-9]*")
	       (let ((l (- (match-end 0) (match-beginning 0))))
		 (delete-region (match-beginning 0) (match-end 0))
		 (setq pos (- pos l)))))
	   (re-search-forward "[^_a-zA-Z0-9]" (point-max) t)
	   (backward-char))
	  ((looking-at "'")
	   (eclipse-goto-end-of-quote))
	  ((looking-at "\"")
	   (eclipse-goto-end-of-string))
	  ((looking-at "[a-z0-9_]")
	   (re-search-forward "[^_a-zA-Z0-9']" (point-max) t)
	   (backward-char))
	  ((looking-at "%")
	   (forward-line)
	   (unless (eobp)
	     (beginning-of-line)))
	  ((looking-at "/\\*")
	   (unless (re-search-forward "\\*/" (point-max) t)
	     (goto-char (point-max))))
	  (t
	   (forward-char)
	   (if (re-search-forward "[a-zA-Z0-9_'\"%/\\]" (point-max) t)
	       (backward-char)
	     (goto-char pos)
	     (or (eobp) (forward-char)))))))

(defun eclipse-insert-comment-pred-short ()
  "Insert \":- comment(,).\" into the program text."
  (interactive)
  (unless eclipse-esp-selected
    (insert ":- comment(,).\n")
    (forward-line -1)
    (beginning-of-line)
    (search-forward "(")))

(defun eclipse-insert-comment-pred-full ()
  "Insert comment/2 call with all arguments into program text."
  (interactive)
  (unless eclipse-esp-selected
    (let (pnt spec vars functor arity aux next)
      (eclipse-jump-over-strings t)
      (setq spec (eclipse-get-current-predicate-template t))
      (message spec)
      (if (string-equal spec "/0")
	  (eclipse-insert-comment-pred-short)
	(setq vars (eclipse-get-current-predicate-args)
	      aux (split-string spec "/")
	      functor (nth 0 aux)
	      arity (string-to-number (nth 1 aux)))
	(insert (concat ":- comment(" spec  ", [\n"))
	(setq pnt (point))
	(insert "        summary:,\n")
	(unless (zerop arity)
	  (insert (concat "        amode:" functor (concat "(" (make-string (- arity 1) 44) ")") ",\n"
			  "        args:[\n"))
	  (while (car vars)
	    (setq next (car vars)
		  vars (cdr vars))
	    (insert (concat "                 \"" next))
	    (if (car vars)
		(insert "\": ,\n")
	      (insert "\": \n")))
	  (insert "             ],\n"))
	(insert (concat "        desc:,\n"
			"        fail_if:,\n"
			"        resat:,\n"
			"        eg:,\n"
			"        see_also:,\n"
			"        index:]).\n\n"))
	(goto-char pnt)
	(search-forward ":")))))
  
;;
;; dabbrev support for ECLiPSe keywords
;;

(defun eclipse-dabbrev-expand ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion including arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-expand1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion without arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "[a-zA-Z][a-zA-Z_0-9]*(?"))

(defun eclipse-dabbrev-expand2 (reg)
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    ;; a bit of a hack:
    ;; we want our own keyword list searched first,
    ;; then the current buffer etc.
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-dabbrev-expand0 ()
  "Automatic expansion by dabbrev.
Checks for expansions in current buffer first, then for predefined keywords."
  (interactive)
  (let (aux1 aux2)
    (eclipse-load-dabbrev)
    (setq aux1 dabbrev-search-these-buffers-only
	  aux2 dabbrev-abbrev-char-regexp
	  dabbrev-abbrev-char-regexp nil)
    (eclipse-update-dabbrev-list (current-word) t)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp aux2
	  dabbrev-search-these-buffers-only aux1)))

(defun eclipse-dabbrev-completion ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansions including arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-completion1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansions without arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "[a-zA-Z][a-zA-Z_0-9]*(?"))

(defun eclipse-dabbrev-completion2 (reg)
  ;; automatic expansion of ECLiPSe keywords
  ;; returns list of possible expansions
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-completion nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-load-dabbrev ()
  ;; load dabbrev if needed, and load the keyword list
  (require 'dabbrev)
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (make-local-variable 'dabbrev-search-these-buffers-only))

(defun eclipse-update-dabbrev-list (keyword &optional flag)
  ;; get list of keywords from ECLiPSe, via help/1 call
  (get-buffer-create eclipse-keywords-buffer)
  (unless (or (not keyword)
	      (if dabbrev--last-abbrev-location
		  (if (numberp dabbrev--last-abbrev-location)
		      ;; dabbrev--last-abbrev-location can either be number or
		      ;; marker!?
		      (= dabbrev--last-abbrev-location (point))
		    (= (marker-position dabbrev--last-abbrev-location) (point)))))
    (let (help-call)
      ;; call ECLiPSe help for keyword
      (setq help-call (concat eclipse-help-call1 (downcase keyword) eclipse-help-call2))
      ;; parse output
      (save-excursion
	(set-buffer eclipse-keywords-buffer)
	(goto-char (point-min))
	(delete-char (- (point-max) (point)))
	(insert (shell-command-to-string help-call))
	(goto-char (point-min))
	(if (looking-at "string stream")
	    (delete-char (- (point-max) (point-min)))
	  (while (not (eobp))
	    (cond ((looking-at "----")
		   (delete-char 4)
		   (delete-blank-lines)
		   (if (looking-at "Call")
		       (delete-char (- (point-max) (point)))
		     (let ((aux1 (point)) aux2)
		       (end-of-line)
		       (search-backward ":" aux1 t)
		       (when (looking-at ":")
			 (forward-char)
			 (skip-chars-forward " \t")
			 (setq aux1 (point))
			 (beginning-of-line)
			 (delete-char (- aux1 (point))))
		       (cond ((looking-at "lib([a-z_]+)")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     ((looking-at "[a-z]+ [a-z]+/index")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     (t (forward-line)))
		       (beginning-of-line)
		       (setq aux1 (point))
		       (search-forward "----" (eobp) t)
		       (backward-char 4)
		       (setq aux2 (point))
		       (goto-char aux1)
		       (delete-char (- aux2 aux1)))))
		  ((looking-at "[ \t]*\n")
		   (delete-blank-lines))))))))
  ;; update dabbrev buffer list
  (let ((blist (get-all-eclipse-buffers)))
    (if flag
	(setq blist (append (list (buffer-name) eclipse-keywords-buffer)
			    (cdr blist)))
      (setq blist (append (list eclipse-keywords-buffer) blist))
    (setq dabbrev-search-these-buffers-only blist))))

(defun get-all-eclipse-buffers ()
  ;; get list of all ECLiPSe buffers
  (let ((blist (list (buffer-name)))
	(all-buffers (cdr (buffer-list)))
	next next-name ext)
    (while (car all-buffers)
      (setq next (car all-buffers)
	    all-buffers (cdr all-buffers)
	    next-name (buffer-name next)
	    ext (nth 1 (split-string next-name "\\.")))
      (when (member (concat "." ext) eclipse-extensions)
	(setq blist (append blist (list next-name)))))
    blist))

;;
;; Module loading & help functions
;;

(defun eclipse-load-all-modules ()
  "Load all project files specified in all buffers containing ECLiPSe programs,

i.e. any file inside a use_module/1, ensure_loaded/1, compile/1, or include/1
call."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-load-modules t)))

(defun eclipse-load-modules (&optional flag)
  "Load all project files specified in the current buffer,

i.e. any file inside a use_module/1, ensure_loaded/1, compile/1, or include/1
call."
  (interactive)
  (unless eclipse-esp-selected
    (let* ((blist (if flag (buffer-list) ; start with current buffer
		    (list (car (buffer-list))))) ; or all current ECLiPSe buffers
	   (all-buffers blist)
	   name ext begin end filename buffer)
      (save-excursion
	(while (car blist) ; iterate through all buffers
	  (setq name (buffer-name (car blist))
		ext (concat "." (nth 1 (split-string name "\\."))))
	  (when (member ext eclipse-extensions)
	    (set-buffer name)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (cond ((re-search-forward "\\<\\(use_module\\|ensure_loaded\\|compile\\|include\\)('?\\([^')]+\\)'?)"
					(point-max) t)
		     (setq filename (match-string 2))
		     ;; try all combinations...
		     (unless (file-exists-p filename)
		       (let ((fn nil) (exts eclipse-extensions))
			 (while (and exts (not fn))
			   (setq fn (concat filename (car exts))
				 exts (cdr exts))
			   (unless (file-exists-p fn)
			     (setq fn nil)))
			 (setq filename fn)))
		     (unless (not filename)
		       ;; open file (if not yet open)
		       (setq buffer (find-file-noselect filename t))
		       ;; add to list of buffers (if not yet in that list)
		       (unless (member buffer all-buffers)
			 (setq blist (append blist (list buffer))
			       all-buffers (append all-buffers (list buffer))))))
		    (t (goto-char (point-max))))))
	  (setq blist (cdr blist)))))))

(defun eclipse-call-help ()
  "Call the ECLiPSe help.

You will be asked for the predicate name for which you need help.
The input will be passed on to the help/1 predicate.
The output will be presented in the buffer *eclipse-help*.
The format for the help call is Name for simple help and 
<Module:>Name/Arity for detailed help."
  (interactive)
  (eclipse-load-dabbrev)
  (let* ((cw (current-word))
	 (help-call (downcase (or cw "")))
	 aux res)
    (setq aux (read-from-minibuffer 
	       (if help-call
		   (concat "Describe predicate (default " help-call "): ")
		 "Describe predicate: ")))
    (cond ((not help-call) ; no current word: use input from mini-buffer
	   (unless (= 0 (string-width aux))
	     (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2)
		   res (shell-command help-call eclipse-help-buffer))))
	  ((= 0 (string-width aux)) ; no input from mini-buffer: use current word
	   (unless (= 0 (string-width help-call))
	     (setq help-call (concat eclipse-help-call1 help-call eclipse-help-call2)
		   res (shell-command help-call eclipse-help-buffer))))
	  (t ; else use input from mini-buffer
	   (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2)
		 res (shell-command help-call eclipse-help-buffer))))
    (when (eq res 1)
      (message (concat "No help available for \"" aux "\"")))))

;;
;; Highlighting
;;

(defun eclipse-highlight ()
  "Highlight all occurrences of the current word in the current buffer.
Any other highlighting is removed."
  (interactive)
  (let ((cw (current-word)))
    (when (string-match "[a-zA-Z_][a-zA-Z0-9_]+" cw)
      (let ((len (length cw)) ovl beg end)
	(eclipse-dehighlight)
	(unless (string-equal cw eclipse-highlighted) ; the same? just remove
	  (setq eclipse-highlighted cw)
	  ;; highlight all occurrences in current buffer
	  (save-excursion
	    (goto-char (point-min))
	    (while (not (= (point) (point-max)))
	      (when (search-forward cw (point-max) 1)
		(save-excursion
		  (backward-char len)
		  (setq beg (point)
			end (+ beg len)
			ovl (make-overlay beg end))
		  (overlay-put ovl 'face 'eclipse-highlight-face)
		  (eclipse-add-overlay ovl))))))))))

(defun eclipse-add-overlay (ovl)
  ;; add overlay to list of overlays
  (if eclipse-overlays
      (setq eclipse-overlays (append eclipse-overlays (list ovl)))
    (setq eclipse-overlays (list ovl))))

(defun eclipse-dehighlight ()
  "Remove any highlighting from the current buffer."
  (interactive)
  (let (ovl)
    (while eclipse-overlays
      (setq ovl (car eclipse-overlays)
	    eclipse-overlays (cdr eclipse-overlays))
      (delete-overlay ovl))
    (setq eclipse-highlighted nil)))

(defun eclipse-goto-highlight-backward ()
  "Go backwards to previous occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (overlay-recenter (point))
    (let* ((ol (overlay-lists))
	   (list (append (reverse (car ol)) (cdr ol)))
	   last)
      (while (< (overlay-start (car list)) (point))
	(setq last (car list)
	      list (cdr list)))
      (when last
	(goto-char (overlay-start last))))))

(defun eclipse-goto-highlight-forward ()
  "Go forwards to next occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (overlay-recenter (point))
    (let* ((ol (overlay-lists))
	   (list (append (reverse (car ol)) (cdr ol))))
      (while (and list (<= (overlay-start (car list)) (point)))
	(setq list (cdr list)))
      (when list
	(goto-char (overlay-start (car list)))))))

;;
;; Metrics
;;

(defun count-to-end (esp-flag fun &rest args)
  ;; count number of non-empty lines from current point to point
  ;; identified by evaluating function fun with arguments args
  (let* ((start (point))
	 (aux (save-excursion (eval (append (list fun) args))
			      (point)))
	 (end (if (and esp-flag (= aux (point)))
		  (point-max)
		aux))
	 (count 0))
    (while (< (point) end)
      (unless (looking-at "^\\s-*\n")
	(setq count (+ 1 count)))
      (forward-line))
    (unless (or (= end (point)) (and esp-flag (eobp)))
      (forward-line -1))
    count))

(defun count-comments-to-end-esp (fun &rest args)
  ;; count number of comment lines from current point to point
  ;; identified by evaluating function fun with arguments args
  (let ((start (point))
	(end (save-excursion (eval (append (list fun) args))
			     (point)))
	(count 0) aux)
    (while (< (point) end)
      (cond ((looking-at "^\\s-*%")
	     (setq count (+ 1 count)))
	    ((looking-at "/\\*")
	     ;; C-style comment
	     (setq aux (count-to-end nil 're-search-forward "\\*/"
				     (point-max) t)
		   count (+ count aux)))
	    (t t))
      (forward-line))
    (unless (= end (point))
      (forward-line -1))
    count))

(defun compare-metrics-max (n max min)
  (cond ((= min -1) n)
	((> n max) n)
	(t max)))

(defun compare-metrics-min (n min)
  (cond ((= min -1) n)
	((< n min) n)
	(t min)))

(defun float-metrics (n1 n2)
  (if (= n1 0)
      "0.00"
    (concat (number-to-string (truncate (* 1.0 n1) n2)) "."
	    (let ((n (truncate (% (/ (* 100 n1) n2) 100))))
	      (if (= n 0)
		  "00"
		(number-to-string n))))))

(defun eclipse-get-metrics (name esp-mode-flag no-msg-flag)
  ;; computes metrics for current buffer
  (let (metrics (pred_end -1) (clause_end -1) (size (buffer-size))
		(total 0) (comments 0) (pc 0))
    (cond (esp-mode-flag
	   (let ((esp_segm 0) (esp_segm_code 0) (esp_segm_min -1)
		 (esp_segm_max 0) (esp_expr 0) (esp_expr_code 0)
		 (esp_expr_min -1) (esp_expr_max 0) (esp_directives 0)
		 (esp_dir_code 0) (esp_dir_min -1) (esp_dir_max 0)
		 (html 0) aux l_esp_segm l_esp_dir l_esp_expr
		 aux_line_beg aux1)
	     (save-excursion
	       (set-buffer name)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (unless no-msg-flag
		   (setq pc (eclipse-percent-message "Computing metrics"
						     size pc)))
		 (skip-chars-forward " \t\n")
		 (cond ((eobp) t)
		       ((looking-at "<\\?esp")
			(let (esp_code esp_tag)
			  (cond ((looking-at "<\\?esp:compile")
				 (setq esp_tag 2
				       esp_directives (1+ esp_directives)))
				((looking-at "<\\?esp=")
				 (setq esp_tag 1
				       esp_expr (1+ esp_expr)))
				(t
				 (setq esp_tag 0
				       esp_segm (1+ esp_segm))))
			  (setq aux1 (point)
				aux1 (save-excursion
				       (beginning-of-line)
				       (skip-chars-forward " \t")
				       (= aux1 (point)))
				aux_line_beg (line-beginning-position)
				aux (count-to-end t 're-search-forward
						  "\\?>" (point-max) t)
				aux (if aux1 (1- aux) aux)
				total (+ total (if aux1 aux (1- aux)))
				aux1 aux
				esp_code aux)
			  (unless (= aux_line_beg (line-beginning-position))
			    (save-excursion
			      (goto-char aux_line_beg)
			      (setq aux (count-comments-to-end-esp
					 're-search-forward
					 "\\?>" (point-max) t)
				    comments (+ comments aux)
				    esp_code (- esp_code aux)
				    aux (- aux1 aux))))
			  (cond ((= esp_tag 2)
				 (setq esp_dir_max (compare-metrics-max aux esp_dir_max esp_dir_min)
				       esp_dir_min (compare-metrics-min aux esp_dir_min)
				       esp_dir_code (+ esp_dir_code
						       esp_code)))
				((= esp_tag 1)
				 (setq esp_expr_max (compare-metrics-max aux esp_expr_max esp_expr_min)
				       esp_expr_min (compare-metrics-min aux esp_expr_min)
				       esp_expr_code (+ esp_expr_code
							esp_code)))
				((= esp_tag 0)
				 (setq esp_segm_max (compare-metrics-max aux esp_segm_max esp_segm_min)
				       esp_segm_min (compare-metrics-min aux esp_segm_min)
				       esp_segm_code (+ esp_segm_code
							esp_code))))
			  (forward-line)))
		       (t ; HTML code
			(setq aux (count-to-end t 're-search-forward
						"<\\?esp" (point-max) t)
			      total (+ total aux)
			      html (+ html aux))
			(unless (or (looking-at "<\\?esp") (eobp))
			  (re-search-forward "<\\?esp" (point-max) t)
			  (backward-char 5))))))
	     (setq l_esp_segm (float-metrics esp_segm_code esp_segm)
		   l_esp_dir (float-metrics esp_dir_code esp_directives)
		   l_esp_expr (float-metrics esp_expr_code esp_expr)
		   metrics (list esp-mode-flag esp_segm esp_segm_code
				 l_esp_segm (max 0 esp_segm_min)
				 esp_segm_max esp_directives esp_dir_code
				 l_esp_dir (max 0 esp_dir_min)
				 esp_dir_max esp_expr esp_expr_code
				 l_esp_expr (max 0 esp_expr_min)
				 esp_expr_max total html comments size))))
	  (t
	   (let ((predicates 0) (clauses 0) (cl_pr 0) (cl_pr_min -1)
		 (cl_pr_max 0) (code 0) (directives 0)
		 (l_pr 0) (l_pr_min -1) (l_pr_max 0) (l_cl 0) (l_cl_min -1)
		 (l_cl_max 0) cl_pr_aux l_pr_aux l_cl_aux aux (n_clauses 0)
		 (n_code_pr 0) (n_code_cl 0))
	     (save-excursion
	       (set-buffer name)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (unless no-msg-flag
		   (setq pc (eclipse-percent-message "Computing metrics"
						     size pc)))
		 (skip-chars-forward " \t\n")
		 (cond ((looking-at ":-[ \t]*comment(")
			;; comment/2 directive: counted as comment
			(setq aux (count-to-end nil 'eclipse-goto-clause-end)
			      total (+ total aux)
			      comments (+ comments aux)))
		       ((looking-at ":-")
			;; directive
			(setq aux (count-to-end nil 'eclipse-goto-clause-end)
			      total (+ total aux)
			      directives (+ directives aux)))
		       ((looking-at "%")
			;; comment
			(setq comments (1+ comments)
			      total (1+ total)))
		       ((looking-at "/\\*")
			;; C-style comment
			(setq aux (count-to-end nil 're-search-forward "\\*/" (point-max) t)
			      total (+ total aux)
			      comments (+ comments aux)))
		       ((or (looking-at "\n") (eobp))
			;; empty line
			t)
		       (t ;; normal line
			(cond ((> (point) pred_end) ; next predicate
			       (unless (= predicates 0)
				 ;; update clauses per predicate (min/max)
				 ;; for previous predicate
				 (setq cl_pr_max (compare-metrics-max n_clauses cl_pr_max cl_pr_min)
				       cl_pr_min (compare-metrics-min n_clauses cl_pr_min)
				       ;; update lines per predicate (min/max)
				       ;; for previous predicate
				       l_pr_max (compare-metrics-max n_code_pr l_pr_max l_pr_min)
				       l_pr_min (compare-metrics-min n_code_pr l_pr_min)
				       ;; update lines per clause (min/max)
				       ;; for previous clause
				       l_cl_max (compare-metrics-max n_code_cl l_cl_max l_cl_min)
				       l_cl_min (compare-metrics-min n_code_cl l_cl_min)))
			       ;; increase predicate and clause counter
			       (setq predicates (1+ predicates)
				     clauses (1+ clauses)
				     n_clauses 1
				     n_code_pr 0
				     n_code_cl 0
				     pred_end (save-excursion
						(eclipse-goto-predicate-end)
						(point))
				     clause_end (save-excursion
						  (eclipse-goto-clause-end)
						  (point))))
			      ((> (point) clause_end) ; next clause
			       ;; update lines per clause (min/max)
			       ;; for previous clause
			       (setq l_cl_max (compare-metrics-max n_code_cl l_cl_max l_cl_min)
				     l_cl_min (compare-metrics-min n_code_cl l_cl_min)
				     ;; increase predicate and clause counter
				     clauses (1+ clauses)
				     n_clauses (1+ n_clauses)
				     n_code_cl 0
				     clause_end (save-excursion
						  (eclipse-goto-clause-end)
						  (point))))
			      (t t))
			(setq total (1+ total)
			      n_code_cl (1+ n_code_cl)
			      n_code_pr (1+ n_code_pr)
			      code (1+ code))))
		 (forward-line)))
	     ;; update clauses per predicate (min/max)
	     ;; for previous predicate
	     (setq cl_pr_max (compare-metrics-max n_clauses cl_pr_max cl_pr_min)
		   cl_pr_min (compare-metrics-min n_clauses cl_pr_min)
		   ;; update lines per predicate (min/max)
		   ;; for previous predicate
		   l_pr_max (compare-metrics-max n_code_pr l_pr_max l_pr_min)
		   l_pr_min (compare-metrics-min n_code_pr l_pr_min)
		   ;; update lines per clause (min/max)
		   ;; for previous clause
		   l_cl_max (compare-metrics-max n_code_cl l_cl_max l_cl_min)
		   l_cl_min (compare-metrics-min n_code_cl l_cl_min)
		   cl_pr (float-metrics clauses predicates)
		   l_pr (float-metrics code predicates)
		   l_cl (float-metrics code clauses)
		   metrics (list esp-mode-flag predicates clauses cl_pr
				 (max 0 cl_pr_min)
				 cl_pr_max total code comments directives l_pr
				 (max 0 l_pr_min)
				 l_pr_max l_cl
				 (max 0 l_cl_min)
				 l_cl_max size)))))
  metrics))

(defun eclipse-display-metrics (&optional flag)
  "This function computes the following metrics for the current buffer:

1. number of predicates
2. number of clauses
3. average clauses per predicate (min/max)
4. total lines, excluding empty lines
5. lines of code
6. lines of comments
7. lines of directives
8. average lines per predicate (min/max)
9. average lines per clause (min/max)
10. size

comment/2 directives are counted as comments.

Note that the line count function only looks at the beginning of a line.
The following code:

    :- comment(summary, \"Line count example\").
               % comment with leading white space

    example :-                     % This comment is not counted
        writeln(\"Hello world!\"),  /* This C-style comment
    shows that you have to be careful to avoid comment lines
    that are difficult to recognize!*/
        writeln(\"Hello again!\"),
    /* This C-style comment is counted as a comment */
        writeln(\"Goodbye!\").

gives this line count:

    Line count for example.ecl:
    -----------------------
    code lines: 6
    comments  : 3

The reason is that two lines of the first C-style comment are counted as
code lines, since the start of the comment is not at the beginning of a line.


If the current buffer is a ECLiPSe Server Pages buffer (.esp), the
following metrics are computed:

1. number of ESP segments (<?esp ... ?>)
2. number of ESP directives (<?esp:compile ... ?>)
3. number of ESP expressions (<?esp= ... ?>)
4. total lines, excluding empty lines
5. lines of ESP segment
6. average lines per segment (min/max)
7. lines of ESP directives
8. average lines per directive (min/max)
9. lines of ESP expression
10. average lines per expression (min/max)
11. lines of HTML code
12. lines of comments
13. size
"
  (interactive)
  (let* ((name (buffer-name)) metrics result-window line res)
    (unless flag
      (message "Computing metrics..."))
    (setq metrics (eclipse-get-metrics name eclipse-esp-selected flag))
    (unless flag
      (message "Computing metrics...done")
      (get-buffer-create eclipse-stats-buffer))
    (set-buffer eclipse-stats-buffer)
    (unless flag
      (delete-region (point-min) (point-max)))
    (goto-char (point-max))
    (setq line (concat name ":"))
    (insert-metrics line metrics)
    (unless flag
      (display-buffer eclipse-stats-buffer)
      (goto-char (point-min)))
    metrics))

(defun insert-metrics (line metrics)
  (let* ((esp-mode-flag (car metrics))
	 (metrics (cdr metrics)))
    (cond (esp-mode-flag
	   (insert (concat line "\n"
			   (make-string (string-width line) 45) "\n"
			   "ESP segments       : "
			   (number-to-string (nth 0 metrics)) "\n"
			   "ESP directives     : "
			   (number-to-string (nth 5 metrics)) "\n"
			   "ESP expressions    : "
			   (number-to-string (nth 10 metrics)) "\n"
			   "Total lines        : "
			   (number-to-string (nth 15 metrics)) "\n"
			   "ESP segm. lines    : "
			   (number-to-string (nth 1 metrics)) "\n"
			   "Lines per segment  : " (nth 2 metrics) "\t(min:"
			   (number-to-string (nth 3 metrics)) "/max:"
			   (number-to-string (nth 4 metrics)) ")\n"
			   "ESP direct. lines  : "
			   (number-to-string (nth 6 metrics)) "\n"
			   "Lines per directive: " (nth 7 metrics) "\t(min:"
			   (number-to-string (nth 8 metrics)) "/max:"
			   (number-to-string (nth 9 metrics)) ")\n"
			   "ESP expr. lines    : "
			   (number-to-string (nth 11 metrics)) "\n"
			   "Lines per expr.    : " (nth 12 metrics) "\t(min:"
			   (number-to-string (nth 13 metrics)) "/max:"
			   (number-to-string (nth 14 metrics)) ")\n"
			   "HTML lines         : "
			   (number-to-string (nth 16 metrics)) "\n"
			   "Comment lines      : "
			   (number-to-string (nth 17 metrics)) "\n"
			   "Size               : "
			   (number-to-string (nth 18 metrics)) "\n")))
	  (t
	   (insert (concat line "\n"
			   (make-string (string-width line) 45) "\n"
			   "Predicates         : "
			   (number-to-string (nth 0 metrics)) "\n"
			   "Clauses            : "
			   (number-to-string (nth 1 metrics)) "\n"
			   "Clauses/predicate  : " (nth 2 metrics) "\t(min:"
			   (number-to-string (nth 3 metrics)) "/max:"
			   (number-to-string (nth 4 metrics)) ")\n"
			   "Total lines        : "
			   (number-to-string (nth 5 metrics)) "\n"
			   "Lines of code      : "
			   (number-to-string (nth 6 metrics)) "\n"
			   "Comment lines      : "
			   (number-to-string (nth 7 metrics)) "\n"
			   "Directives lines   : "
			   (number-to-string (nth 8 metrics)) "\n"
			   "Lines per predicate: " (nth 9 metrics) "\t(min:" 
			   (number-to-string (nth 10 metrics)) "/max:"
			   (number-to-string (nth 11 metrics)) ")\n"
			   "Lines per clause   : " (nth 12 metrics) "\t(min:" 
			   (number-to-string (nth 13 metrics)) "/max:" 
			   (number-to-string (nth 14 metrics)) ")\n"
			   "Size               : "
			   (number-to-string (nth 15 metrics)) "\n"))))))

(defun all-metrics-min (min new)
  (cond ((= min -1) new)
	((< new min) new)
	(t min)))

(defun all-metrics-max (max new)
  (if (> new max) new max))

(defun eclipse-display-metrics-all ()
  "This function displays the metrics for all ECLiPSe buffers.

See `eclipse-display-metrics' for more details."
  (interactive)
  (let* ((blist (get-all-eclipse-buffers))
	 next line metrics all-ecl-metrics all-esp-metrics
	 (predicates 0) (clauses 0) (cl_pr 0) (cl_pr_min -1) (cl_pr_max 0)
	 (total-ecl 0) (code 0) (comments-ecl 0) (directives 0) (l_pr 0)
	 (l_pr_min -1) (l_pr_max 0) (l_cl 0) (l_cl_min -1) (l_cl_max 0)
	 (size-ecl 0) (esp_segm 0) (esp_segm_code 0) (esp_segm_min -1)
	 (esp_segm_max 0) (l_esp_segm 0) (esp_directives 0) (esp_dir_code 0)
	 (esp_dir_min -1) (esp_dir_max 0) (l_esp_dir 0) (esp_expr 0)
	 (esp_expr_code 0) (esp_expr_min -1) (esp_expr_max 0) (l_esp_expr 0)
	 (html 0) (total-esp 0) (comments-esp 0) (size-esp 0)
	 (ecl-buffers nil) (esp-buffers nil) (pc 0) (nb 0) (ab (length blist)))
    (get-buffer-create eclipse-stats-buffer)
    (set-buffer eclipse-stats-buffer)
    (delete-region (point-min) (point-max))
    (message "Computing metrics...")
    (while (car blist)
      (setq next (car blist)
	    blist (cdr blist))
      (save-excursion
	(set-buffer next)
	(if eclipse-esp-selected
	    (setq esp-buffers t)
	  (setq ecl-buffers t))
	(setq metrics (append metrics (list (eclipse-display-metrics t)))
	      nb (1+ nb)
	      pc (eclipse-percent-message "Computing metrics" ab pc nb)))
      (goto-char (point-max))
      (insert "\n"))
    (message "Computing metrics...done")
    (while (car metrics)
      (let ((esp-mode-flag (caar metrics)) (metr (cdar metrics)))
	(setq metrics (cdr metrics))
	(cond (esp-mode-flag
	       (setq esp_segm (+ esp_segm (nth 0 metr))
		     esp_segm_code (+ esp_segm_code (nth 1 metr))
		     l_esp_segm (float-metrics esp_segm_code esp_segm)
		     esp_segm_min (all-metrics-min esp_segm_min (nth 3 metr))
		     esp_segm_max (all-metrics-max esp_segm_max (nth 4 metr))
		     esp_directives (+ esp_directives (nth 5 metr))
		     esp_dir_code (+ esp_dir_code (nth 6 metr))
		     l_esp_dir (float-metrics esp_dir_code esp_directives)
		     esp_dir_min (all-metrics-min esp_dir_min (nth 8 metr))
		     esp_dir_max (all-metrics-max esp_dir_max (nth 9 metr))
		     esp_expr (+ esp_expr (nth 10 metr))
		     esp_expr_code (+ esp_expr_code (nth 11 metr))
		     l_esp_expr (float-metrics esp_expr_code esp_expr)
		     esp_expr_min (all-metrics-min esp_expr_min (nth 13 metr))
		     esp_expr_max (all-metrics-max esp_expr_max (nth 14 metr))
		     total-esp (+ total-esp (nth 15 metr))
		     html (+ html (nth 16 metr))
		     comments-esp (+ comments-esp (nth 17 metr))
		     size-esp (+ size-esp (nth 18 metr))))
	      (t
	       (setq predicates (+ predicates (nth 0 metr))
		     clauses (+ clauses (nth 1 metr))
		     cl_pr (float-metrics clauses predicates)
		     cl_pr_min (all-metrics-min cl_pr_min (nth 3 metr))
		     cl_pr_max (all-metrics-max cl_pr_max (nth 4 metr))
		     total-ecl (+ total-ecl (nth 5 metr))
		     code (+ code (nth 6 metr))
		     comments-ecl (+ comments-ecl (nth 7 metr))
		     directives (+ directives (nth 8 metr))
		     l_pr (float-metrics code predicates)
		     l_pr_min (all-metrics-min l_pr_min (nth 10 metr))
		     l_pr_max (all-metrics-max l_pr_max (nth 11 metr))
		     l_cl (float-metrics code clauses)
		     l_cl_min (all-metrics-min l_cl_min (nth 13 metr))
		     l_cl_max (all-metrics-max l_cl_max (nth 14 metr))
		     size-ecl (+ size-ecl (nth 15 metr)))))))
    (setq all-ecl-metrics (list nil predicates clauses cl_pr
				(max 0 cl_pr_min) cl_pr_max total-ecl code
				comments-ecl directives l_pr
				(max 0 l_pr_min) l_pr_max l_cl
				(max 0 l_cl_min) l_cl_max size-ecl)
	  all-esp-metrics (list t esp_segm esp_segm_code
				l_esp_segm (max 0 esp_segm_min)
				esp_segm_max esp_directives esp_dir_code
				l_esp_dir (max 0 esp_dir_min)
				esp_dir_max esp_expr esp_expr_code
				l_esp_expr (max 0 esp_expr_min)
				esp_expr_max total-esp html comments-esp
				size-esp))
    (when ecl-buffers
      (insert-metrics "All ECL buffers:" all-ecl-metrics)
      (when esp-buffers (insert "\n")))
    (when esp-buffers
      (insert-metrics "All ESP buffers:" all-esp-metrics))
    (display-buffer eclipse-stats-buffer)
    (goto-char (point-min))))

;;
;; Inferior eclipse mode
;;

(defvar inferior-eclipse-mode-map nil)

(defun eclipse-inferior-mode-commands (map)
  "Contains the key-bindings for the major ECLiPSe mode.
The following commands are available:

\\{inferior-eclipse-mode-map}"
  (define-key map "\r" 'eclipse-next-line)
  (define-key map "\C-c\C-e" 'run-eclipse)
  (define-key map "\C-c\C-q" 'stop-eclipse)
  (define-key map "\C-c\C-k" 'kill-eclipse)
  (define-key map "\C-c\C-t" 'eclipse-start-tools)
  (define-key map "\C-c\C-s" 'eclipse-source-tracing-toggle)
  (define-key map "\C-cx" 'eclipse-toggle-source-breakpoint)
  (define-key map "\C-c\C-h" 'eclipse-call-help)
  (define-key map "\t" 'comint-dynamic-complete))

(defun inferior-eclipse-mode ()
  "Major mode for interacting with an inferior ECLiPSe process.

The following commands are available:

\\{inferior-eclipse-mode-map}

Note that some key bindings of the comint mode map are overwritten in order to preserve consistency
with the key bindings of the ECLiPSe major mode.

Entry to this mode calls the value of `eclipse-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`eclipse-mode-hook' is called after `comint-mode-hook'.

Commands:
Return at end of buffer sends line as input.

\\[run-eclipse] opens inferior process buffer (if not already open) and starts ECLiPSe.

\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[stop-eclipse] or \\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-quit-subjob] sends quit signal. \\[kill-eclipse] sends quit signal and closes the process
buffer.

\\[eclipse-start-tools] starts the TkTools program.

\\[eclipse-source-tracing-toggle] toggles source tracing on/off. If source tracing is on, breakpoints can be
set using \\[eclipse-toggle-source-breakpoint]. Source tracing only works with ECLiPSe 6.0 or later.

You can send text to the inferior ECLiPSe process from other buffers using
the commands \\[eclipse-compile-buffer] and \\[eclipse-compile-region].
\\[eclipse-compile-region-and-go] sends the region to the inferior process and switches to the process
buffer. Use \\[eclipse-run-region] to send text as ECLiPSe commands.

If there is a problem with entering commands in the inferior ECLiPSe process
window, disable the line
     (define-key map \"\\r\" 'eclipse-next-line)
in the definition of function `eclipse-inferior-mode-commands' in the ECLiPSe mode file
eclipse.el."
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq major-mode 'inferior-eclipse-mode
	mode-name "Inferior Eclipse"
	comint-prompt-regexp "\\[eclipse [1-9][0-9]*\\]: ")
  (eclipse-mode-variables)
  (unless inferior-eclipse-mode-map
    (setq inferior-eclipse-mode-map (copy-keymap comint-mode-map))
    (eclipse-inferior-mode-commands inferior-eclipse-mode-map))
  ;; define menu for inferior mode
  (easy-menu-define
   inferior-eclipse-process-menu inferior-eclipse-mode-map
   "ECLiPSe menu for inferior ECLiPSe process"
   '("ECLiPSe"
     ["Run ECLiPSe" run-eclipse t]
     ["Stop ECLiPSe" stop-eclipse t]
     ["Kill ECLiPSe" kill-eclipse t]
     "--"
     ["Source tracing on/off" eclipse-source-tracing-toggle
      :active (>= eclipse-version 6.0)
      :style toggle
      :selected eclipse-source-tracing]
     "--"
     ["Start TkTools" eclipse-start-tools t]))
  (easy-menu-add inferior-eclipse-process-menu)
  (use-local-map inferior-eclipse-mode-map)
  (run-hooks 'eclipse-mode-hook))

;;;###autoload
(defun run-eclipse ()
  "Run an inferior ECLiPSe process, input and output via buffer *eclipse*."
  (interactive)
  (require 'comint)
  (let (eclipse-window)
    (setq eclipse-source-tracing-counter 0)
    (save-excursion
      (setq eclipse-window (get-buffer-window eclipse-process-buffer))
      (cond ((not eclipse-window)
	     (split-window)
	     (select-window (next-window)))
	    (t (select-window eclipse-window)))
      (add-hook 'comint-preoutput-filter-functions 'eclipse-safe-output)
      (unless (> eclipse-version 0.0)
	(add-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
      (switch-to-buffer (make-comint eclipse-process-name eclipse-program-call))
      (inferior-eclipse-mode))))

(defun eclipse-safe-output (output)
  "Make sure that output is always added at the end of the buffer"
  (goto-char (point-max))
  output)

(defun eclipse-get-version (output)
  "Extract the version number of ECLiPSe."
  (when (string-match "#" output)
    (setq eclipse-version (string-to-number (nth 0 (split-string (nth 1 (split-string output "\\(Version \\)")) " #"))))
    (remove-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
  output)

(defun stop-eclipse ()
  "Send C-c to an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status)
    (save-excursion
      (setq eclipse-window (get-buffer-window eclipse-process-buffer)
	    eclipse-status (process-status eclipse-process-name))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (t
	     (process-send-string eclipse-process-name "\C-c")
	     (if (not eclipse-window)
		 (switch-to-buffer eclipse-process-buffer)
	       (select-window eclipse-window)))))))

(defun kill-eclipse ()
  "Kill an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status)
    (save-excursion
      (setq eclipse-window (get-buffer-window eclipse-process-buffer)
	    eclipse-status (process-status eclipse-process-name))
      (when eclipse-status
	(process-send-string eclipse-process-name eclipse-halt-string))
      (mapcar '(lambda (buf)
		 (when (bufferp (get-buffer buf))
		   (kill-buffer buf)))
	      (list eclipse-process-buffer eclipse-tktools-buffer))
      (unless (not eclipse-window)
	(delete-window eclipse-window)))))

(defun eclipse-start-tools ()
  "Start TkTools for an inferior ECLiPSe process in an external Tcl/Tk window.
The socket number shown in the ECLiPSe process buffer has to be entered
manually into the input field \"Port\" in the external TkTools window."
  (interactive)
  (let (eclipse-window eclipse-status tools-status tools-buffer)
    (save-excursion
      (setq eclipse-status (process-status eclipse-process-name)
	    tools-buffer (get-buffer eclipse-tktools-buffer)
	    tools-status (process-status eclipse-tktools-name))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (tools-status
	     (beep)
	     (message "TkTools already running"))
	    (t
	     ;; close buffer if open from last time tools were started
	     (when tools-buffer
	       (kill-buffer tools-buffer))
	     (setq eclipse-window (get-buffer-window eclipse-process-buffer))
	     (if eclipse-window
		 (select-window eclipse-window)
	       (switch-to-buffer eclipse-process-buffer))
	     (cond ((< eclipse-version 5.4)
		    ;; if version < 5.4 there's no automatic start-up
		    (start-process eclipse-tktools-name eclipse-tktools-buffer eclipse-tktools-call)
		    (insert eclipse-53-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name eclipse-53-tktools-call)
		    (message "Enter socket number in TkTools input field \"Port\" and press \"OK\""))
		   (t
		    ;; add to preoutput filters
		    (add-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
		    (insert eclipse-54-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string eclipse-process-name eclipse-54-tktools-call))))))))

(defun eclipse-run-tktools (output)
  ;; Extracts Host and Port parameters from output, starts TkTools
  ;; with host and port parameters.
  ;; This command needs ECLiPSe 5.4 or later.
  (let (host port output-list list head tail)
    (cond ((string-match "\\[.*, .*\\]\n" output)
	   (setq list (split-string output "\\(\\[\\|\\]\\|, \\)")
		 host (nth 1 list)
		 port (nth 2 list))
	   (start-process eclipse-tktools-name eclipse-tktools-buffer eclipse-tktools-call "-h" host "-p" port)
	   ;; TkTools started, so remove this function from preoutput filters
	   (remove-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
	   "")
	  (t output))))

(defun eclipse-run-region (compile beg end)
  "Send the region to an inferior ECLiPSe process."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (let (eclipse-window)
      (save-excursion
	(process-send-region eclipse-process-name beg end)
	(setq eclipse-window (get-buffer-window eclipse-process-buffer))
	(if eclipse-window
	    (select-window eclipse-window)
	  (switch-to-buffer eclipse-process-buffer))))))

(defun eclipse-remove-empty-lines (output)
  "Remove whitespace from beginning of output lines."
  (let ((str (replace-regexp-in-string "\\(^[  ]+\\)" "" output)))
    (when (string-match "\\[eclipse [0-9]+\\]:" str)
      (remove-hook 'comint-preoutput-filter-functions 'eclipse-remove-empty-lines))
    str))

(defun eclipse-compile-buffer ()
  "Send the entire buffer to an inferior ECLiPSe process and compile it."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (add-hook 'comint-preoutput-filter-functions 'eclipse-remove-empty-lines)
      (process-send-string eclipse-process-name 
			   (concat eclipse-compile-string (buffer-string) "\n"))
      (if eclipse-eof-string
	  (process-send-string eclipse-process-name eclipse-eof-string)
	(process-send-eof eclipse-process-name))))) ;Send eof to eclipse process.

(defun eclipse-compile-region (compile beg end)
  "Send the region to an inferior ECLiPSe process and compile it."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (save-excursion
      (add-hook 'comint-preoutput-filter-functions 'eclipse-remove-empty-lines)
      (process-send-string eclipse-process-name eclipse-compile-string)
      (process-send-region eclipse-process-name beg end)
      (process-send-string eclipse-process-name "\n") ;May be unnecessary
      (if eclipse-eof-string
	  (process-send-string eclipse-process-name eclipse-eof-string)
	(process-send-eof eclipse-process-name))))) ;Send eof to eclipse process.

(defun eclipse-compile-region-and-go (compile beg end)
  "Send the region to an inferior ECLiPSe process, compile it, and switch to
*eclipse* buffer."
  (interactive "P\nr")
  (unless eclipse-esp-selected
    (eclipse-compile-region compile beg end)
    (let (eclipse-window)
      (setq eclipse-window (get-buffer-window eclipse-process-buffer))
      (if eclipse-window
	  (select-window eclipse-window)
	(switch-to-buffer eclipse-process-buffer)))))

(defvar eclipse-trace-overlay nil) ; trace point overlay

(defun eclipse-source-tracing-toggle()
  "Toggle source tracing on/off.

Source tracing only works with ECLiPSe 6.0 or higher."
  (interactive)
  (when (>= eclipse-version 6.0)
    (setq eclipse-source-tracing (not eclipse-source-tracing))
    (cond (eclipse-source-tracing
	   (unless eclipse-trace-overlay
	     (eclipse-make-trace-overlay))
	   (add-hook 'comint-preoutput-filter-functions 'eclipse-print-source-info-filter)
	   (add-hook 'comint-preoutput-filter-functions 'eclipse-print-bp-info-filter))
	  (t
	   (eclipse-delete-trace-overlay)
           (remove-hook 'comint-preoutput-filter-functions 'eclipse-print-source-info-filter)
	   (remove-hook 'comint-preoutput-filter-functions 'eclipse-print-bp-info-filter)))))

(defun eclipse-make-trace-overlay ()
  ;; make tracer overlay
  (let ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'eclipse-tracepoint-face)
    (setq eclipse-trace-overlay ov)))

(defun eclipse-delete-trace-overlay ()
  ; delete tracer overlay
  (delete-overlay eclipse-trace-overlay)
  (setq eclipse-trace-overlay nil))

(defun eclipse-print-source-info-filter (output)
  ;; when tracer is at CALL or EXIT port, a command is sent to get source info
  (cond ((> eclipse-source-tracing-counter 0)
	 (eclipse-process-source-info output))
	((and (>= (length output) 3)
	      (equal (substring output (- (length output) 3)) "%> ")
	      (string-match "\\(CALL\\|NEXT\\|EXIT\\|FAIL\\|LEAVE\\)" output))
	 (let ((beg (point)))
	   (insert output)
	   (let ((over (make-overlay beg (point) nil nil t)))
	     (overlay-put over 'face 'default)))
	 (if (string-match "\\(FAIL\\|LEAVE\\)" output)
	     (overlay-put eclipse-trace-overlay 'face 'eclipse-tracepoint-fail-face)
	   (overlay-put eclipse-trace-overlay 'face 'eclipse-tracepoint-face))
	 (save-excursion
	   (set-buffer eclipse-process-buffer)
	   (goto-char (point-max))
	   (eclipse-set-process-mark))
	 (process-send-string eclipse-process-name "=")
	 (setq eclipse-source-tracing-counter 2)
	 "")
	((and (= eclipse-source-tracing-counter -1)
	      (string-match " *%> " output 0))
	 (setq eclipse-source-tracing-counter 0)
	 "")
	(t
         (setq eclipse-source-tracing-counter 0)
	 output)))

(defvar eclipse-source-trace-file nil)
;; name of currently traced file
(defvar eclipse-source-trace-pos 0)
;; line number of current trace position
(defvar eclipse-bp-file nil)
;; name of currently traced file
(defvar eclipse-bp-pos 0)
;; line number of current trace position
(defvar eclipse-source-trace-bp-no-swap nil)
;; flag that indicates whether we want to swap the window or not in function eclipse-process-source-info
(defvar eclipse-dbgflag nil)
;; flag that indicates whether breakoint was set from a running debugger session

(defun eclipse-process-source-info (output)
  ;; source info is traced to extract file name and position
  ;; the file is loaded if necessary and shown in other window
  ;; the cursor is positioned at the beginning of the relevant line
  (let ((list (split-string output "\n")) str srclist file pos result)
    (while (and (> eclipse-source-tracing-counter 0)
		(> (length list) 0))
      (setq str (car list)
	    list (cdr list))
      (cond ((= eclipse-source-tracing-counter 2)
	     (cond ((string-match "Source position:" str)
		    ;; extract source position
		    (setq str (car list)
			  list (cdr list))
		    (message str)
                    (setq srclist (split-string str ":")
                          file (nth 0 srclist)
			  eclipse-source-trace-file file
                          pos (string-to-number (nth 1 srclist))
			  eclipse-source-trace-pos pos)
		    (if eclipse-source-trace-bp-no-swap
			;; we don't need to swap to other window when breakpoint
			;; is set while debugger is already running
			(find-file file)
		      ;; otherwise, we need to swap to other window
		      (find-file-other-window file))
		    (unless (string-equal mode-name "Eclipse")
		      (eclipse-mode)) ;; just in case it doesn't have a registered extension
                    (goto-line pos)
                    (move-overlay eclipse-trace-overlay (point) (1+ (point))
                                  (get-buffer (buffer-name)))
		    (switch-to-buffer-other-window (get-buffer eclipse-process-buffer))
                    (setq eclipse-source-tracing-counter 1))
		   (t
		    (message "No source information.")
		    (setq eclipse-source-trace-file nil
			  eclipse-source-trace-pos 0)
		    (when eclipse-trace-overlay
		      (move-overlay eclipse-trace-overlay (point-min) (point-min)
				    (get-buffer (buffer-name))))
		    (setq eclipse-source-tracing-counter -1)))
	     (setq result ""))
	    ((= eclipse-source-tracing-counter 1)
	     (setq eclipse-source-tracing-counter -1)
	     (if (string-match "%>" str)
		 (setq result "")
	       (setq result str)))))
    result))

;; define function line-number-at-pos, if not vailable (i.e., Emacs =< 21)
(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS. If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
       (save-excursion
         (goto-char (point-min))
         (setq start (point))
         (goto-char opoint)
         (forward-line 0)
         (1+ (count-lines start (point)))))))

(defun eclipse-print-bp-info-filter (output)
  ;; filter function to capture added breakpoints
  (cond ((not eclipse-bp-flag)
	 ;; breakpoints can't be added: do nothing
	 output)
	((string-match "breakpoint added" output 0)
	 ;; breakpoint added
	 (let* ((list (split-string output "\n")) (str (nth 0 list)) beg end line file next)
	   ;; extract file/line information
	   (setq list (split-string str " ")
		 line (string-to-number (nth 4 list))
		 file (nth 7 list))
	   ;; add overlay
	   (find-file file)
	   (save-excursion
	     (goto-line line)
	     (setq beg (line-beginning-position))
	     (setq end (line-end-position)))
	   (setq next (make-overlay beg end))
	   (overlay-put next 'face 'eclipse-breakpoint-face)
	   ;; switch to ECLiPSe process buffer to insert output
	   (set-buffer (get-buffer eclipse-process-buffer)))
	 output)
	(t
	 ;; else: do nothing
	 output)))

(defun eclipse-toggle-source-bp1 (&optional output)
  ;; function that sends command to set/unset breakpoints.
  ;; is used as a filter function when called when a debugger session is running
  (when (or (not eclipse-dbgflag)
	    (string-match "\\[eclipse [0-9]+\\]:" output))
    ;; goto position when called from a debugger session
    (when (and eclipse-dbgflag
	       eclipse-source-trace-file)
      (find-file-other-window eclipse-bp-file)
      (goto-line eclipse-bp-pos))
    (let* ((line (line-number-at-pos))
	   (start (save-excursion (beginning-of-line) (point)))
	   (ovls (overlays-at start))
	   (bp-set t)
	   next face str)
      ;; check if breakpoint is set at this position
      (while (and ovls bp-set)
	(setq next (car ovls)
	      ovls (cdr ovls)
	      face (overlay-get next 'face))
	(when (eq face 'eclipse-breakpoint-face)
	  (setq bp-set nil)))
      (setq eclipse-bp-flag bp-set)
      (message (buffer-file-name))
      ;; set/unset breakpoint
      (unless bp-set
	(setq str "no"))
      (setq str (concat str "spy('" (buffer-file-name) "':" (number-to-string line) ").\n"))
      (process-send-string eclipse-process-name str))
    (when eclipse-dbgflag
      ;; when called from debugger session, change filter functions
      (remove-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp1)
      (add-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp2)
      (setq eclipse-dbgflag nil)))
  output)

(defun eclipse-toggle-source-bp2 (output)
  ;; filter function that sends signal to end break level status
  (when (string-match "\\[eclipse [0-9]+\\]:" output)
    (save-excursion
      (set-buffer eclipse-process-buffer)
      (goto-char (point-max))
      (eclipse-set-process-mark))
    ;; leave break level (and don't swap windows!)
    (setq eclipse-source-trace-bp-no-swap t)
    ;; exchange filter functions
    (remove-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp2)
    (add-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp3)
    ;; leave break level
    (process-send-string eclipse-process-name "end_of_file.\n"))
  output)

(defun eclipse-toggle-source-bp3 (output)
  ;; filter function to clean up after breakpoint was set
  (when (string-match ".*%>" output)
    (setq eclipse-source-trace-bp-no-swap nil)
    (save-excursion
      (set-buffer eclipse-process-buffer)
      (goto-char (point-max))
      (eclipse-set-process-mark))
    (remove-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp3))
  output)

(defun eclipse-toggle-source-breakpoint ()
  "Toggle breakpoint at position. Requires that file is compiled in a corresponding ECLiPSe session."
  (interactive)
  (when eclipse-source-tracing
    (cond ((equal (buffer-name) eclipse-process-buffer)
	   ;; when we're in an ECLiPSe process buffer...
	   (when eclipse-source-trace-file
	     ;; .. and source file is already loaded
	     (eclipse-set-process-mark)
	     ;; load relevant file in other window
	     (find-file-other-window eclipse-source-trace-file)
	     (goto-line eclipse-source-trace-pos)
	     (setq eclipse-bp-file eclipse-source-trace-file
		   eclipse-bp-pos eclipse-source-trace-pos)))
	  (t
	   (setq eclipse-bp-file (buffer-file-name)
		 eclipse-bp-pos (line-number-at-pos))))
    (cond ((and (equal (buffer-name) eclipse-process-buffer)
		(save-excursion
		  (forward-line 0)
		  (looking-at "\\[eclipse [0-9]+\\]:")))
	   ;; when in ECLiPSe process window, but not debugging, do nothing
	   t)
	  ((and (get-buffer eclipse-process-buffer)
		;; if ECLiPSe process exists...
		(save-excursion
		  (set-buffer  eclipse-process-buffer)
		  (save-excursion
		    (forward-line 0)
		    (looking-at ".*%>"))))
	   ;; ...and we're debugging, add filter function, then send break signal
	   (add-hook 'comint-preoutput-filter-functions 'eclipse-toggle-source-bp1)
	   (setq eclipse-dbgflag t)
	   (process-send-string eclipse-process-name "b\x0d"))
	  (t
	   ;; else, just add breakpoint
	   (eclipse-toggle-source-bp1)))))


;;
;; ECLiPSe Outline commands
;;
;; This part provides outline commands inside the ECLiPSe major mode.

;; we're using outline without running it as minor mode, so we have to define
;; these variables here
(defvar outline-regexp "[a-z]\\|:-\\|[ \t]+("
  "*Regular expression to match the beginning of a heading.")

(defvar outline-heading-end-regexp "do[ \t\n]\\|->\\|:-\\|\\."
  "*Regular expression to match the end of a heading line.")

(defun eclipse-outline (map)
  ;; add functions for outlining to map
  (require 'outline)
  (eclipse-outline-define-map map)
  (set (make-local-variable 'outline-level) 'eclipse-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t)))

(defun eclipse-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line."
  (save-excursion
    (looking-at outline-regexp)
    (if (looking-at "\\([a-z]\\|[:?]-\\)")
	0
      (- (match-end 0) (match-beginning 0)))))

(defun eclipse-outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (unless eclipse-esp-selected
    (when (re-search-forward (concat "\\(/\\*\\|\n\\(" outline-regexp "\\)\\)") nil 0)
      (let ((aux (1+ (match-beginning 0))))
	(goto-char (match-beginning 0))
	(cond ((looking-at "/\\*")
	       (unless (re-search-forward "\\*/" (point-max) t)
		 (goto-char (point-max)))
	       (eclipse-outline-next-heading))
	      (t (goto-char aux)))))))

(defun eclipse-outline-previous-heading ()
  "Call \\[outline-previous-heading], unless ESP mode is on."
  (interactive)
  (unless eclipse-esp-selected
    (outline-previous-heading)))

(defun eclipse-outline-next-visible-heading (arg)
  "Call \\[outline-next-visible-heading], unless ESP mode is on."
  (interactive "p")
  (unless eclipse-esp-selected
    (outline-next-visible-heading arg)))

(defun eclipse-outline-previous-visible-heading (arg)
  "Call \\[outline-next-visible-heading], unless ESP mode is on."
  (interactive "p")
  (unless eclipse-esp-selected
    (outline-next-visible-heading (- arg))))

(defun eclipse-outline-mark-subtree ()
  "Call \\[outline-mark-subtree], unless ESP mode is on."
  (interactive)
  (unless eclipse-esp-selected
    (outline-mark-subtree)))

(defun eclipse-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (save-excursion
    (goto-char from)
    (end-of-line)
    (outline-flag-region (point) to flag)))

(defun eclipse-hide-clause ()
  "Hide the clause directly following this heading."
  (interactive)
  (unless eclipse-esp-selected
    (unless (eclipse-check-clause-begin)
      (eclipse-goto-clause-begin))
    (outline-end-of-heading)
    (save-excursion
      (eclipse-outline-flag-region
       (point)
       (progn (eclipse-goto-clause-end) (point)) t))))

(defun eclipse-show-clause ()
  "Show the clause directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (eclipse-outline-flag-region
       (1- (point))
       (progn (eclipse-goto-clause-end) (point)) nil))))

(defun eclipse-hide-predicate ()
  "Hide the predicate directly following this heading."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-goto-clause-end)
    (eclipse-goto-predicate-begin)
    (outline-end-of-heading)
    (save-excursion
      (eclipse-outline-flag-region
       (point)
       (progn (eclipse-goto-predicate-end) (point)) t))))

(defun eclipse-show-predicate ()
  "Show the predicate directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (unless eclipse-esp-selected
    (save-excursion
      (eclipse-goto-clause-end)
      (eclipse-goto-predicate-begin)
      (eclipse-outline-flag-region
       (1- (point))
       (progn (eclipse-goto-predicate-end) (point)) nil))))

(defun eclipse-hide-predicates ()
  "Hide all of buffer except predicate headings."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-hide-region-body (point-min) (point-max))))

(defun eclipse-hide-clauses ()
  "Hide all of buffer except clause headings."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-hide-region-body (point-min) (point-max) t)))

(defun eclipse-hide-region-body (start end &optional flag)
  "Hide all body lines in the region, but not headings."
  (unless eclipse-esp-selected
    (let ((head (point-min))
	  (length (- end start)) (last 0))
      (message "Hiding...")
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (unless (outline-on-heading-p)
	    (eclipse-goto-clause-begin))
	  (outline-end-of-heading)
	  (while (not (eobp))
	    (eclipse-outline-flag-region
	     (point)
	     (progn (goto-char head)
		    (if flag
			(eclipse-goto-clause-end)
		      (eclipse-goto-predicate-end))
		    (point)) t)
	    (setq last (eclipse-percent-message "Hiding" length last))
	    (unless (eobp)
	      (eclipse-outline-next-heading)
	      (setq head (point))
	      (outline-end-of-heading))))))
    (message "Hiding...done.")))

(defun eclipse-show-predicates ()
  "Show all of buffer."
  (interactive)
  (eclipse-show-all))

(defun eclipse-show-clauses ()
  "Show all of buffer."
  (interactive)
  (eclipse-show-all))

(defun eclipse-show-all ()
  "Show all of buffer."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-region (point-min) (point-max) nil)))

(defun eclipse-hide-block ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-subtree t)))

(defun eclipse-show-block ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (unless eclipse-esp-selected
    (eclipse-outline-flag-subtree nil)))

(defun eclipse-outline-flag-subtree (flag)
  ;; flag subtree
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (eclipse-outline-flag-region
     (point)
     (progn
       (outline-back-to-heading)
       (eclipse-outline-end-of-block)
       (point))
     flag)))

(defun eclipse-outline-end-of-block ()
  ; we are looking at "[ \t]+("
  (let (level)
    (skip-chars-forward " \t")
    (forward-char)
    (setq level 1)
    (while (not (or (eobp) (= level 0)))
      (cond ((looking-at ")")
	     (setq level (- level 1))
	     (forward-char))
	    ((looking-at "(")
	     (setq level (+ level 1))
	     (forward-char))
	    ((looking-at "'")
	     (eclipse-goto-end-of-quote))
	    ((looking-at "\"")
	     (eclipse-goto-end-of-string))
	    (t (forward-char))))
    (forward-char -1)))

(defun eclipse-outline-up-heading (arg)
  "Call \\[outline-up-heading], unless ESP mode is on."
  (interactive "p")
  (unless eclipse-esp-selected
    (outline-up-heading arg)))

(defun eclipse-outline-forward-same-level (arg)
  "Call \\[outline-forward-same-level], unless ESP mode is on."
  (interactive "p")
  (unless eclipse-esp-selected
    (outline-forward-same-level arg)))

(defun eclipse-outline-backward-same-level (arg)
  "Call \\[outline-backward-same-level], unless ESP mode is on."
  (interactive "p")
  (unless eclipse-esp-selected
    (outline-backward-same-level arg)))

(defun eclipse-outline-headers-as-kill (beg end)
  "Call \\[outline-headers-as-kill], unless ESP mode is on."
  (interactive "r")
  (unless eclipse-esp-selected
    (outline-headers-as-kill beg end)))

;;
;; Compile mode support & syntax checking
;;

(defconst eclipse-compile-event-handlers
":- assert(module_list([])).

my_event_handler(Nr, Where, Module) :-
        error_id(Nr, Msg),
        (
            compiled_stream(Stream)
        ->
            get_stream_info(Stream, line, Line1),
            get_stream_info(Stream, offset, Offset1),
            get_stream_info(Stream, name, Name),
            get_flag(version_as_list, _Version),
            (
                Name == user
            ->
                Offset is Offset1 - 2636,
                Line is Line1 - 86
            ;
                Offset is Offset1,
                Line = Line1
            ),
            module_list([M|_]),
            (
                Name == M
            ->
                printf(\"*** %w ** %w ** %w ** %w%n%b\",
                       [Msg, Name, Line, Offset])
            ;
                true
            )
        ;
            printf(\"*** %w ** %w in module %w%n%b\", [Msg, Where, Module])
        ).

my_compile_start_event_handler(_Nr, Where) :-
        Msg = \"Start of compilation\",
        printf(\">>> %w ** %w%n%b\", [Msg, Where]),
        retract(module_list(L)),
        assert(module_list([Where|L])).

my_compile_end_event_handler(_Nr, Where) :-
        Msg = \"Compilation ended\",
        (
            Where = (term, _, _)
        ->
            true
        ;
            Where = (Name, Size, Time),
            printf(\"<<< %w ** %w ** %w bytes ** %w seconds%n%b\",
                   [Msg, Name, Size, Time])
        ),
        retract(module_list([_|L])),
        assert(module_list(L)).

:- (
       for(Nr, 1, 333)
   do
       (
           Nr == 146
       ->
           set_event_handler(Nr, my_compile_start_event_handler/2)
       ;
           Nr == 139
       ->
           set_event_handler(Nr, my_compile_end_event_handler/2)
       ;
           current_error(Nr),
           get_event_handler(Nr, Handler, _),
           memberchk(Handler,
                     [parser_error_handler/2, error_handler/2,
                      error_handler/3, error_handler/4, warning_handler/3,
                      singleton_in_loop/2, compiler_error_handler/2,
                      call_handler/4, redef_other_file_handler/2,
                      undef_array_handler/3, undef_record_handler/2,
                      dynamic_handler/3, undef_dynamic_handler/3,
                      declaration_warning_handler/3,
                      locked_access_handler/2, no_lookup_module_handler/4,
                      ambiguous_import_warn/3, macro_handler/3])
       ->
           set_event_handler(Nr, my_event_handler/3)
       ;
           true
       )
   ),
   set_stream(warning_output, output).
"
"ECLiPSe predicates to reformat compile time warning and error messages.
These predicates are used so that compiling ECLiPSe code will result in
standardised output that can be easily parsed by the Emacs compile mode
functions.")

(defun eclipse-compilation-parse-errors-filename-function (filename-in)
  (let (filename-out)
    (cond ((string-equal filename-in "user")
	   (setq filename-out (buffer-file-name (get-buffer eclipse-compile-buffer))))
	  (t
	   (find-file-noselect filename-in t)
	   (setq filename-out filename-in)))
    filename-out))
  
(defun parse-and-insert (messages)
  ;; parses compile errors and warnings
  (let ((current-file nil)
	(current-buffer nil)
	(current-offset nil)
	(current-line-offset nil)
	start end aux line col clause predicate msg)
    ;; Parse messages.
    (while messages
      ;; next message
      (setq msg (car messages)
	    messages (cdr messages))
      (when (and msg (not (equal msg "")))
	(cond ((equal (substring msg 0 3) "***") ; error or warning
	       (setq col (string-to-number (cadr (cddr (split-string msg "\\( \\*\\* \\)"))))
		     aux (eclipse-compile-find-pos (car current-buffer) (+ col (car current-offset)))
		     line (car aux)
		     col (cadr aux)
		     aux (split-string msg "\\( \\*\\* \\)")
		     msg (concat (car aux) " ** " (cadr aux) " ** " line " ** " col)))
	      ((equal (substring msg 0 4) "File") ; ECLiPSe 6.0 warnings
	       (setq aux (split-string msg "\\(: \\)")
		     msg (cadr aux)
		     line (number-to-string
			   (- (string-to-number (cadr (split-string (car aux) "\\(line \\)")))
			      (if (string-match "File user" (car aux)) (- 86 (car current-line-offset)) 0)))
		     msg (concat "*** WARNING: " msg " ** " (car current-file) " ** " line " ** 1")))
	      ((equal (substring msg 0 3) ">>>") ; new file opened
	       (setq filename (cadr (split-string msg "\\(\\*\\* \\)")))
	       (cond ((string-equal filename "user")
		      ;; top level: get filename of eclipse-compile-buffer, put info on stack
		      (setq current-file (cons (buffer-file-name (get-buffer eclipse-compile-buffer)) current-file)
			    current-buffer (cons eclipse-compile-buffer current-buffer)
			    current-offset (cons eclipse-compile-offset current-offset)
			    current-line-offset (cons (save-excursion
							(set-buffer eclipse-compile-buffer)
							(1- (line-number-at-pos eclipse-compile-offset)))
						      current-line-offset)))
		     (t ; else get filename and open file if necessary, put info on stack
		      (setq current-file (cons filename current-file)
			    current-buffer (cons (buffer-name (find-file-noselect filename t)) current-buffer)
			    current-offset (cons 0 current-offset)
			    current-line-offset (cons 0 current-line-offset)))))
	      ((equal (substring msg 0 3) "<<<") ; file finished: pop stack
	       (setq current-file (cdr current-file)
		     current-buffer (cdr current-buffer)
		     current-offset (cdr current-offset)
		     current-line-offset (cdr current-line-offset)))
	      ((string-match "structure" msg) ; unrecognized structure: we only support new style (ECLiPSe 6.0 and later)
	       (unless (equal (substring msg 0 1) "*") ; make it look like other messages
		 (setq msg (concat "*** " msg)))
	       (setq aux (cadr (split-string msg "\\(in \\)"))
		     start (string-match "module \\([^\n]\\)+ in " msg))
	       (unless start
		 (setq start (length msg)
		       msg (concat msg " " (car messages))
		       messages (cdr messages)))
	       (setq end (string-match "{" msg ))
	       (unless end
		 (setq end (string-match " with" msg )))
	       (setq aux (substring msg start end))
	       (save-excursion ; find appropriate point in buffer to point to
		 (set-buffer (car current-buffer))
		 (goto-char (point-min))
		 (re-search-forward (concat aux "\\({\\|[ \t]+with\\)") (point-max) t)
		 (goto-char (1+ (match-beginning 0)))
		 (setq line (number-to-string (line-number-at-pos))
		       col (number-to-string (1+ (- (point) (progn (beginning-of-line) (point)))))
		       msg (concat msg " ** " (car current-file) " ** " line " ** " col))))
	      (t t)) ; else do nothing
	(insert msg)
	(insert "\n")))
    ;; fontify
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (set-buffer-modified-p nil)))

(defun parse-lint-and-insert (bufname messages)
  ;; parses lint warnings
  (setq compilation-error-list nil)
  (let* ((buf (get-buffer bufname))
	 (current-file (buffer-file-name buf))
	 msg aux aux2 line)
    ;; Parse messages.
    (while messages
      (setq msg (car messages)
	    messages (cdr messages))
      (when (and msg (not (equal msg "")))
	(cond ((or (equal (substring msg 0 4) "File")
		   (equal (substring msg 0 3) "---"))
	       (setq msg (concat msg " " (car messages))
		     messages (cdr messages)
		     aux (split-string msg "\\(: \\)")
		     aux2 (car aux)
		     msg (cadr aux)
		     aux (cddr aux))
	       (while aux
		 (setq msg (concat msg ": " (car aux))
		       aux (cdr aux)))
	       (setq line (cadr (split-string aux2 "\\(line \\)"))
		     msg (concat "*** WARNING: " msg " ** " current-file " ** " line " ** 1")))
	      ((equal (substring msg 0 4) "WARN")
	       (setq msg (concat msg " " (car messages))
		     messages (cdr messages)
		     aux (cdr (split-string msg "\\(: \\)"))
	       	     msg (cadr aux)
		     aux (split-string (car aux) "\\(line \\)")
		     line (cadr aux)
		     msg (concat "*** WARNING: " (car aux)  msg " ** " current-file " ** " line " ** 1")))
	      (t t))
	(insert msg)
	(insert "\n")))
    ;; fontify
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (set-buffer-modified-p nil)))

(defun eclipse-compile-find-clause (buffer predicate clause)
  ;; find the appropriate clause of predicate
  (save-excursion
    (let ((i 1))
      (set-buffer buffer)
      (goto-char (point-min))
      (eclipse-goto-clause-begin)
      (unless (string-equal predicate (eclipse-get-current-predicate-template t))
 	(while (not (string-equal predicate (eclipse-get-current-predicate-template t)))
 	  (eclipse-goto-predicate-end)
	  (eclipse-jump-over-strings t t)))
      (while (< i clause)
	(eclipse-goto-clause-end)
	(eclipse-jump-over-strings t t)
 	(setq i (1+ i)))
      (list (number-to-string (line-number-at-pos (point)))
	    (number-to-string (1+ (- (point) (progn (beginning-of-line) (point)))))))))

(defun eclipse-compile-find-pos (buffer pos)
  ;; find the appropriate clause of predicate
  (save-excursion
    (set-buffer buffer)
    (goto-char pos)
    (message (number-to-string pos))
    (list (number-to-string (line-number-at-pos (point)))
	  (number-to-string (1+ (- (point) (progn (beginning-of-line) (point))))))))

(defvar eclipse-compile-call-origin nil)
(defvar eclipse-compile-call-mode 0)

(defun eclipse-call-compile (mode code &optional fileflag)
  "Compiles ECLiPSe code using the Emacs compile mode."
  (interactive)
  (if (not eclipse-emacs-22)
      (prog2
	  (beep)
	  (message "Newer Emacs required (>= 22)"))
    (require 'compile)
    (require 'comint)
    (setq eclipse-compile-call-origin (buffer-name)
	  eclipse-compile-call-mode mode)
    (save-excursion
      (message (concat "Start " mode "..."))
      (let ((ecb (get-buffer eclipse-compilation-buffer))
            (inhibit-read-only t))
        ;; make compilation buffer
        (when (get-buffer eclipse-compile-process-buffer)
          (kill-buffer eclipse-compile-process-buffer))
        (when ecb
          (save-excursion
            (set-buffer eclipse-compilation-buffer)
            (delete-region (point-min) (point-max))))
        (set-buffer (make-comint eclipse-compile-process-name eclipse-program-call))
        (setq comint-use-prompt-regexp "\\[eclipse [0-9]+\\]:")
        (add-hook 'comint-output-filter-functions 'eclipse-compile-filter)
        ;; switch to compile (output) buffer
        (if (and ecb
                 (window-live-p (get-buffer-window ecb)))
            (progn
              (select-window (get-buffer-window ecb))
              (switch-to-buffer (get-buffer-create eclipse-compilation-buffer)))
          (split-window)
          (select-window (next-window))
          (switch-to-buffer (get-buffer-create eclipse-compilation-buffer)))
        ;; switch to compilation mode, and set the variables
        (compilation-mode)
        (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
        (setq inhibit-read-only t
              compilation-error-screen-columns nil
              compilation-read-command nil
              compile-command nil
              compilation-parse-errors-filename-function 'eclipse-compilation-parse-errors-filename-function
              eclipse-compile-buffer eclipse-compile-call-origin
              compilation-error-regexp-alist-alist
              (if (string-equal eclipse-compile-call-mode "compiling")
                  '((eclipse-warning "\\*\\*\\* WARNING: \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([0-9]+\\) \\*\\* \\(-?[0-9]+\\)" 2 3 4 (1))
                    (eclipse-error "\\*\\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([0-9]+\\) \\*\\* \\(-?[0-9]+\\)" 2 3 4)
                    (eclipse-module-error "\\*\\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) in module \\(.+\\)" 3))
                ;; (string-equal eclipse-compile-call-mode "checking")
                '((eclipse-warning "\\*\\*\\* WARNING: \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([0-9]+\\) \\*\\* \\(-?[0-9]+\\)" 2 3 4 (1))
                  (eclipse-error "\\*\\*\\* \\([^*]+\\) \\*\\* \\([^*]+\\) \\*\\* \\([0-9]+\\) \\*\\* \\(-?[0-9]+\\)" 2 3 4 )
                  (eclipse-lint-warning "WARNING: Directive failed or aborted in file \\([^,]+\\), line \\([0-9]+\\):" 1 2)))
              compilation-error-regexp-alist
              (if (string-equal eclipse-compile-call-mode "compiling")
                  '(eclipse-warning eclipse-error eclipse-module-error)
                ;; (string-equal eclipse-compile-call-mode "checking")
                '(eclipse-warning eclipse-error eclipse-lint-warning)))
        (font-lock-add-keywords nil (compilation-mode-font-lock-keywords))
        (cond ((string-equal mode "compiling")
               ;; redefine event handlers first!
               (process-send-string eclipse-compile-process-name eclipse-compile-string)
               (process-send-string eclipse-compile-process-name eclipse-compile-event-handlers)
               (process-send-string eclipse-compile-process-name "\n")
               (if eclipse-eof-string
                   (process-send-string eclipse-compile-process-name eclipse-eof-string)
                 (process-send-eof eclipse-compile-process-name)) ;Send eof to eclipse process.
               ;; now send code to eclipse
               (cond (fileflag
                      (process-send-string eclipse-compile-process-name "compile('")
                      (process-send-string eclipse-compile-process-name code)
                      (process-send-string eclipse-compile-process-name "').")
                      (process-send-string eclipse-compile-process-name "\n"))
                     (t
                      (process-send-string eclipse-compile-process-name eclipse-compile-string)
                      (process-send-string eclipse-compile-process-name code)
                      (process-send-string eclipse-compile-process-name "\n"))))
              ((string-equal mode "checking")
               (process-send-string eclipse-compile-process-name eclipse-lint-call)
               (process-send-string eclipse-compile-process-name (concat eclipse-lint-cmd1
                                                                         code
                                                                         eclipse-lint-cmd2))))
        (if eclipse-eof-string
            (process-send-string eclipse-compile-process-name eclipse-eof-string)
          (process-send-eof eclipse-compile-process-name)) ;Send eof to eclipse process.
        ;; send halt command to eclipse, then wait until eclipse is done checking
        (process-send-string eclipse-compile-process-name eclipse-halt-string)
        (while (eq (process-status eclipse-compile-process-name) 'run)
          (sleep-for 0.1))))))

(defun eclipse-compile-filter (output)
  (when (string-match "\\[eclipse 3\\]:" output)
    (remove-hook 'comint-output-filter-functions 'eclipse-compile-filter)
    (eclipse-compile-process-result))
  "")

(defun eclipse-compile-process-result ()
  (set-buffer eclipse-compile-process-buffer)
  (goto-char (point-max))
  (let* ((buf eclipse-compile-call-origin)
	 (start (save-excursion
		  (search-backward "[eclipse 2]:" (point-min) t)
		  (forward-char 13)
		  (point)))
	 (end (save-excursion
		(goto-char start)
		(search-forward "[eclipse 3]:" (point-max) t)
		(forward-line -2)
		(point)))
	 ;; split the output in single lines
	 (auxmsg (split-string (buffer-substring start end) "\n"))
	 (messages '()))
    ;; delete leading whitespace
    (while (car auxmsg)
      (setq messages (append messages (list (replace-regexp-in-string "^[ \t]*" "" (car auxmsg))))
	    auxmsg (cdr auxmsg)))
    (if  (string-equal eclipse-compile-call-mode "checking")
	(message "Checking done")
      (message "Compiling done"))
    (let ((eclipse-compile-window (get-buffer-window eclipse-compilation-buffer))
	  (inhibit-read-only t))
      ;; switch to compile (output) buffer
      (if eclipse-compile-window
	  (select-window eclipse-compile-window)
	(split-window)
	(select-window (next-window)))
      (switch-to-buffer eclipse-compilation-buffer)
      ;; insert the compilation output and parse it
    (if  (string-equal eclipse-compile-call-mode "checking")
	(parse-lint-and-insert buf messages)
      (parse-and-insert messages)))))

(defun eclipse-check-buffer ()
  "Compiles the file in a buffer in an ECLiPSe process.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (if eclipse-emacs-22
      (unless eclipse-esp-selected
	(cond ((buffer-modified-p)
	       (beep)
	       (message "Please save the buffer before calling this function."))
	      (t
	       (save-excursion
		 (setq eclipse-compile-offset 0)
		 (eclipse-call-compile "compiling" (buffer-file-name) t)))))
    (beep)
    (message "Newer Emacs required (>= 22)")))

(defun eclipse-check-region ()
  "Send the current region to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (if eclipse-emacs-22
      (unless eclipse-esp-selected
	(save-excursion
	  (setq eclipse-compile-offset (region-beginning))
	  (eclipse-call-compile "compiling" (buffer-substring (region-beginning) (region-end)))))
    (beep)
    (message "Newer Emacs required (>= 22)")))

(defun eclipse-check-predicate ()
  "Send the current predicate to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (if eclipse-emacs-22
      (unless eclipse-esp-selected
	(save-excursion
	  (let ((point1 (prog2 (eclipse-goto-predicate-begin) (point)))
		(point2 (prog2 (eclipse-goto-predicate-end) (point))))
	    (setq eclipse-compile-offset point1)
	    (eclipse-call-compile "compiling" (buffer-substring point1 point2)))))
    (beep)
    (message "Newer Emacs required (>= 22)")))

(defun eclipse-check-clause ()
  "Send the current clause to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compilation*."
  (interactive)
  (if eclipse-emacs-22
      (unless eclipse-esp-selected
	(save-excursion
	  (let ((point1 (prog2 (eclipse-goto-clause-begin) (point)))
		(point2 (prog2 (eclipse-goto-clause-end) (point))))
	    (setq eclipse-compile-offset point1)
	    (eclipse-call-compile "compiling" (buffer-substring point1 point2)))))
    (beep)
    (message "Newer Emacs required (>= 22)")))

(defun eclipse-lint-buffer ()
  "Check the buffer using the library lint.
Warnings and errors are returned in buffer *eclipse-lint*.
If the current buffer has been edited, it must be saved to file first.

Note that the line count is unreliable if directives in the code contain
errors. In this case, the subsequent warnings in the output window are
not linked to the code correctly."
  (interactive)
  (if eclipse-emacs-22
      (unless eclipse-esp-selected
	(cond ((buffer-modified-p)
	       (beep)
	       (message "Please save the buffer before calling this function."))
	      (t
	       (setq eclipse-compile-offset 0)
	       (eclipse-call-compile "checking" (buffer-file-name)))))
    (beep)
    (message "Newer Emacs required (>= 22)")))

(provide 'eclipse)

;;; eclipse.el ends here

