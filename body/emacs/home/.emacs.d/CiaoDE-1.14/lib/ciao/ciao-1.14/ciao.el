;; -*- mode: emacs-lisp; -*-
;;
;; DO NOT CHANGE THIS FILE
;; IT HAS BEEN GENERATED AUTOMATICALLY
;;
;;---------------------------------------------------------------------------
;; Emacs support for the Ciao programming environment
;; (can be used as a general Prolog mode under Emacs)
;;---------------------------------------------------------------------------
;; Copyright (C) 1986-2002 Free Software Foundation, Inc. and M. Hermenegildo
;; and others (herme@fi.upm.es, UPM-CLIP, Spain) See 'Other comments,
;; acknowledgements and changelog/version info' below for history,
;; other authors, and log of changes. 
;; 
;; 
;; This file is part of GNU Emacs.  GNU Emacs is distributed in the
;; hope that it will be useful, but WITHOUT ANY WARRANTY.  No author
;; or distributor accepts responsibility to anyone for the
;; consequences of using it or for whether it serves any particular
;; purpose or works at all, unless he says so in writing.  Refer to
;; the GNU Emacs General Public License for full details.  Everyone is
;; granted permission to copy, modify and redistribute GNU Emacs, but
;; only under the conditions described in the GNU Emacs General Public
;; License.  A copy of this license is supposed to have been given to
;; you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named GPL.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.
;; 
;;---------------------------------------------------------------------------
;; To use this mode the following lines must be present in your '.emacs'
;; (the Ciao installation procedure typically builds a file 'ciao-mode-init.el'
;; with the correct paths which you can include directly):
;;---------------------------------------------------------------------------
;;
;;; -*- mode: emacs-lisp; -*-
;;;;
;;;; Ciao/Prolog mode initialization
;;;; -------------------------------
;;;; (can normally be used with other Prolog modes and the default prolog.el)
;;;; 
;;(setq load-path (cons "~/CiaoDE/lib/ciao" load-path))
;;;; Java mode in ciao                                 
;;(setq load-path
;;   (cons "~/CiaoDE/lib/ciao" load-path))
;;(defun load-java-ciaopp-mode ()
;;  (require 'java-ciaopp)
;;  (java-ciaopp-setup))
;;(add-hook 'java-mode-hook 'load-java-ciaopp-mode)
;;
;;(autoload 'run-ciao-toplevel "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'ciao-startup "ciao"
;;          "The Ciao/Prolog program development system startup screens." t)
;;(autoload 'ciao "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'prolog "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'run-ciao-preprocessor "ciao"
;;          "Start a Ciao/Prolog preprocessor sub-process." t)
;;(autoload 'ciaopp "ciao"
;;          "Start a Ciao/Prolog preprocessor sub-process." t)
;;(autoload 'ciao-mode "ciao"
;;          "Major mode for editing and running Ciao/Prolog" t)
;;(autoload 'ciao-inferior-mode "ciao"
;;          "Major mode for running Ciao/Prolog, CiaoPP, LPdoc, etc." t)
;;(setq auto-mode-alist (cons '("\\.pl$" . ciao-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.pls$" . ciao-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.lpdoc$" . ciao-mode) auto-mode-alist))
;;(setq completion-ignored-extensions
;;      (append '(".dep" ".itf" ".po" ".asr" ".cpx")
;;              completion-ignored-extensions))
;;;; ------------------------------------------------------------------------
;;;; In Un*x, the following (or similar) lines should be included in your
;;;; .cshrc or .profile to find the manuals (the Ciao installation leaves
;;;; in the Ciao library directory 'DOTcshrc' and 'DOTprofile' files with
;;;; the right paths which can be included directly in your startup scripts):
;;;; 
;;;; setenv INFOPATH /usr/local/info:/usr/info:"~/public_html/CiaoDE//"
;;;; ------------------------------------------------------------------------
;;% -----------------------------------------------------------
;;% Other comments, acknowledgments and changelog/version info 
;;% -----------------------------------------------------------
;;
;;:- use_package([assertions]).
;;
;;:- doc(filetype, documentation).
;;
;;:- doc(title,"Using Ciao inside GNU emacs").
;;:- doc(subtitle,"An interactive program development environment for Ciao").
;;
;;:- doc(subtitle_extra,"@bf{The Ciao System Documentation Series}").
;;:- doc(subtitle_extra,"Technical Report CLIP 4/00.5.81").
;;:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
;;
;;:- doc(author,"Manuel Hermenegildo").
;;:- doc(author,"Manuel C. Rodriguez").
;;:- doc(author,"Daniel Cabeza").
;;
;;:- include(library('ClipAddress')).
;;
;;:- include(library('Copyright')).
;;
;;:- doc(summary,"This documents the Ciao emacs interface (or @em{mode}
;;    in @apl{emacs} terms), which provides a rich, integrated user interface
;;    to the Ciao program development environment components, including the
;;    @apl{ciaosh} interactive top level and the @apl{ciaopp}
;;    preprocessor. While most features of the Ciao development environment
;;    are available from the command line of the preprocessor and the
;;    top-level shell, using Ciao inside @apl{emacs} is highly recommended,
;;    since it greatly facilitates the process of editing, compiling,
;;    running, and debugging Ciao programs. 
;;
;;    In particular, source-level
;;    debugging and location of errors in source files, syntax highlighting,
;;    automatic access to online help, and automatic version control are only
;;    available within the Ciao emacs mode.").
;;
;;:- doc(module,"@include{CiaoMode.lpdoc}").
;;
;;
;;; -*- mode: emacs-lisp; -*-
; Changelog now in version control (previously in CiaoMode.pl).

;; *** prompt is not orange in emacs-22?
;;  --> this is because comint-highlight-prompt does not exist in emacs-22 :-(
;; @include{README_CIAOPP.lpdoc}
;; was Systems/ciaopp/doc but ciaopp is now 1.0 and does not have
;; (yet) a README.lpdoc file
;; 
;; The values in the automatically generated lpdoc SETTINGS file must
;; be configurable.
;; - It seems that a module-qualified goal in a predicate is not correctly
;;   marked in source debugging, the previous line is marked instead.
;; - When an empty .pl file is opened, add module declaration etc.
;; Preprocess with options should take you to the other buffer.
;; When an empty .pl file is visited an empty module declaration is
;; added and perhaps even version control.
;; The same way as with versions, files could have an indication of
;; where the 'project' file is (with the main, etc.)
;; **** add button for seeing output
;; **** xemacs tool bar
;; **** local doc generator has to be rebuilt (it is broken right now)
;; **** inferior should not be visible? (too advanced for now...)
;; **** Inferior mode 'Ciao' menu should change depending on whether
;;      it is LPdoc or CiaoPP, or ...
;; **** Tool bar has a button so that one can be in preprocess, debug,
;;      help, etc. mode, and the toolbar changes
;; **** do not use word help
;; **** really test on macs and windows, older versions, etc.
;; **** debugger does not work on files with .pls ending?
;; 
;; *** Frame titles: / In any case title should be 'Ciao'
;; default:
;; (setq (multiple-frames "%b" ("" invocation-name "@" system-name)))
;; ideas:
;; (setq frame-title-format (concat "Ciao: " "%b"))
;; (setq frame-title-format "%b")
;; 
;; **** Coloring:
;; %-type comments after a use_module declaration.
;; 
;; When the first argument of a predicate is a quoted atom, the open
;; parenthesis is also colored red: 
;; separator('(' , _, '') :- !.
;; 
;; On these (which again do not work), see comments by DTM below.
;; incorrectly_colored('Atom ending in 0').
;; 
;;  trans_info(eapp026, 'Webmergers 120').
;;  trans_info(eapp030, 'QSP''s ASP business').
;;  trans_info(tech055, 'AiSoftw@re SpA').
;;  trans_info(eser002, 'ISP Europeo, la participacion del 49,5 % poseida').
;;  
;;  I think that at least the two first can (and should) be easily
;;  corrected.
;; 
;; Should also color variables, etc. in top level?
;;
;; On Mac:
;;    Ciao requires the definition of some shell environment variables
;;    through the DOTcshrc or DOTprofile.  If correctly configured, when
;;    emacs is launched from a terminal all works fine, but not when you
;;    load emacs from the desktop icon (or dockbar, finder, etc.). It seems
;;    that these applications doesn't load any profile file (in a standard
;;    way).
;; 
;;    A workaround may be starting emacs from a terminal (ensuring that Ciao
;;    works in a terminal) in this way:
;; 
;;    $ open /Applications/Emacs.app
;;    - We could see if there is a way to load the user's profile file from
;;      the .emacs file.
;;    - We should in any case document it in the manual.
;; 
;; **** Define a 'novice' or 'ciao environment' variable. When on, menus 
;;      are very simple and only talk about Ciao (nothing on emacs). 
;;      In any case, menus should be divided into not Ciao or CiaoPP,
;;      but rather in other classes, e.g., help, debug, compile,
;;      precompile, document, version control, config, gui builder, etc.
;; emacs -q -l /home/clip/lib/ciao/ciao-mode-init -f ciao-startup
;; **** Generate task lists from the bug lists of distributions? (Paco)
;; **** Have a good bug reporting mechanism
;; **** Eliminate a changelog entry
;; **** Need to fix better the deleted buffer problem
;; 
;; Missing buttons in toolbar for new emacs versions??
;; 
;;    '(font-latex-match-font-outside-braces		      ;;;\textit{text}
;;      (0 font-lock-keyword-face
;;         append                         ;Override? [t 'keep 'prepend 'append]
;;         ;; Can't use prepend because that overwrites syntax fontification
;;         ;; e.g. comments.
;;         t)                              ;Laxmatch? if t, do not signal error
;;      (1 font-latex-italic-face append t)
;;      (2 font-latex-bold-face append t)
;;      (3 font-lock-type-face append t))

;; **** Remember to keep track of which version of emacs we are working with
;; **** C-u <compilation command> asks for options, remembers them per
;;      buffer (or set them through a menu)?

;; --------------------------------------------------------------------------
;; The actual code of the mode starts here.
;; --------------------------------------------------------------------------

;; In emacs this is done most reliably by setting INFOPATH (done in
;; Ciao installation).  xemacs does need it for finding the Ciao
;; manuals (does not seem to read INFOPATH). 

;; These three are rewritten during installation:
(defvar ciao-bin-dir "~/CiaoDE/bin"
  "Where the actual Ciao binaries are.")

(defvar ciao-real-lib-dir "~/CiaoDE/lib/ciao/ciao-1.14"
  "Where the actual Ciao lib directory is (and, thus, e.g., the image files).")

(defvar ciao-info-dir "~/.emacs.d/CiaoDE-1.14/public_html"
  "Where the actual Ciao (LPdoc) info directory is.")

;(if (boundp 'xemacs-logo)
    (progn
      (load-library "info")             ; Info creates Info-directory-list
      ;; (require 'info)
      (if (null Info-directory-list)    ; but it is not initialized there
          (setq Info-default-directory-list ; Will be initialized from here
                (cons ciao-info-dir Info-default-directory-list))
        (setq Info-directory-list (cons ciao-info-dir Info-directory-list))
        )
      ) 
;  )

;; This is so that the other .el files (word-help, etc.) in the Ciao
;; lib are found (this path is updated automatically during installation):
(setq load-path (cons ciao-real-lib-dir load-path))

;; This is to avoid warnigns in fsf from xemacs vars and functions.
(eval-when-compile
  (if (boundp 'xemacs-logo)
      ()
    ;; fsf
    (defconst default-toolbar nil)
    (defconst right-toolbar-visible-p nil)
    (defconst right-toolbar-width nil)
    (defconst right-toolbar nil)
    (defun toolbar-make-button-list (&rest foo))
    (defun console-on-window-system-p (&rest foo))
    (defun set-specifier (&rest foo))
    (defun specifier-specs (&rest foo))
    (defun make-glyph (&rest foo))
    (defun extent-at (&rest foo))
    (defun make-extent (&rest foo))
    (defun set-extent-property (&rest foo))
    (defun ciao-createenumerated-list (&rest foo))
    ))


;; --------------------------------------------------------------------------
;; Mode documentation and acks (see also documentation in functions
;; and the CiaoMode.pl file included above)
;; --------------------------------------------------------------------------

(defun ciao-mode-documentation ()
  "This function generates documentation in lpdoc format for the
    Ciao mode commands and their bindings."
  (interactive)
  (switch-to-buffer "*ciao-tmp*")
  (ciao-mode-nocheck) ;; so that the bindings that we document are active!
;;  (ciao-inferior-mode) ;; so that the bindings that we document are active!
  
  (insert "@comment{** Do not edit--generated automatically **}

The Ciao @concept{emacs interface} (or @em{mode} @cindex{emacs mode}
in @apl{emacs} terms) provides a rich, integrated user interface to the
Ciao @index{program development environment} components, including the
@apl{ciaosh} interactive top level and the @apl{ciaopp} preprocessor. While
most features of the Ciao development environment are available from the
command line of the preprocessor and the top-level shell, using Ciao inside
@apl{emacs} is highly recommended. The facilities that this mode provides
include:

@begin{itemize}

@item @index{Syntax-based highlighting} (coloring), @cindex{coloring,
syntax} @index{auto-indentation}, @index{auto-fill}, etc. of code. This
includes the assertions used by the preprocessor and the documentation
strings used by the Ciao auto-documenter, @apl{lpdoc}.

@item Providing automatic access to @concept{on-line help} for all
predicates by accessing the Ciao system manuals in @apl{info} format.

@item Starting and communicating with @apl{ciaopp}, the @index{Ciao
preprocessor}, running in its own @concept{sub-shell}. This allows easily
performing certain kinds of @index{static checks} (useful for finding
errors in programs before running them), program analysis tasks, and
@index{program transformations} on source programs.

@item Starting and communicating with the @index{Ciao top-level}, running
in its own @concept{sub-shell}. This facilitates loading programs, checking
the @em{syntax} of programs (and of @index{assertions} within programs),
marking and unmarking modules for interactive debugging, @index{tracing the
source code} @cindex{source-level debugging} @cindex{debugging,
source-level} during debugging, making stand-alone executables, compiling
modules to dynamically linkable Ciao objects, compiling modules to active
objects, etc.

@item Syntax highlighting and coloring of the error and warning messages
produced by the top level, preprocessor, or any other tool using the same
message format (such as the @apl{lpdoc} auto-documenter), and @em{locating
automatically the points in the source files where such errors occur}.

@item Performing automatic @index{version control} and keeping a
@index{changelog} of individual files or whole applications. This is done
by automatically including changelog entries in source files, which can
then be processed by the @apl{lpdoc} auto-documenter.

@end{itemize}

This chapter explains how to use the Ciao @apl{emacs} interface and
how to set up your @apl{emacs} environment for correct operation.  The Ciao
@apl{emacs} interface can also be used to work with traditional Prolog or CLP
systems.

@section{Conventions for writing Ciao programs under Emacs}
@cindex{formatting conventions, for emacs} 

This is particularly important for the @concept{source-level debugger}
and the @concept{syntax-based coloring} capabilities.  This is due to
the fact that it would be unrealistic to write a complete Ciao
parser in Emacs lisp. These conventions are the following, in order of
importance:

@begin{itemize}

@item Clauses should begin on the first column (this is used to recognize
      the beginning of a clause). 

@item C style comments should not be used in a clause, but can be used
      outside any clause.

@end{itemize}

@noindent 
The following suggestion is not strictly necessary but can improve
operation:

@begin{itemize}

@item Body literals should be indented. There should be not more than
one literal per line. This allows more precision in the location of
program points during source-level debugging, i.e., when marking
breakpoints and during line tracing.

@end{itemize}

@noindent Comments which start with @tt{%}s are indented to the right
if indentation is asked for.

@noindent For syntax-based highlighting to be performed font-lock must
be available and not disabled (the Ciao mode enables it but it may be
disabled elsewhere in, e.g., the @file{.emacs} file).

@section{Checking the installation}

Typically, a complete pre-installation of the Ciao @apl{emacs}
interface is completed during Ciao installation. To check that
installation was done and sucessful, open a file with a @tt{.pl}
ending. You should see that @apl{emacs} enters Ciao mode: the
mode is identified in the @concept{status bar} below the
@concept{buffer} and, if the @concept{emacs menu bar} is enabled, you
should see the Ciao menus. You should be able from the
menu-bar, for example, to go to the Ciao manuals in the info or load
the @tt{.pl} file that you just opened into a Ciao top level.

If things don't work properly, see the section @ref{Installation of the
Ciao emacs interface} later in this chapter.

@section{Functionality and associated key sequences (bindings)}

The following sections summarize the capabilities of the Ciao
emacs interface and the (default) @index{key sequences} used to access
those capabilities.  Most of these functions are accessible also from
the menu bar.

")

  ;; This inserts the documentation strings for the bindings.
  (ciao-do-document-bindings (nreverse ciao-documented-commands))

  (insert (concat "

@section{Using Ciao mode capabilities in standard shells} 

The capabilities (commands, coloring, error location, ...) which are
active in the Ciao @em{inferior} mode can also be made
available in any standard command line shell which is being run within
emacs. This can be enabled by going to the buffer in which the shell
is running and typing ``@key{M-x} @tt{ciao-inferior-mode}''.  This is
very useful for example when running the stand-alone compiler, the
@apl{lpdoc} auto-documenter, or even certain user applications (those
that use the standard error message library) in an emacs
sub-shell. Turning the Ciao inferior mode on on that sub-shell
will highlight and color the error messages, and automatically find
and visit the locations in the files in which the errors are reported.

Finally, one the most useful applications of this is when using the
@concept{embedded debugger} (a version of the debugger which can be
embedded into executables so that an interactive debugging session can
be triggered at any time while running that executable without needing
the top-level shell). If an application is run in a shell buffer which
has been set with Ciao inferior mode (@key{M-x} @tt{ciao-inferior-mode}) and
this application starts emitting output from the embedded debugger
(i.e., which contains the embedded debugger and is debugging its code)
then the Ciao emacs mode will be able to follow these messages, for
example tracking execution in the source level code. This also works
if the application is written in a combination of languages, provided
the parts written in Ciao are compiled with the embedded debugger
package and is thus a covenient way of debugging multi-language
applications. The only thing needed is to make sure that the output
messages appear in a shell buffer that is in Ciao inferior mode.



@section{Customization}

This section explains all variables used in the Ciao emacs mode
which can be customized by users. Such customization can be performed
(in later versions of @apl{emacs}) from the @apl{emacs} menus
(@tt{Help -> Customize -> Top-level Customization Group}), or also by
adding a @tt{setq} expression in the @tt{.emacs} file. Such @tt{setq}
expression should be similar to:

@tt{(setq <variable> <new_value>)}

@noindent The following sections list the different variables which can be
customized for @apl{ciao}, @apl{ciaopp} and @apl{lpdoc}.\n"))

(ciao-document-variables)

(insert (concat "

@section{Installation of the Ciao emacs interface}

If opening a file ending with @tt{.pl} puts emacs in another mode
(such as @apl{perl} mode, which is the --arguably incorrect-- default
setting in some @apl{emacs} distributions), then either the emacs mode
was not installed or the installation settings are being overwritten
by other settings in your @tt{.emacs} file or in some library.  In any
case, you can set things manually so that the Ciao mode is
loaded by default in your system. This can be done by including in
your @file{.emacs} file a line such as:

@tt{(load ""<CIAOLIBDIR>/ciao-mode-init"")}

@noindent This loads the above mentioned file from the Ciao library, which
contains the following lines (except that the paths are changed during
installation to appropriate values for your system):

@begin{verbatim}
@includeverbatim{ciao-mode-init.el.skel}
@end{verbatim}

If you would like to configure things in a different way, you can also
copy the contents of this file to your @file{.emacs} file and make the
appropriate changes.  For example, if you do not want @tt{.pl} files
to be put automatically in Ciao mode, then comment out (or
remove) the line:

@tt{(setq auto-mode-alist} ... @tt{)}

@noindent You will then need to switch manually to Ciao mode by
typing @tt{M-x ciao-mode} after opening a Ciao file.

If you are able to open the Ciao menu but the Ciao manuals are not
found or the @apl{ciao} command (the top-level) is not found when loading
@tt{.pl} files, the probable cause is that you do not have the Ciao paths
in the @tt{INFOPATH} and @tt{MANPATH} @index{environment variables}
(whether these variables are set automatically or not for users depends on
how the Ciao system was installed). Under Un*x, you can add these paths
easily by including the line:

@tt{source <CIAOLIBDIR>/DOTcshrc}

@noindent in your @tt{.login} or @tt{.cshrc} files if you are using
@apl{csh} (or @apl{tcsh}, etc.), or, alternatively, the line:

@tt{. <CIAOLIBDIR>/DOTprofile}

@noindent in your @tt{.login} or @tt{.profile} files if you are using
@apl{sh} (or @apl{bash}, etc.). See the Ciao installation instructions
(@ref{Installing Ciao from the source distribution} or @ref{Installing
Ciao from a Win32 binary distribution}) for details.

@section{Emacs version compatibility} "

ciao-mode-emacs-version "

@section{Acknowledgments (ciao.el)}

This code is derived from the 1993 version of the emacs interface for
@concept{&-Prolog} by M. Hermenegildo, itself derived from the original
@file{prolog.el} by @index{Masanobu Umeda} with changes by @index{Johan
Andersson}, @index{Peter Olin}, @index{Mats Carlsson}, and @index{Johan
Bevemyr} of @index{SICS}, Sweden. Other changes also by Daniel Cabeza and
Manuel C. Rodriguez. See the changelog for details."

  ))
  (setq version-control 'never)
  (write-file "CiaoMode.lpdoc")
  )

;;----------------------------------------------------------------------------
;; Required packages (see also info below for xemacs, etc.)
;; ---------------------------------------------------------------------------

(require 'comint)
;; (require 'calendar)
(require 'easymenu)
;; (require 'word-help)
(require 'etags)

;; We use FSF Emacs overlays. XEmacs uses extents instead, but comes
;; with a package to emulate overlays.
(if (boundp 'xemacs-logo)
  (require 'overlay))
(provide 'ciao)


;; ===========================================================================
;; Mode variables
;; ===========================================================================

;; --------------------------------------------------------------------------
;; Note: version control information, title, authors, etc. is now maintained 
;;       automatically, synchronized with the overall Ciao system
;;       versions, in file CiaoMode.pl (included on installation 
;;       with this file).
;;---------------------------------------------------------------------------

;; Do not change the two lines below (Patched by installation!):
(defconst ciao-mode-version "1.14.2"
  "This is the version number of the ciao.el file")

(defconst ciao-mode-emacs-version 

  "This mode is currently being developed within @apl{GNU emacs}
version 21.3. It should also (hopefully) work with all other 21.XX,
20.XX, and later 19.XX versions. We also try our best to keep things
working under @apl{xemacs} and under the diverse emacs native ports
for the mac."

  "This is a comment describing for which emacs version this ciao.el
   file has been developed.")

;; ---------------------------------------------------------------------------
;; Basic Ciao mode variables
;; ---------------------------------------------------------------------------

(defgroup ciao-environment nil
  "The Ciao system programming environment, including Ciao, CiaoPP and LPdoc." 
  :tag "Ciao"
  :group 'emacs)

(defgroup ciao-environment nil
  "The Ciao system programming environment, including Ciao, CiaoPP and LPdoc." 
  :tag "Ciao"
  :group 'languages)

(defgroup ciao nil
  "The Ciao system."
  :tag "Ciao"
  :group 'ciao-environment)

(defgroup ciaopp nil
  "The Ciao preprocesor."
  :tag "CiaoPP"
  :group 'ciao-environment)

(defgroup lpdoc nil
  "The LPdoc documentation generator."
  :tag "LPdoc"
  :group 'ciao-environment)

;; General faces group
(defgroup ciao-highlighting-faces nil
  "Ciao environment faces for syntax highlighting, debugger, etc."
  :tag "Ciao Faces" :group 'ciao-environment)

(defcustom ciao-faces-use-variable-pitch-in-comments nil
  "Controls whether variable pitch fonts are used when highlighting
comments. Unset by default. After changing this you must exit and
reinitialize for the change to take effect."
  :group 'ciao-highlighting-faces
  :type 'boolean)

(defcustom ciao-system (or (getenv "CIAO") (concat ciao-bin-dir "/ciao-1.14"))
  "Name of Ciao executable which runs the classical top level."
  :group 'ciao
  :type 'string)

(defun ciao-set-ciao-system () 
  "Change the Ciao executable used to run the top level. It is set by
default to @tt{ciao} or, to the environment variable @tt{CIAO} if it
is defined. @cindex{toplevel command, setting}" 
  (interactive)
  (setq ciao-system
	(read-file-name "Change Ciao top-level executable ?" 
			"" ciao-system nil ciao-system)))

(defcustom ciao-system-args (or (getenv "CIAOARGS") "")
  "Arguments passed to Ciao toplevel executable."
  :group 'ciao
  :type 'string)

(defun ciao-set-ciao-system-args () 
  "Change the arguments passed to the Ciao executable. They are
set by default to none or, to the environment variable @tt{CIAOARGS} if it
is defined. @cindex{toplevel command args, setting}"
  (interactive)
  (setq ciao-system-args
	(read-file-name "Change args passed to Ciao executable ?" 
			"" ciao-system-args nil ciao-system-args)))

;; 'ignore' is because custom passes id of symbol
(defun ciao-do-set-library-path (ignore ciaolib) 
  (if (string= ciaolib "") 
      (progn
	(setenv "CIAOLIB" nil)
	(setq ciao-library-path ""))
    (setenv "CIAOLIB" ciaolib)
    (setq ciao-library-path ciaolib)))

(defun ciao-initialize-library-path (ignorea ignoreb) 
  (ciao-do-set-library-path nil (or (getenv "CIAOLIB") "")))

(defcustom ciao-library-path ""
  "Path to the Ciao System libraries (reads/sets the CIAOLIB
environment variable ). Typically left empty, since ciao
executables know which library to use."
  :group 'ciao
  :type 'string
  :initialize 'ciao-initialize-library-path
  :set 'ciao-do-set-library-path
  )

(defun ciao-set-library-path () 
  "Change the location of the Ciao library paths (changes the
   environment variable @tt{CIAOLIB})."
  (interactive)
  (ciao-do-set-library-path nil
   (read-file-name "Change Ciao library path ?" 
		   "" (getenv "CIAOLIB") nil (getenv "CIAOLIB"))))

(defcustom ciao-locate-errors-after-run t
  "If set, location of any errors produced when running Ciao tools
(loading or preprocessing code, running the documenter, etc.) will be
initiated automatically. I.e., after running a command, the system
will automatically highlight any error messages and the corresponding
areas in source files if possible. If set to nil this location will
only happen after typing \\<ciao-mode-map>
\\[ciao-find-last-run-errors] or accessing the corresponding menu or
tool bar button."
  :group 'ciao
  :type 'boolean)

(defcustom ciao-locate-also-note-messages nil
  "If set, also when errors of type NOTE are detected the
corresponding file is visited and the location marked. It is set to
nil by default because sometimes the user prefers not to take any
action with respect to these messages (for example, many come from the
documenter, indicating that adding certain declarations the
documentation would be improved)."
  :group 'ciao
  :type 'boolean)

(defcustom ciao-inhibit-toolbar nil
  "*Non-nil means don't use the specialized Ciao toolbar."
  :type 'boolean
  :group 'ciao)

;; Prompt patterns -- left some out of custom because you need to be
;;                    really careful when changing these...

(defvar ciao-prompt "?-" 
  "Ciao prompt (simplest)")

(defvar ciao-ciaopp-prompt "ciaopp ?-" 
  "CiaoPP prompt (simplest)")

(defvar ciao-prompt-pattern "\n\\?- " 
  "Matching the Ciao prompt")

(defvar ciao-ciaopp-prompt-pattern "\nciaopp \\?- " 
  "Matching the CiaoPP prompt")

(defcustom ciao-os-shell-prompt-pattern "\\[[0-9]+\\]> "
  "Regular expression used to describe the shell prompt pattern, so
that error location works in inferior shells. This is useful for
example so that errors are located when generating documentation, and
also when using the embedded debugger or any other application in a
shell. It is best to be as precise as possible when defining this so
that the standard Ciao error location does not get confused."
  :group 'ciao
  :type 'string)

(defvar ciao-any-prompt-pattern (concat 
  "\\("
  "^\\(\\(\\|[0-9]+ \\|ciaopp \\|lpdoc \\|| \\)\\?-\\)" 
  "\\|" 
  ciao-os-shell-prompt-pattern 
  "\\)")
  "Matching any Ciao or other inferior process prompt")
; "\\(^|* *\\?- *\\)\\|\\(^ciaopp \\?- *\\)") Old Ciao/SICStus prompt patterns

;; Note this one is a function
(defun ciao-error-or-prompt-pattern ()
  (concat 
  "\\("				      
  (if ciao-locate-also-note-messages
      "^\\({?WARNING.*:\\|{?ERROR.*:\\|{?NOTE.*:\\)"
    "^\\({?WARNING.*:\\|{?ERROR.*:\\)")
  "\\|"
  ciao-any-prompt-pattern
  "\\)"))

;; Set to ciao-os-shell-prompt-pattern:
(defvar ciao-lpdoc-prompt-pattern ciao-any-prompt-pattern
  "Matching the lpdoc prompt")

;; *** For furture lpdoc version 2.0 (with top level):
;; (defvar ciao-lpdoc-prompt-pattern "\n\\lpdoc ?- " 
;;   "Matching the lpdoc prompt")

;; End of prompt patterns --

(defcustom ciao-user-directives '( "mydirective" )
  "List of identifiers of any directives defined by users which you
would like highlighted (colored). Be careful, since wrong entries may
affect other syntax highlighting."
  :group 'ciao
  :type 'list)

;; Problem is, environment usually opened with -q -> needs a special 
;; config file for this case...
;; (i.e., typically by double-clicking on an icon) 
(defcustom ciao-create-sample-file-on-startup t
  "When starting the Ciao environment using ciao-startup two buffers
are opened: one with a toplevel and another with a sample file. This
toggle controls whether the sample file, meant for novice users, is
created or not. Set by default, non-novice users will probably want to
turn it off."
  :group 'ciao
  :type 'boolean)

(defcustom ciao-toplevel-buffer-name "Ciao"
  "Basic name of the buffer running the Ciao toplevel inferior process."
  :group 'ciao
  :type 'string)

(defcustom ciao-first-indent-width 8 ;; some people prefer 4
  "First level indentation for a new goal."
  :group 'ciao
  :type 'integer) ;; it was 'tab-width' before

(defcustom ciao-indent-width 4
  "Indentation for a new goal."
  :group 'ciao
  :type 'integer)

(defvar ciao-temp-file-name "ciao")
(defvar ciao-last-temp-file nil)

(defvar ciao-previous-error nil
  "Stores where the last error was.")

(defvar ciao-inferior-error nil
  "Stores the line in the inferior buffer which shows the error line.")

(defvar ciao-finding-errors nil
  "Non nil if we are in the middle of the process of finding errors.
   In that case it contains the original buffer to return to in the end.")

(defvar ciao-last-process-buffer-used nil
  "Contains which is the last process buffer (preprocessor, toplevel,
   ...) used.")

(defvar ciao-last-source-buffer-used nil
  "Used to contain sometimes the last source buffer used (useful for
returning to it after processing).")

(defcustom ciao-main-filename ""
  "Name of main file in a multiple module program. Setting thsi is
very useful when working on a multi-module program because it allows 
issuing a load command after working on an inferior module which will 
reload from the main module, thus also reloading automatically all
dependent modules." 
  :group 'ciao
  :type 'string)

(defun ciao-set-main-filename ()
  "Set the current buffer as the principal file in a multiple module
programming environment."
  (interactive)
  (setq ciao-main-filename
	(read-file-name "Change Ciao main file? " 
			"" (buffer-file-name) t (buffer-file-name)))
  (ciao-send-command ciao-ciaopp-buffer-name 
       (concat "set_prolog_flag(main_module,\'" ciao-main-filename "\').") t))
;;   (setq ciao-main-filename 
;; 	(read-file-name "Change Ciao main module? " 
;; 			"" (ciao-get-module-name) nil (ciao-get-module-name))))

(defvar ciao-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the Ciao buffer.")

(defvar ciao-prompt-inferior-hook nil
  "Things to do in Ciao once the prompt is found in the Ciao buffer.")

(defvar ciao-prompt-marker-acc ""
  "Keep the last line written in the Ciao inferior buffer. It is used
to search for the prompt since the prompt should be after a newline.")
(make-variable-buffer-local 'ciao-prompt-marker-acc)

(defcustom ciao-query ""
  "Query to use in Ciao. Setting this is useful when using a long or
complicated query because it saves from having to type it over and
over again. It is possible to set that this query will be issued 
any time a program is (re)loaded."
  :group 'ciao
  :type 'string)

(defun ciao-set-query ()
  "Set a default query. This may be useful specially during debugging
sessions. However, as mentioned elsewhere, note that commands that
repeat previous queries are also available. 

This query can be recalled at any time using \\<ciao-mode-map>
\\[ciao-load-query].  It is also possible to set things up so that
this query will be issued automatically any time a program is
(re)loaded. The functionality is available in the major mode (i.e.,
from a buffer containing a source file) and in the inferior mode
(i.e., from the buffer running the top-level shell). When called from
the major mode (i.e., from window containing a source file) then the
user is prompted in the minibuffer for the query. When called from the
inferior mode
(i.e., from a top-level window) then the query on the current line,
following the Ciao prompt, is taken as the default query.

To clear the default query use \\<ciao-mode-map> \\[ciao-clear-query]
or simply set it to an empty query: i.e., in a source buffer select
\\[ciao-set-query] and enter an empty query. In an inferior mode
simply select \\[ciao-set-query] on a line that contains only the
system prompt."


  (interactive)
  (let (beg query)
    (cond ((string= (buffer-name) (concat "*" ciao-toplevel-buffer-name "*"))
	   (save-excursion
             ;; MH This approach does not work in 21.1
             ;; (beginning-of-line) 
             (if (not (search-backward-regexp ciao-prompt-pattern nil t))
                 (setq ciao-query "")
               (goto-char (match-end 0))
	       (setq beg (point))
	       (end-of-line)
	       (setq ciao-query 
		    (buffer-substring-no-properties beg (point))))))
	  ((eq major-mode 'ciao-mode)
	   (setq query (read-string "Set default query to: " ciao-query))
	   (setq ciao-query query)
	   )))
  (if (string= ciao-query "")
      (message "Default query cleared")
    (message (concat "Default query set to: '" ciao-query "'" ))))

;; MCarlos: In xemacs function match-string-no-properties does not exist.
;; This will fix that, but when using ciao-match-string you
;; should use (funcall ciao-match-string <args>)
(defvar ciao-match-string nil)
(if (not (boundp 'xemacs-logo))
    (setq ciao-match-string 'match-string-no-properties)
  (setq ciao-match-string 'match-string))

(defun ciao-clear-query ()
  "Clear the default query."
  (interactive)
  (setq ciao-query "")
  (message "Default query cleared"))

;; ---------------------------------------------------------------------------
;; Source debugger variables
;; ---------------------------------------------------------------------------

;; CHANGE
(defvar ciao-debug-filter-defer-flag nil
  "Non-nil means don't process anything form the debugger 
right now. It is saved for when flag is not set.")

(defvar ciao-debug-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in
'ciao-debug-filter'.")

(defvar ciao-debug-delete-prompt-marker nil)

(defvar ciao-debug-last-frame nil 
  "Last file over which we have drawn.")

(defvar ciao-debug-last-line nil 
  "Temporary storage of last line (coloring). Con pair buffer, line number.")

(defvar ciao-debug-marker-acc ""
  "Text to search for ciao-debug-marker-prompt.")
(make-variable-buffer-local 'ciao-debug-marker-acc)

(defvar ciao-debug-marker-regexp nil
  "Regular expression for looking for file position info.")
(setq ciao-debug-marker-regexp
      (concat 
	      "         In "
	      "\\(.*\\)"         ; Src file
	      " ("
	      "\\([0-9]+\\)"     ; Start line
	      "-"
	      "\\([0-9]+\\)"     ; End line
              ") "
	      "\\(.*\\)"         ; Pred name
	      "-"
	      "\\([0-9]+\\)\n"    ;)) ; n-th pred
	      ".*[\*0-9]+  [\*0-9]+"
	      "  \\([CERF][a-z]+\\):.* ? "))

(defcustom ciao-logo "ciao.xpm"
  "Ciao logo image."
  :group 'ciao
  :type 'file)

(defcustom java-logo "java-logo.gif"
  "Java logo image."
  :group 'ciao
  :type 'file)

(defcustom ciao-clip-logo "clip.xpm"
  "CLIP logo image."
  :group 'ciao
  :type 'file)

;; ---------------------------------------------------------------------------
;; CiaoPP variables
;; ---------------------------------------------------------------------------
;; (defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp-1.0")
(defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp")
  "Name of Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system () 
  "Change the executable used to run the Ciao Preprocessor
toplevel. It is set by default to @tt{ciaopp} or, to the environment 
variable @tt{CIAOPP} if it is defined. @cindex{preprocessor command, setting}"
  (interactive)
  (setq ciao-ciaopp-system
	(read-file-name "Change Ciao preprocessor executable ?"
   		        "" ciao-ciaopp-system nil ciao-ciaopp-system))) 

(defcustom ciao-ciaopp-system-args (or (getenv "CIAOPPARGS") "")
  "Arguments passed to Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system-args () 
  "Change the arguments passed to the Ciao preprocessor executable. They are
set by default to none or to the environment variable @tt{CIAOPPARGS} if it
is defined. @cindex{preprocessor command args, setting}"
  (interactive)
  (setq ciao-ciaopp-system-args
	(read-file-name "Change args passed to Ciao preprocessor executable ?"
	        "" ciao-ciaopp-system-args nil ciao-ciaopp-system-args))) 

(defcustom ciao-ciaopp-buffer-name "Ciao-Preprocessor"
  "Basic name of the buffer running the Ciao preprocessor inferior process."
  :group 'ciaopp
  :type 'string) 

(defcustom ciao-ciaopp-gmenu-buffer-name "CiaoPP Interface"
  "Basic name of the buffer running the Ciao preprocessor graphical
menu interface." 
  :group 'ciaopp
  :type 'string) 

(defvar ciao-ciaopp-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the CiaoPP buffer.")

(defvar ciao-ciaopp-prompt-inferior-hook nil
  "Things to do in CiaoPP once the prompt is found in the CiaoPP buffer.")

(defvar ciao-ciaopp-prompt-marker-acc ""
  "Keep the last line written in the CiaoPP inferior buffer. It is
used to search for the prompt since the prompt should be after a
newline.")
(make-variable-buffer-local 'ciao-ciaopp-prompt-marker-acc)

;; DTM: Added this new "hook"
(defvar ciao-ciaopp-gmenu-hook nil
  "Things to do in CiaoPP once a ciaopp menu string is found in the
CiaoPP buffer.")

(defcustom ciao-ciaopp-use-graphical-menu t
  "If set, an interactive graphical menu is used for controlling
CiaoPP, instead of asking ascii questions in the CiaoPP buffer."
  :group 'ciaopp
  :type 'boolean)

(defvar ciao-ciaopp-prog-lang nil
  "Whether the program loaded is a ciao (0) or a Java (1) program." )


;; ---------------------------------------------------------------------------
;; LPdoc variables
;; ---------------------------------------------------------------------------

;; "gmake" for lpdoc-1.9; "lpdoc" for 2.0+
(defcustom ciao-lpdoc-system (or (getenv "LPDOC") "lpdoc-3.0")
  "Name of LPdoc auto-documenter executable."
  :group 'lpdoc
  :type 'string)

(defun ciao-set-lpdoc-system () 
  "Change the executable used to run the LPdoc auto-documenter. It is
set by default to @tt{lpdoc} or to the environment  
variable @tt{LPDOC} if it is defined. @cindex{lpdoc command, setting}
@cindex{auto-documenter command, setting}"
  (interactive)
  (setq ciao-lpdoc-system
	(read-file-name "Change Ciao LPdoc auto-documenter executable ?"
   		        "" ciao-lpdoc-system nil ciao-lpdoc-system))) 

(defcustom ciao-lpdoc-system-args (or (getenv "LPDOCARGS") "")
  "Arguments passed to LPdoc executable."
  :group 'lpdoc
  :type 'string)

(defun ciao-set-lpdoc-system-args () 
  "Change the arguments passed to the LPdoc auto-documenter. They are
set by default to none or to the environment variable @tt{LPDOCARGS} if it
is defined. @cindex{lpdoc command args, setting}
@cindex{auto-documenter command args, setting}" 
  (interactive)
  (setq ciao-lpdoc-system-args
     (read-file-name "Change args passed to LPdoc auto documenter executable ?"
	        "" ciao-lpdoc-system-args nil ciao-lpdoc-system-args))) 

(defvar ciao-lpdoc-buffer-tmpdir-list nil
  "Assoc. list relating filenames and their temporary doc dirs.")

(defcustom ciao-lpdoc-wdir-root (or (getenv "LPDOCWDIR") "/tmp")
  "Name of root working dir used by LPdoc."
  :group 'lpdoc
  :type 'directory)

(defun ciao-set-lpdoc-wdir-root () 
  "Change the root working dir used by the LPdoc auto-documenter. It is
set by default to a new dir under @tt{/tmp} or to the environment  
variable @tt{LPDOCWDIR} if it is defined. @cindex{lpdoc working dir, setting}
@cindex{auto-documenter working dir, setting}"
  (interactive)
  (setq ciao-lpdoc-wdir-root
     (read-file-name "Change root working dir used by LPdoc auto-documenter ?"
   		        "" ciao-lpdoc-wdir-root nil ciao-lpdoc-wdir-root))) 

(defcustom ciao-lpdoc-docformat (or (getenv "LPDOCFORMAT") "html")
  "Name of default output format used by LPdoc."
  :group 'lpdoc
  :type '(choice (const "dvi")
		 (const "ps")
		 (const "pdf")
		 (const "html")
		 (const "info")
		 (const "man")))

(defun ciao-set-lpdoc-docformat () 
  "Change the default output format used by the LPdoc auto-documenter. It
is set by default to @tt{html} or to the environment variable
@tt{LPDOCFORMAT} if it is defined. @cindex{lpdoc default format, setting}
@cindex{auto-documenter default format, setting}"
  (interactive)
  (setq ciao-lpdoc-docformat
    (read-string "Change default doc format used by LPdoc auto-documenter ?"
		 ciao-lpdoc-docformat)))

(defcustom ciao-lpdoc-libpath (or (getenv "LPDOCLIB") "~/CiaoDE/lib/lpdoc/lpdoc-3.0")
  "Path in which the LPdoc library is installed."
  :group 'lpdoc
  :type 'directory)

(defun ciao-set-lpdoc-libpath () 
  "Change the path in which the LPdoc library is installed. It is
set by default to @tt{/home/clip/lib} or to the environment  
variable @tt{LPDOCLIB} if it is defined. @cindex{lpdoc lib path, setting}
@cindex{auto-documenter lib path, setting}"
  (interactive)
  (setq ciao-lpdoc-libpath
	(read-file-name "Change path in which LPdoc lib is installed ?"
   		        "" ciao-lpdoc-libpath nil ciao-lpdoc-libpath))) 

(defcustom ciao-ask-for-version-maintenance-type "no"
  "If turned to yes the system asks prompts to set version control
when saving files that do not set a version control system
explicitly within the file."
  :group 'lpdoc
  :type 'string) 

(defcustom ciao-lpdoc-buffer-name "LPdoc"
  "Basic name of the buffer running the auto-documenter inferior process."
  :group 'lpdoc
  :type 'string) 

(defvar ciao-lpdoc-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the LPdoc buffer.")

(defvar ciao-lpdoc-prompt-inferior-hook nil
  "Things to do in LPdoc once the prompt is found in the LPdoc buffer.")

(defvar ciao-lpdoc-prompt-marker-acc ""
  "Keep the last line written in LPdoc inferior buffer. It is used to
search for the prompt since prompt should be after a newline.")
(make-variable-buffer-local 'ciao-lpdoc-prompt-marker-acc)

(defvar update-version-comments 0 ; 0 means "uninitialized"
  "Controls for each buffer whether version maintenance is performed
or not and if it is, what type.")
(make-variable-buffer-local 'update-version-comments)

;; ===========================================================================
;; Mode body
;; ===========================================================================

(defun exe-buffer-name (str)
  (concat "*" str "*"))

;; List of manuals for word-help (help on symbol under cursor). These
;; info files must be accessible from the paths in the environment
;; variable 'INFOPATH' (or in the emacs variable Info-default-directory-list).
;; 
;; Unfortunately, this is very brittle... (Stallman seems to have
;; plans for a better word-help in new versions of emacs)
(setq word-help-mode-alist
 (cons
  '("Ciao"
    (
     ;; Indices currently in manuals: concept lib pred prop regtype decl usage
     ;; Later entries take precedece, so this is probably the right order!
     ("ciao-1.14.2.info" 
      "Global Index"
      "Author Index"
      "Concept Index" 
      "Library/Module Index" 
      "Predicate/Method Index" 
      "Property Index" 
      "Regular Type Index" 
      "Declaration Index" 
      ) 
     ; ("sicstus3" 
     ;  "Predicate Index" "Obj Index" "Concept Index")
     )
    (("[A-Za-z_]+" 0)
     ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
    nil
    (("[A-Za-z_]+" 0))
    )
 (cons
  '("Ciao/CiaoPP/LPdoc Listener"
    (
     ;; Indices currently in manuals: concept lib pred prop regtype decl usage
     ;; Later entries take precedece, so this is the right order!
     ("ciao-1.14.2.info"
      "Global Index"
      "Author Index"
      "Concept Index" 
      "Library/Module Index" 
      "Predicate/Method Index" 
      "Property Index" 
      "Regular Type Index" 
      "Declaration Index" 
      ) 
     )
    (("[A-Za-z_]+" 0)
     ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
    nil
    (("[A-Za-z_]+" 0))
    )
  word-help-mode-alist)))

;; MH Changed to do it in current dir (so that slaves can see it, etc.!)
(defvar ciao-temp-file-counter 0)

(defun ciao-last-temp-code-file ()
  "Returns the name of a the last created temporary file in the
current dir (or creates one)."
  (if (eq ciao-last-temp-file nil)
      (setq ciao-last-temp-file (ciao-temp-code-file "."))
    ciao-last-temp-file))

(defun ciao-temp-code-file (from-dir)
  "Returns the name of a temporary file in dir given in argument."
  (concat (expand-file-name (concat from-dir "/")) 
	  ciao-temp-file-name 
	  (int-to-string ciao-temp-file-counter) "_" (make-temp-name "")))

;; Not really needed?
(defun ciao-new-temp-code-file (from-dir)
  "Builds new temporary file names in the current dir."
  (setq ciao-temp-file-counter (+ ciao-temp-file-counter 1))
  (ciao-temp-code-file from-dir))

(defun ciao-new-temp-code-dir (filename)
  "Builds new temporary dir names in lpdoc root dir."
  (setq ciao-temp-file-counter (+ ciao-temp-file-counter 1))
  (concat (expand-file-name ciao-lpdoc-wdir-root) "/lpdoc_" filename "_"
	  (int-to-string ciao-temp-file-counter) "_" (make-temp-name "")))

(defvar ciao-objects-lib-loaded nil "Stores whether objects library
has been loaded or not (see ciao-load-command).")

(defvar ciao-assrt-lib-loaded nil "Stores whether assertion library
has been loaded or not (see ciao-check-buffer-syntax).")

;;----------------------------------------------------------------------------
;; Font-lock support - regular expressions and matching
;;----------------------------------------------------------------------------

;; Just a bridge (for documentation and setting local binding)
;; but better than font-lock-fontify-buffer
(defun ciao-fontify-buffer ()
  "Undate (recompute) syntax-based highlighting (coloring)."
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defvar ciao-predicate-directives
  '( "data" "dynamic" "multifile" "impl_defined" "meta_predicate"
     "discontiguous" "persistent"
     ;; jfmc: moved as predicate directives
     "export" "redefining" 
     ;; jfmc: new OO-module system
     "public" "constructor" "constant" "static" "attr" "mut" "fluid" "virtual"
     )
  "Names of directives describing properties of predicates.")

(defvar ciao-module-directives
  '( "module" "package"
     "use_module" "ensure_loaded" "use_active_module"
     "use_package" "include" "use_foreign_library" "use_foreign_source"
     "reexport" "import"
     "initialization" "on_abort"
     )
  "Names of module directives.")
(defvar ciao-module-simple-directives
  '( ;; TODO: Those should be module directives
     ;; jfmc: new OO-module system
     "extends"
     )
  "Names of simple module directives.")

(defvar ciao-module-block-directives
  '( "class" "interface" "mixin"
     )
  "Names of module directives that may contain curly braces.")

(defvar ciao-builtin-directives
  '( "new_declaration" "op" 
     "load_compilation_module" "add_sentence_trans" "add_term_trans"
     "add_clause_trans" "add_goal_trans"
     "set_prolog_flag" "push_prolog_flag" "pop_prolog_flag" 
     ;; Resources and Granularity
     "compound_resource" "platform_constants" "platform"
     "load_resource_module" "resource" "head_cost" "literal_cost"
     "trust_default" "granularity_resources"
     ;; Unit-Testing
     "load_test_module"
     "load_test_package"
     ;; Foreign-interface
     "extra_compiler_opts" "extra_linker_opts"
     )
  "Names of other directives.")

(defvar ciao-condcode-directives
  '( 
     ;; jfmc: new OO-module system (conditional code)
     "if" "else" "elif" "endif"
     )
  "Names of directives to define conditional code.")

(defvar ciao-library-directives
  '( 
;; functions
     "fun_eval" "fun_return" "lazy" "funct" 
     ;; backwards compatibility:
     "function"
;; argnames
     "argnames" 
;; make
     "make" 
;; ociao
     ;; jfmc: some commented to avoid conflicts with new OO-module system
     ; "public" "class" "virtual"
     "inheritable" "implements" "inherit_class"
     "use_class"
     )
  "Names of additional directives defined in the libraries.")
 
;; Also, 'ciao-user-directives' now customizable; see above in file.


;; KNOWN FEATURES:
;;
;; atom with % => Decided to sacrify this. It is _very hard_ to
;; define a grammar in which comments and language grammar is included
;; (the way to do that is to "pre-propocess" the text.  In summary:
;; 'asdfsdf % asdfasdf' => is considered like: 'asdfsdf COMMENT, so no
;; right part is not highlighted.
;;
;; Atom that ends in 0:
;; 'antom0'.
;; 0'. is consider a character instead of an atom. This is important
;; in order not to break the highlighting syntax. Example:
;; X = 0'a,
;; Y = 0'b.
;; In this case:
;;      'a,
;; Y = 0'  would be considered an atom if 0' are not treated first.
;; But: X = 0'a,X = 0'a,X = 0'a (all text would be considered an atom).
;;
;; Comments in the middle of assertions that contain a period:
;; :- true pred l(X) %well...
;;    : list( X ) "bla bla bla".
;; A preprocessor would remove the comment and it would work, but
;; as we cannot "remove" it, dot is consider the end of the assertion :(
;;
;; Other things (those that are not written in good "Prolog programming
;; style"):
;; X = 
;; 'This is my atom'.
;; 
;; Order is forwards. Basically font lock works like this:
;; Once an expresion is matched, it is highligthed. This cannot be
;; "rehighlithed" unless rewrite or keep is specified (unlike what
;; happened with the hilit package, which has resulted in many changes).
(defvar ciao-mode-font-lock-keywords 
  `(
    ;; comments /* */
    ((lambda (limit) 
       (ciao-font-lock-match limit "/\\*" "\\*/"))
     . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-comment-variable-pitch
	'ciao-face-comment))
    ;; comments
    ("%.*% TODO:.*$" . ciao-face-comment)
    ("% TODO:.*$" . ciao-face-lpdoc-bug-comment)
    ("%.*$" . ciao-face-comment)
    ;; scripts
    ((lambda (limit) 
       (ciao-font-lock-match limit "^#!" "^[ \t]*$"))
     . ciao-face-script-header)

    ;; lpdoc comment strings in assertions
;    ("#[^\\\"]*\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 
    ("#[ \n\t]*\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 
     1 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)
    ;; lpdoc (bug) comments
    ((lambda (limit)
       (ciao-font-lock-match limit
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*bug\\>"
                     "[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 ciao-face-lpdoc-bug-comment keep)
    ;; lpdoc title comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*title[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-2-face keep)
    ;; lpdoc section comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*section[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-3-face keep)
    ;; lpdoc subsection comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*subsection[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-4-face keep)
    ;; other lpdoc comments
    ;; "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version\\(_maintenance\\)?\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>" 
    ;; These ones have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)("
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was: [ \t]*$
     0
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)
    ;; These ones do not have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version_maintenance\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>"
	"[ \t\n]*)[ \t\n]*\\.")) ; was: [ \t]*$
     0
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)

    ;; Trick (ciao-font-lock-match-in-comment) to color LPdoc commands
    ;; only if they have already been colored as lpdoc comments (JFMC)
    ;;
    ;; The face is overridden by using the t parameter (override)
    ((lambda (limit) 
       (ciao-font-lock-match-in-comment limit "@begin{verbatim}" "@end{verbatim}"))
     0 ciao-face-lpdoc-verbatim t)

    ((lambda (limit) 
       (ciao-font-lock-match-in-comment limit "@include[^ {}@]*{" "[^}@]*}"))
     0 ciao-face-lpdoc-include t)

    ((lambda (limit)
       (ciao-font-lock-simple-match-in-comment
	limit
	"@\\([}{@]\\|\\([A-Za-z]+\\|[?!]\\)[ \t\n]\\)"))
     0 ciao-face-lpdoc-command t)

    ((lambda (limit)
       (ciao-font-lock-simple-match-in-comment
	limit
	"@[^ \t\n{}@=<>]*{[^{}@]*}"))
     0 ciao-face-lpdoc-command t)

    ;; quoted atoms
    ("\\('\\(\\\\\\\\\\|\\\\'\\|[^'\n]\\)*'\\)" 0 ciao-face-quoted-atom)
    ;; Strings
;;    ("^[^']*\\(\"\\([^\\\"]\\|\"\"\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 1 ciao-face-string)
    ("\\(\"\\([^\\\"]\\|\"\"\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 0 ciao-face-string)

    ;; funexp atoms (jfmc)
    ("\\(~[a-z][A-Za-z0-9_\\.^/]*[A-Za-z0-9_^/]\\|~[a-z]\\)" 0 ciao-face-funexp-atom)

    ;; Characters 0'...
    ("0'\\(\\\\.\\|.\\)" 0 ciao-face-string)

    ("@\\(cite\\|ref\\|section\\|subsection\\){[^{}@]*}"
     0 ciao-face-lpdoc-crossref)

    ;; Directives (keep => They can have a comment)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit    
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-predicate-directives t) "\\>")
        "\\.")) ; was: ^[ \t]*$\\|\\.$
     0 ciao-face-predicate-directive keep)
    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-builtin-directives t) "\\>")
        ")[ \t]*\\.")) ; was: "^[ \t]*$\\|\\."
     0 ciao-face-builtin-directive keep)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-module-directives t) "\\>")
;	")[ \t]*\\.")) ; was: [ \t]*
	")[ \t]*\\."))
     0 ciao-face-module-directive keep)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-module-simple-directives t) "\\>")
;	")[ \t]*\\.")) ; was: [ \t]*
	"[ \t]*\\."))
     0 ciao-face-module-directive keep)
    ;; jfmc: new OO-module system
    ((lambda (limit) 
       (ciao-font-lock-match-block-directive limit))
     0 ciao-face-module-directive keep)
    ;; jfmc: new OO-module system (conditional code)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-condcode-directives t) "\\>")
	"\\."))
     0 ciao-face-condcode-directive keep)

    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-library-directives t) "\\>")
        "^[ \t]*$\\|\\.")) ; was: "^[ \t]*$\\|\\.$"
     0 ciao-face-library-directive keep)

    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" (regexp-opt ciao-user-directives t) "\\>")
        "^[ \t]*$\\|\\.")) ; was: .$
     0 ciao-face-user-directive keep)
    ;; --- add whatever is like :- I dont know bla bla bla bla.

    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "\\<debug_message("))
     0 ciao-face-debug-mess)

    ;; LPdoc comments
    ;; lpdoc version comments (and other related directives)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*version(" 
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 ciao-face-lpdoc-version-comment keep)

    ;; Assertions Variables
    ;; ("^[ \t]*:-[^(]*\\<\\([A-Z_][a-zA-Z0-9_]*\\)" 1 ciao-face-assrt-variable)

    ;; Assertions
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "checked") (ciao-end-assrt-regexp)))
     0 ciao-face-checked-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "true") (ciao-end-assrt-regexp)))
     0 ciao-face-true-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "false") (ciao-end-assrt-regexp)))
     0 ciao-face-false-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "trust") (ciao-end-assrt-regexp)))
     0 ciao-face-trust-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "check") (ciao-end-assrt-regexp)))
     0 ciao-face-check-assrt keep)

    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*true("))
     0 ciao-face-true-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*false("))
     0 ciao-face-false-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*trust("))
     0 ciao-face-trust-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*check("))
     0 ciao-face-check-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*checked("))
     0 ciao-face-checked-assrt keep)

    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp 
	       (regexp-opt '("decl" "pred" "comp" "calls" 
			     "success" "test" "texec") t)
	       )
	(ciao-end-assrt-regexp)))
     0 ciao-face-check-assrt keep)

    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "prop") (ciao-end-assrt-regexp)))
     0 ciao-face-prop-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "test") (ciao-end-assrt-regexp)))
     0 ciao-face-test-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "texec") (ciao-end-assrt-regexp)))
     0 ciao-face-texec-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "regtype") (ciao-end-assrt-regexp)))
     0 ciao-face-type-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "hmtype") (ciao-end-assrt-regexp)))
     0 ciao-face-type-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "entry") (ciao-end-assrt-regexp)))
     0 ciao-face-entry-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "modedef") (ciao-end-assrt-regexp)))
     0 ciao-face-modedef-assrt keep)

    ;; Variables
    ("\\<\\([A-Z_][a-zA-Z0-9_]*\\)" 1 ciao-face-variable)
    ;; Concurrency ops
    ("\\([ \t]&&\\|[ \t]&>\\|[ \t]<&\\|[ \t]&\\|[ \t]@[ \t]\\)"
    ;; ("\\(&\\|&>\\|<&\\|@[^=<>]\\)"
     . ciao-face-concurrency-op) 
    ;; Cut
    ("!" . ciao-face-cut)
    ;; Declaration neck (somewhat of a warning --recognized ones
    ;; colored above)
    ("^[ \t]*:-" . ciao-face-lpdoc-bug-comment)
    ;; Necks.
    ("\\(:-\\|-->\\|:=\\)" . ciao-face-prompt)
    ;; Other major connectors, etc.
    ;; jfmc: disabled, coloring operators along with variables and
    ;;       funexps was distracting
;    ("\\(|\\|?\\|->\\|~\\)" . ciao-face-quoted-atom)
    ;; jfmc: all symbols
;    ("[^'a-zA-Z0-9,()_ \t\n]" . ciao-face-quoted-atom)
    ;; jfmc: all atoms
;    ("[a-zA-Z0-9_]+" . ciao-face-quoted-atom)

    ;; Clause heads
    ; this consider atoms _without_ spaces and normal names as a
    ; clause name
;    ("^\\([a-z'][^(\\.: ]*\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)"
;      1 ciao-face-clauseheadname t)
;;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)
;;    ("^\\('\\(\\\\'\\|[^']\\)*'\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)" 
;;    0 ciao-face-clauseheadname)
;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)
    ;; jfmc: tricky match fo clause head name
    ((lambda (limit)
       (ciao-font-lock-match-clauseheadname
	limit))
     0 ciao-face-clauseheadname keep)

    ;; This is for debugging... if patata appears in green it means
    ;; all previous rules are understood by emacs (or XEmacs)
    ;; Emacs
    ;; ("patata" 0 patata_var t)
    ;; XEmacs

    ;; ("patata" 0 ciao-face-true-assrt t)
    
    ;; quoted atoms larger than 1 line
    ("\\('\\(\\\\\\\\\\|\\\\'\\|[^']\\)*'\\)" 0 ciao-face-quoted-atom)
    )
  )

(defvar patata_var 'ciao-face-true-assrt)

(defun ciao-begin-assrt-regexp (identifier)
  (concat "^[ \t]*:-[ \t\n]*" identifier "[ \t\n]"))

;older
;"^[ \t]*:-[ \t]*\\(check\\)?[ \t]*\\(decl\\|pred\\|comp\\|calls\\|success\\) "

(defun ciao-end-assrt-regexp ()
  "[^#\\.]*\\(#\\|\\.\\)[ \t]*$")

;  "[^#\\.]*\\(#[ \t\n]*\\|\\.[ \t\n]*$\\)")
;  "[ \t]#[ \t\n]\\|^#[ \t\n]\\|\\.[ \t]*$")

(defun ciao-font-lock-match (limit beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (cdr (match-data)))
	(set-match-data (cons begin end))
	t
      ))))

;; JFMC: like font-lock-match, but skips N elements from the match of
;; beginexp and M elements from the match of endexp
(defun ciao-font-lock-match-inside (limit n m beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (nth n (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (nth m (match-data)))
	(set-match-data (cons begin (cons end nil)))
	t
      ))))

;; JFMC: match only if the text has been colored before as ciao-face-lpdoc-comment
(defun ciao-font-lock-simple-match-in-comment (limit beginexp)
  (if (not (search-forward-regexp beginexp limit t))
      nil
    (eq (get-text-property (car (match-data)) 'face) 'ciao-face-lpdoc-comment)))

(defun ciao-font-lock-match-in-comment (limit beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (cdr (match-data)))
	(set-match-data (cons begin end))
	(eq (get-text-property (car (match-data)) 'face) 'ciao-face-lpdoc-comment)
      ))))

;;; jfmc: old definition
;; (defun ciao-font-lock-match-clauseheadname (limit)
;;  (search-forward-regexp "^[a-z][a-zA-Z0-9_]*" limit t))

;; Matches a clauseheadname
;; jfmc: version that works even if clauses are inner indented
(defun ciao-font-lock-match-clauseheadname (limit)
  (let (begin)
    (if (not (search-forward-regexp "[a-z][a-zA-Z0-9_]*" limit t))
	nil
      (progn
	(setq begin (match-beginning 0))
	(if (save-match-data (save-excursion
			       (progn
				 (goto-char begin)
				 (ciao-scan-backward-clausehead))))
	    t
	  ;; Not found, search again (point is moved)
	  (ciao-font-lock-match-clauseheadname limit)
	  )))))

(defun ciao-scan-backward-clausehead ()
  "Scan characters backwards to determine if we are at a clause head
   position"
  (if (and (> (current-column) 0)
	   (= (skip-chars-backward " \t") 0))
      nil ;; no blank, this cannot be a clause head
    (ciao-scan-backward-clausehead-2)))

(defun ciao-scan-backward-clausehead-2 ()
  "Scan characters backwards to determine if we are at a clause head
   position"
  ;; TODO: missing: beginning of buffer
  (forward-char -1)
  (if (looking-at "\n")
      ;; search in this line
      (progn
	;; skip comments
	;; TODO: incorrectly treats quoted % as comments (jfmc)
	(beginning-of-line)
	;; Find a possible comment
	(re-search-forward "[^%\n]*")
	;; Skip blanks backwards
	(skip-chars-backward " \t")
	;; See again if this can be a valid position for a clausehead
	(ciao-scan-backward-clausehead-2))
    (cond ((looking-at "\\.") t) ;; clause end
	  ((looking-at "{") (ciao-scan-forward-curly-block)) ;; curly brace
	  (t nil)) ;; no clause
    )
  )

(defun ciao-scan-forward-curly-block ()
  "Detect if the curly block contains sentences rather than terms"
  ;; TODO: imprecise, it should parse the tokens (i.e. {foo({bar}).} is not detected
  ;; TODO: {foo.} is not detected
  (re-search-forward "[^\\.}]*")
  (looking-at "\\.")
  )

(defun ciao-font-lock-match-block-directive (limit)
  "Match a directive that may contain a postfix block"
  (if (not (ciao-font-lock-match
	    limit
	    (concat "^[ \t]*:-[ \t\n]*"
		    (regexp-opt ciao-module-block-directives t) "\\>")
	    "{\\|)[ \t]*\\."))
      nil
    ;; We do not want '{' to be colored, move end marker 1 character
    ;; backward if '{' is found.
    (when (save-match-data (save-excursion (forward-char -1) (looking-at "{")))
      (let ((begin (car (match-data)))
	    (end (car (cdr (match-data)))))
	(setq end (copy-marker (- end 1)))
	(set-match-data (cons begin (cons end nil)))
	))
    t
    )
  )

;; Matches corresponding closing delimiter
(defun ciao-font-lock-match-until-matching-sexp (limit beginexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (goto-char (- (car (cdr (match-data))) 1))
      (forward-list)
      (setq end (cons (point) nil))
      (set-match-data (cons begin end))
      t
      )))

(defvar ciao-inferior-font-lock-keywords
      `(
	("^\\([A-Z][a-zA-Z0-9_]*\\) = \\(.*\\)\\(,\\| \\?.*\\)$"
         (1 ciao-face-answer-var)               ;; Answer variable
         (2 ciao-face-answer-val)               ;; Answer value
         (3 ciao-face-prompt)                   ;; Prompt after answer
         )
	("^\\([ \t]+[0-9]+[ \t]+[0-9]+\\)\\(Call:\\).*$"
         (1 ciao-face-debug-redo)               ;; 
         (2 ciao-face-debug-call)               ;; 
         )
	(
	 ,ciao-any-prompt-pattern
	 ;; "^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)"
         . ciao-face-prompt)                    ;; Prompts
	("^yes$" . ciao-face-yes-answer)        ;; Answer
	("^no$" . ciao-face-no-answer)          ;; Answer
;;	("^Select[^:]*:" . ciao-face-ciaopp-option) ;; Preproc prompt
	("\\([A-Z][a-zA-Z \\-]*:\\) *\\(\\[[a-zA-Z0-9, _\t\n]*\\]\\)[ \t\n]*\\(([^)]*)\\)[ \t\n]*\\(\\?\\)"
         (1 ciao-face-ciaopp-option)            ;; Option message
         (2 ciao-face-answer-val)               ;; Option values
         (3 ciao-face-answer-var)               ;; Default
         (4 ciao-face-prompt)                   ;; Prompt
         )
	("^{?ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^{SYNTAX ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^\\*\\* here \\*\\*[ \t]*$" . ciao-face-error-mess)  ;; Error mes
	("^{?WARNING.*$" . ciao-face-warning-mess)  ;; Error messages
	("^{DEBUG.*$" . ciao-face-debug-mess)       ;; Error messages
	("^{?Note:.*$" . ciao-face-note-mess)       ;; Error messages
	("^{NOTE.*$" . ciao-face-note-mess)         ;; Error messages
        ("^\\({.*\\|}\\)" . ciao-face-other-mess)   ;; Error messages
;;        ("^\\*\\*\\* ---------.*\n^\\*\\*\\* .*\n\\*\\*\\* ---------.*$" 
        ("^\\*\\*\\* \\(---------\\|=========\\).*$" 
	 . ciao-face-highlight-code)              ;; LPdoc (1.9) messages
        ("^\\*\\*\\* .*$" . ciao-face-debug-call) ;; LPdoc (1.9) messages
	("^Ciao\\>.*$" . ciao-face-startup-mess);; Startup
        ; Recognizes a date at the end of the line (ciaopp still does it)
	("^(C) .* \\w\\w\\w \\w\\w\\w [1-3]?[0-9]\
 [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z][A-Z] [1-2][0-9][0-9][0-9]$"
        . ciao-face-startup-mess)              ;; Startup, second line
;	("\\(^\\?- *[^{ ]\\|^| \\?- *\\).*\\.[ \t]*\n"
;	 . ciao-face-prompt) ;; Query doesn't work(?)
))

;;----------------------------------------------------------------------------
;; Font-lock support - (customizable) face definitions
;;----------------------------------------------------------------------------
;;  Used to have conceptual faces and then actual faces, but it was a
;;  nightmare to keep compatible between emacs and xemacs. For now,
;;  'key' definitions (the conceptual ones) made actual faces until we
;;  work out a portable fix.

;; Reminder of tty colors:
;; black, red, green, yellow, blue, magenta, cyan, white
;; (tty-color-translate color) approximates the color

;; Debugger
(defgroup ciao-highlighting-faces-debugger nil
  "Ciao faces for debugger."
  :tag "Ciao Debugger Faces" :group 'ciao-highlighting-faces)

;; This super-kludge of adding the unnecessary defvar is needed to 
(defvar ciao-face-debug-call 'ciao-face-debug-call)
(defface ciao-face-debug-call ;; ciao-face-blueish-block
  '((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((class color) (background dark))
     (:background "blue3"))
    (((class color) (background light))
     (:background "slate blue" :foreground "white"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:background "gray")))
  "Face to use when at call port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-exit 'ciao-face-debug-exit)
(defface ciao-face-debug-exit ;; ciao-face-greenish-block
  '((((type tty) (class color))
     (:background "green"))
    (((class color) (background light))
     (:background "green"))
    (((class color) (background dark))
     (:background "darkolivegreen"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at exit port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-fail 'ciao-face-debug-fail)
(defface ciao-face-debug-fail ;; ciao-face-reddish-block
  '((((type tty) (class color))
     (:background "red" :foreground "black"))
    (((class color) (background light))
     (:background "Firebrick" :foreground "White"))
    (((class color) (background dark))
     (:background "Firebrick" :foreground "White"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at fail port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-redo 'ciao-face-debug-redo)
(defface ciao-face-debug-redo ;; ciao-face-orangy-block
  '((((type tty) (class color))
     (:background "magenta" :foreground "black"))
    (((class color) (background light))
     (:background "orange"))
    (((class color) (background dark))
     (:background "orange" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at redo port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-expansion 'ciao-face-debug-expansion)
(defface ciao-face-debug-expansion ;; ciao-face-yellowish-block
  '((((type tty) (class color)) 
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use in source debugger when source literal not located."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-breakpoint 'ciao-face-debug-breakpoint)
(defface ciao-face-debug-breakpoint ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t :weight bold)))
  "Face to use with breakpoints in source debugger."
  :group 'ciao-highlighting-faces-debugger)

;; Misc language stuff
(defgroup ciao-highlighting-faces-misc nil
  "Ciao faces for miscellanous language features."
  :tag "Ciao Misc Faces" :group 'ciao-highlighting-faces)

;; resolve an emacs / xemacs incompatibility
(defvar ciao-face-script-header 'ciao-face-script-header)
(defface ciao-face-script-header ;; ciao-face-forestgreen
  '((((type tty) (class color)) (:foreground "green" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "ForestGreen"))
    (t (:inverse-video t)))
  "Face to use for script headers."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-quoted-atom 'ciao-face-quoted-atom)
(defface ciao-face-quoted-atom ;; ciao-face-quoted-atom
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
;    (((class color) (background light)) (:foreground "brown"))
    (((class color) (background light)) (:foreground "gray40"))
    (((class color) (background dark)) (:foreground "gray90"))
    (t (:italic t)))
  "Face to use for quoted atoms."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-funexp-atom 'ciao-face-funexp-atom)
(defface ciao-face-funexp-atom ;; ciao-face-funexp-atom
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "MediumOrchid4"))
    (((class color) (background dark)) (:foreground "MediumPurple"))
    (t (:italic t)))
  "Face to use for atoms in functional notation."
  :group 'ciao-highlighting-faces-misc)

;(defvar ciao-face-assrt-variable 'ciao-face-variable)
;(defface ciao-face-assrt-variable ;; ciao-face-assrt-variable
;  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
;    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
;    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
;    (((class color) (background light)) (:foreground "DarkGoldenrod"))
;    (((class color) (background dark)) (:foreground "goldenrod1"))
;    (t (:italic t)))
;  "Face to use for variables."
;  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-variable 'ciao-face-variable)
(defface ciao-face-variable ;; ciao-face-variable
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "sienna"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:italic t)))
  "Face to use for variables."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-string 'ciao-face-string)
(defface ciao-face-string ;; ciao-face-string
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face to use for strings."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-comment 'ciao-face-comment)
(defface ciao-face-comment ;; ciao-face-comment
  '((((type tty) (class color)) (:foreground "red"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:weight bold :italic t)))
  "Face to use for code comments using fixed pitch (double %)."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-comment-variable-pitch 'ciao-face-comment-variable-pitch)
(defface ciao-face-comment-variable-pitch 
  '((t (:inherit ciao-face-comment :family "helv")))
  "Face to use for code comments using variable pitch (single %)."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-clauseheadname 'ciao-face-clauseheadname)
(defface ciao-face-clauseheadname ;; ciao-face-blue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :weight bold)))
  "Face to use for clause head functors."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-concurrency-op 'ciao-face-concurrency-op)
(defface ciao-face-concurrency-op ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for concurrency operators."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-cut 'ciao-face-cut)
(defface ciao-face-cut ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:inverse-video t)))
  "Face to use for cuts."
  :group 'ciao-highlighting-faces-misc)

;; LPdoc
(defgroup ciao-highlighting-faces-lpdoc nil
  "Ciao faces for documenter-specific assertions (comments, text
strings, commnds, etc.)."
  :tag "Ciao LPdoc Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-lpdoc-bug-comment 'ciao-face-lpdoc-bug-comment)
(defface ciao-face-lpdoc-bug-comment ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc bug comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-bug-comment 'ciao-face-lpdoc-bug-comment)
(defface ciao-face-lpdoc-bug-comment ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc bug comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-version-comment 'ciao-face-lpdoc-version-comment)
(defface ciao-face-lpdoc-version-comment ;; ciao-face-comment
  '((((type tty) (class color)) (:foreground "red"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:weight bold :italic t)))
  "Face to use for LPdoc version comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-comment 'ciao-face-lpdoc-comment)
(defface ciao-face-lpdoc-comment ;; ciao-face-navyblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
  "Face to use for LPdoc textual comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-comment-variable-pitch
  'ciao-face-lpdoc-comment-variable-pitch) 
(defface ciao-face-lpdoc-comment-variable-pitch 
  '((t (:inherit ciao-face-lpdoc-comment :family "helv")))
  "Face to use for LPdoc textual comments in variable pitch."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-verbatim 'ciao-face-lpdoc-verbatim)
(defface ciao-face-lpdoc-verbatim ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "RoyalBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc verbatim text."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-include 'ciao-face-lpdoc-include)
(defface ciao-face-lpdoc-include ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "RoyalBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for LPdoc include commands."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-crossref 'ciao-face-lpdoc-crossref)
(defface ciao-face-lpdoc-crossref ;; ciao-face-golden
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:weight bold :italic t)))
  "Face to use for LPdoc cross-references."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-command 'ciao-face-lpdoc-command)
(defface ciao-face-lpdoc-command ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:inverse-video t)))
  "Face to use LPdoc commands inserted in documentation text."
  :group 'ciao-highlighting-faces-lpdoc)

;; Directives
(defgroup ciao-highlighting-faces-directive nil
  "Ciao faces for various directives (:- ...)."
  :tag "Ciao Directives Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-builtin-directive 'ciao-face-builtin-directive)
(defface ciao-face-builtin-directive ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for the standard directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-predicate-directive 'ciao-face-predicate-directive)
(defface ciao-face-predicate-directive ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:inverse-video t)))
  "Face to use for the predicate-related directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-module-directive 'ciao-face-module-directive)
(defface ciao-face-module-directive ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "RoyalBlue" :weight bold))
    (t (:inverse-video t :weight bold))
     )
     "Face to use for the module-related directives."
     :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-condcode-directive 'ciao-face-condcode-directive)
(defface ciao-face-condcode-directive ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "Gray50"))
    (((class color) (background dark)) (:foreground "Gray70"))
    (t (:inverse-video t))
     )
     "Face to use for the conditional code directives."
     :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-library-directive 'ciao-face-library-directive)
(defface ciao-face-library-directive ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "RoyalBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for directives defined in the library."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-user-directive 'ciao-face-user-directive)
(defface ciao-face-user-directive ;; ciao-face-navyblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
  "Face to use for directives defined by the user (see
   ciao-user-directives custom variable to add new ones)."
  :group 'ciao-highlighting-faces-directive)

;; Assertions
(defgroup ciao-highlighting-faces-assertions nil
  "Ciao faces for Ciao assertions."
  :tag "Ciao Assertions Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-checked-assrt 'ciao-face-checked-assrt)
(defface ciao-face-checked-assrt ;; ciao-face-darkgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "DarkGreen" :weight bold))
    (((class color) (background dark)) (:foreground "LightGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for checked assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-true-assrt 'ciao-face-true-assrt)
(defface ciao-face-true-assrt ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "ForestGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for true assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-false-assrt 'ciao-face-false-assrt)
(defface ciao-face-false-assrt ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :weight bold))
    (((class color) (background dark)) (:foreground "Red" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for false assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-trust-assrt 'ciao-face-trust-assrt)
(defface ciao-face-trust-assrt ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for trust assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-entry-assrt 'ciao-face-entry-assrt)
(defface ciao-face-entry-assrt ;; ciao-face-brown-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Brown" :weight bold))
    (((class color) (background dark)) (:foreground "Brown" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for entry assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-check-assrt 'ciao-face-check-assrt)
(defface ciao-face-check-assrt ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "NavyBlue" :weight bold))
    (((class color) (background dark)) (:foreground "RoyalBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for check assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-prop-assrt 'ciao-face-prop-assrt)
(defface ciao-face-prop-assrt ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for property definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-test-assrt 'ciao-face-test-assrt)
(defface ciao-face-test-assrt ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "GreenYellow" :weight bold))
    (((class color) (background dark)) (:foreground "GreenYellow" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for test assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-texec-assrt 'ciao-face-texec-assrt)
(defface ciao-face-texec-assrt ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "GreenYellow" :weight bold))
    (((class color) (background dark)) (:foreground "GreenYellow" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for texec assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-type-assrt 'ciao-face-type-assrt)
(defface ciao-face-type-assrt ;; ciao-face-mediumblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "MediumBlue" :weight bold))
    (((class color) (background dark)) (:foreground "SkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for type definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-modedef-assrt 'ciao-face-modedef-assrt)
(defface ciao-face-modedef-assrt ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "ForestGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for modedef definitions."
  :group 'ciao-highlighting-faces-assertions)

;; Top levels (Ciao, CiaoPP, LPdoc)
(defgroup ciao-highlighting-faces-toplevels nil
  "Ciao faces for the Ciao, CiaoPP, LPdoc and shell top levels."
  :tag "Ciao Top Levels Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-prompt 'ciao-face-prompt)
(defface ciao-face-prompt ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Coral" :weight bold))
    (((class color) (background dark)) (:foreground "Coral" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for prompts in top-level and shells."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-var 'ciao-face-answer-var)
(defface ciao-face-answer-var ;; ciao-face-purple
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:weight bold)))
  "Face to use for answer variables in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-val 'ciao-face-answer-val)
(defface ciao-face-answer-val ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for answer values in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-yes-answer 'ciao-face-yes-answer)
(defface ciao-face-yes-answer ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "ForestGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for yes answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-no-answer 'ciao-face-no-answer)
(defface ciao-face-no-answer ;; ciao-face-golden-bold
  '((((type tty) (class color)) (:foreground "red" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod" :weight bold))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :weight bold))
    (t (:weight bold :italic t)))
  "Face to use for no answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-ciaopp-option 'ciao-face-ciaopp-option)
(defface ciao-face-ciaopp-option ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "ForestGreen" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face to use for CiaoPP option menus."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-startup-mess 'ciao-face-startup-mess)
(defface ciao-face-startup-mess ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) 
     (:foreground "ForestGreen" :weight bold :family "helv" :height 1.1))
    (((class color) (background dark)) 
     (:foreground "ForestGreen" :weight bold :family "helv" :height 1.1))
    (t (:inverse-video t :weight bold)))
  "Face to use for system splash message."
  :group 'ciao-highlighting-faces-toplevels)

;; Messages
(defgroup ciao-highlighting-faces-messages nil
  "Ciao faces for various messages (errors, warnings, notes, etc.)."
  :tag "Ciao Messages Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-debug-mess 'ciao-face-debug-mess)
(defface ciao-face-debug-mess ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) 
     (:foreground "ForestGreen" :weight bold :family "helv"))
    (((class color) (background dark)) 
     (:foreground "ForestGreen" :weight bold :family "helv"))
    (t (:inverse-video t :weight bold)))
  "Face to use for debug messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-error-mess 'ciao-face-error-mess)
(defface ciao-face-error-mess ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) 
     (:foreground "Red" :weight bold :family "helv"))
    (((class color) (background dark)) 
     (:foreground "Red" :weight bold :family "helv"))
    (t (:inverse-video t :weight bold)))
  "Face to use for error messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-warning-mess 'ciao-face-warning-mess)
(defface ciao-face-warning-mess ;; ciao-face-brown-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) 
     (:foreground "Brown" :weight bold :family "helv"))
    (((class color) (background dark)) 
     (:foreground "Brown" :weight bold :family "helv"))
    (t (:inverse-video t :weight bold)))
  "Face to use for warning messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-note-mess 'ciao-face-note-mess)
(defface ciao-face-note-mess ;; ciao-face-brown
  '((((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) 
     (:foreground "brown" :family "helv"))
    (((class color) (background dark)) 
     (:foreground "brown" :family "helv"))
    (t (:inverse-video t)))
  "Face to use for note messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-other-mess 'ciao-face-other-mess)
(defface ciao-face-other-mess ;; ciao-face-brown
  '((((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) 
     (:foreground "brown" :family "helv"))
    (((class color) (background dark)) 
     (:foreground "brown" :family "helv"))
    (t (:inverse-video t)))
  "Face to use for other messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-highlight-code 'ciao-face-highlight-code)
(defface ciao-face-highlight-code ;; ciao-face-yellowish-block
  '((((type tty) (class color)) 
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (t (:inverse-video t)))
  "Face to use for highlighting code areas (e.g., when locating 
   the code area that an error message refers to)."
  :group 'ciao-highlighting-faces-messages)

;; ;; Just for testing -- but does not work after startup :-(
;; (defun ciao-dark-background ()
;;   (interactive)
;;   "Just for testing how Ciao faces show with dark background. Not
;; meant to be used normally."
;;   (if (boundp 'xemacs-logo)
;;       (progn
;; 	(set-face-background 'default "Black")
;; 	(set-face-foreground 'default "White"))
;;     (set-background-color "Black")
;;     (set-foreground-color "White")))
;; 
;; (defun ciao-light-background ()
;;   (interactive)
;;   "Just for testing how Ciao faces show with light background. Not
;; meant to be used normally."
;;   (if (boundp 'xemacs-logo)
;;       (progn
;; 	(set-face-background 'default "White")
;; 	(set-face-foreground 'default "Black"))
;;     (set-background-color "White")
;;     (set-foreground-color "Black")))

;; The definitions of the title faces were originally taken from
;; font-latex.el (Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002,
;; 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation.)  and
;; adapted to the needs of ciao.el.skel --JFMC

(defconst ciao-face-sectioning-max 5
  "Highest number for ciao-face-sectioning-N-face")
(defface ciao-face-sectioning-5-face
  (if (featurep 'xemacs)
      '((((type tty pc) (class color) (background light))
	 (:foreground "blue4" :bold t))
	(((type tty pc) (class color) (background dark))
	 (:foreground "yellow" :bold t))
	(((class color) (background light))
	 (:bold t :foreground "blue4" :family "helvetica"))
	(((class color) (background dark))
	 (:bold t :foreground "RoyalBlue" :family "helvetica"))
	(t (:bold t :family "helvetica")))
    '((((type tty pc) (class color) (background light))
       (:foreground "blue4" :weight bold))
      (((type tty pc) (class color) (background dark))
       (:foreground "yellow" :weight bold))
      (((class color) (background light))
       (:weight bold :inherit variable-pitch :foreground "NavyBlue"))
      (((class color) (background dark))
       (:weight bold :inherit variable-pitch :foreground "CornflowerBlue"))
      (t (:weight bold :inherit variable-pitch))))
  "Face for sectioning commands at level 5."
  :group 'ciao-faces)

(defun ciao-face-update-sectioning-faces (&optional max height-scale)
  "Update sectioning commands faces."
  (unless height-scale
    (setq height-scale (if (numberp ciao-face-fontify-sectioning)
			   ciao-face-fontify-sectioning
			 1.1)))
  (unless max
    (setq max ciao-face-sectioning-max))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "ciao-face-sectioning-%s-face" num))))
      (unless (get face-name 'saved-face) ; Do not touch customized faces.
	(if (featurep 'xemacs)
	    (let ((size
		   ;; Multiply with .9 because `face-height' returns a value
		   ;; slightly larger than the actual font size.
		   ;; `make-face-size' takes numeric points according to Aidan
		   ;; Kehoe in <16989.15536.613916.678965@parhasard.net> (not
		   ;; documented).
		   (round (* 0.9
			     (face-height 'default)
			     (expt height-scale (- max 1 num))))))
	      ;; (message "%s - %s" face-name size)
	      (make-face-size face-name size))
	  (set-face-attribute face-name nil :height  height-scale))))))

(defcustom ciao-face-fontify-sectioning 1.1
  "Whether to fontify sectioning macros with varying height or a color face.

If it is a number, use varying height faces.  The number is used
for scaling starting from `ciao-face-sectioning-5-face'.  Typically
values from 1.05 to 1.3 give best results, depending on your font
setup.  If it is the symbol `color', use `font-lock-type-face'.

Caveats: Customizing the scaling factor applies to all sectioning
faces unless those face have been saved by customize.  Setting
this variable directly does not take effect unless you call
`ciao-face-update-sectioning-faces' or restart Emacs.

Switching from `color' to a number or vice versa does not take
effect unless you call \\[font-lock-fontify-buffer] or restart
Emacs."
  ;; Possibly add some words about XEmacs here. :-(
  :type '(choice (number :tag "Scale factor")
                 (const color))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (unless (eq value 'color)
	   (ciao-face-update-sectioning-faces ciao-face-sectioning-max value)))
  :group 'ciao-faces)

(defun ciao-face-make-sectioning-faces (max &optional height-scale)
  "Build the faces used to fontify sectioning commands."
  (unless max (setq max ciao-face-sectioning-max))
  (unless height-scale
    (setq height-scale (if (numberp ciao-face-fontify-sectioning)
			   ciao-face-fontify-sectioning
			 1.1)))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "ciao-face-sectioning-%s-face" num)))
	   (f-inherit (intern (format "ciao-face-sectioning-%s-face" (1+ num))))
	   (size (when (featurep 'xemacs)
		   (round (* 0.9 (face-height 'default)
			     (expt height-scale (- max 1 num)))))))
      (eval
       `(defface ,face-name
	  (if (featurep 'xemacs)
	      '((t (:size ,(format "%spt" size))))
	    '((t (:height ,height-scale :inherit ,f-inherit))))
	  (format "Face for sectioning commands at level %s.

Probably you don't want to customize this face directly.  Better
change the base face `ciao-face-sectioning-5-face' or customize the
variable `ciao-face-fontify-sectioning'." num)
	  :group 'ciao-faces))
      (when (and (featurep 'xemacs)
		 ;; Do not touch customized  faces.
		 (not (get face-name 'saved-face)))
	(set-face-parent face-name f-inherit)
	;; Explicitely set the size again to code around the bug that
	;; `set-face-parent' overwrites the original face size.
	(make-face-size face-name size)))))

(ciao-face-make-sectioning-faces ciao-face-sectioning-max)

;;------------------------------------------------------------
;; Key and menu bindings + documentation sections
;; These nifty functions allow autodocumenting using lpdoc! MH
;;------------------------------------------------------------

(defvar ciao-mode-map (make-sparse-keymap))

(defvar ciao-inferior-mode-map nil)

(if ciao-inferior-mode-map
	     nil
	   ; HB: 930205: Use the "correct" function 'copy-keymap'
	   ; to copy a keymap.
           ;; Inherit the commands from comint.
	   (setq ciao-inferior-mode-map (copy-keymap
					 comint-mode-map)))

(defvar ciao-documented-commands nil
 "Stores the list of commands which will appear in the documentation
  for the main mode, preceded by section comments.")

(defun ciao-define-key (map binding function)
  "A call to define-key, but we store stuff in our own format, which
  is used later to generate the documentation."
  (setq ciao-documented-commands
	(cons (list binding function) ciao-documented-commands))
  (define-key map binding function))

;; (defun ciao-report-defined-key (map function &optional comment)
;;   "Store the info for an already defined key. Used to generate the
;; documentation. Optional comment overrides the function's default
;; comment."
;;   (setq ciao-documented-commands
;; 	(cons (list (substitute-command-keys 
;; 		     (concat "\\[" (symbol-name function) "]"))
;; 	       (or comment function)) ciao-documented-commands)))

(defun ciao-report-defined-key (map function &optional comment binding)
  "Store the info for an already defined key. Used to generate the
documentation. Optional comment overrides the function's default
comment. Optional binding sets and reports a different binding."
  (let ((desc 
	 (where-is-internal function map t t)
	 )) ;; used: overriding-local-map 
    (setq ciao-documented-commands
	  (cons (list (if (or desc binding)
			  (if (stringp binding)
				binding
			    (key-description desc))
			(princ (format "M-x %s" function)))
		      (or comment function))
		ciao-documented-commands)))
  (if (stringp binding)
      (define-key map binding function)
    nil)
  )

(defun ciao-documentation-section (sec-title sec-intro)
  "We store a section title and intro, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'section sec-title sec-intro) ciao-documented-commands)))

(defun ciao-documentation-paragraph (paragraph-contents)
  "We store paragraph, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'paragraph paragraph-contents) ciao-documented-commands)))

;; Should start with a section!
(defun ciao-mode-commands (map inferior-map)

  (ciao-documentation-section
    "Syntax coloring and syntax-based editing"
    
    "Syntax-based highlighting (coloring) of code is provided
automatically when opening Ciao files.  This includes also the
assertions used by the preprocessor and the documentation strings used
by the Ciao auto-documenter, @apl{lpdoc}.  The mode should be set to
Ciao and the Ciao mode menus should appear on the menu bar. The
colors and fonts used can be changed through the @index{customize}
options in the help menu (see @ref{Customization}).

During editing this coloring may be refreshed by calling the
appropriate function (see below).

Limited syntax-based auto-indentation and auto-fill of code and
comments is also provided. Syntax highlighting and coloring is also
available for the error and warning messages produced by the top
level, preprocessor, and auto-documenter, and, in general, for the
output produced by these tools.

@noindent
Commands:
  ")

  (ciao-define-key map "\C-ch" 'ciao-fontify-buffer)
  (ciao-define-key map "\t" 'ciao-indent-line)

  (ciao-documentation-section 
   "Getting on-line help" 

   "The following commands are useful for getting on-line help. This
is done by accessing the @apl{info} version of the Ciao manuals or the
@apl{emacs} built-in help strings. Note also that the @apl{info}
standard @tt{search} command (generally bound to @key{s}) can be used
inside @apl{info} buffers to search for a given string.
   ")

  (ciao-define-key map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key map "\C-c\C-m" 'ciao-goto-ciao-manuals)
  (ciao-define-key map "\C-hm"    'ciao-describe-mode)

  (ciao-documentation-section 
   "Loading and compiling programs" 

   "These commands allow @index{loading programs}, @index{creating
executables}, etc. by issuing the appropriate commands to a Ciao top
level shell, running in its own buffer as a subprocess. See @ref{The
interactive top-level shell} for details. The following commands
implement the communication with the Ciao top level:
   ")

  (ciao-define-key map "\C-ct" 'run-ciao-toplevel)
  (ciao-define-key map "\C-cl" 'ciao-load-buffer)
  (ciao-define-key map "\C-cf" 'ciao-load-and-check-buffer)

  (ciao-define-key map "\C-cx" 'ciao-make-exec)
  (ciao-define-key map "\C-co" 'ciao-make-po)
  (ciao-define-key map "\C-ca" 'ciao-make-activemod)

  (ciao-define-key map "\C-cs" 'ciao-set-main-filename)
  (ciao-define-key map "\C-cL" 'ciao-load-from-main-module)

  (ciao-define-key map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key map "\C-cQ" 'ciao-load-query)

  (ciao-documentation-section
   "Commands available in toplevel and preprocessor buffers"
    
   "The interactive top level and the preprocessor both are typically
run in an iteractive buffer, in which it is possible to communicate
with them in the same way as if they had been started from a standard
shell. These interactive buffers run in the so-called @em{Ciao
inferior mode}. This is a particular version of the standard emacs
shell package (comint) and thus all the commands typically available
when running shells inside emacs also work in these buffers.  In
addition, many of the commands and key bindings available in buffers
containing Ciao source code are also available in these interactive
buffers, when applicable.  The Ciao-specific commands available
include:
    ")

  ;; Not such a good idea: completion is better (see
  ;; 'comint-dynamic-complete above) 
  ;; (ciao-define-key map "\t" 'ciao-indent-line)
  (ciao-define-key inferior-map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key inferior-map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key inferior-map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key inferior-map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key inferior-map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key inferior-map "\C-cQ" 'ciao-load-query)
  (ciao-define-key inferior-map "\C-c\C-v" 'ciao-show-preprocessor-output)
  (ciao-define-key inferior-map "\C-cv" 'ciao-report-mode-version)

  (ciao-documentation-paragraph
   (substitute-command-keys "@noindent The following are some of the
commands from the comint shell package which may be specially useful
(type \\<ciao-mode-map> @tt{\\[describe-mode]} while in a Ciao
interactive buffer for a complete list of commands):"))

  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-previous-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-next-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-previous-matching-input)
  (ciao-report-defined-key ciao-inferior-mode-map 
 			   'comint-dynamic-complete
			   "Dynamically find completion of the item at
point. Note that this completion command refers generally to filenames
 (rather than, e.g., predicate names, as in the previous functions)."
			   "\t")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-dynamic-list-filename-completions
                           "List all (filename) completions of the
item at point."
			   "\M-?")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-send-input
			   "Return at any point of the a line at the
end of a buffer sends that line as input. Return not at end copies the
rest of the current line to the end of the buffer and sends it as
input.")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-delchar-or-maybe-eof)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-kill-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'backward-kill-word)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-interrupt-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-stop-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-quit-subjob)

  (ciao-documentation-section 
   "Locating errors and checking the syntax of assertions" 

   "These commands allow locating quickly the point in the source code
corresponding to errors flagged by the compiler or preprocessor as
well as performing several syntactic checks of assertions:
@cindex{locating errors} 
   ")

  (ciao-define-key map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key map "\C-cE"    'ciao-check-buffer-syntax)

  (ciao-documentation-section 
   "Commands which help typing in programs" 

   "The following commands are intended to help in the process of
writing programs: @cindex{script header, inserting automatically}
   ")

  (ciao-define-key map "\C-cIS" 'ciao-insert-script-header)
  (ciao-define-key map "\C-ci"  'ciao-indent-file)

  (ciao-documentation-section
   "Debugging programs"

   "These commands allow marking modules for @index{debugging} by
issuing the appropiate commands to a Ciao top level shell, running in
its own buffer as a subprocess. There are two differents types of
debugging: traditional debugging (using the @concept{byrd-box model}
and @concept{spy-points}) and @index{source-level debugging} (same as
traditional debugging plus source tracing and
@concept{breakpoints}). @cindex{debugging, source-level} In order to
use @index{breakpoints}, source debugging must be on. The following
commands implement comunication with the Ciao top level:
   ")

  (ciao-define-key map "\C-cd" 'ciao-debug-buffer)
  (ciao-define-key map "\C-cm" 'ciao-select-debug-mode)
  (ciao-define-key map "\C-c\M-m" 'ciao-select-buffers-for-debug)

  (ciao-define-key map "\C-cSb" 'ciao-debug-breakon)
  (ciao-define-key map "\C-cSv" 'ciao-debug-breakoff)
  (ciao-define-key map "\C-cSn" 'ciao-debug-all-breakoff)
  (ciao-define-key map "\C-cSl" 'ciao-debug-display-breakpt)
  (ciao-define-key map "\C-cSr" 'ciao-debug-uncolor-all-breakpt)

  (ciao-define-key map "\C-cSt" 'ciao-enable-trace)
  (ciao-define-key map "\C-cSd" 'ciao-enable-debug)

  (ciao-define-key map "\C-cr" 'ciao-load-region)
  (ciao-define-key map "\C-cp" 'ciao-load-predicate)

  (ciao-define-key map "\C-cu" 'ciao-run-test-buffer)

  (ciao-documentation-section 
   "Preprocessing programs" 

   "These commands allow @index{preprocessing programs} with
@apl{ciaopp}, the @index{Ciao preprocessor}.

@include{README_CIAOPP.lpdoc}

See the preprocessor manual for details. The following commands
implement the communication with the Ciao preprocessor:
  ")

;;   (ciao-define-key map "\C-cM" 'ciao-preprocess-buffer-menu)
;;   (ciao-define-key map "\C-cP" 'ciao-preprocess-buffer)
;;   (ciao-define-key map "\C-cT" 'ciao-check-types-modes)
  (ciao-define-key map "\C-cA" 'ciao-analyze-buffer)
  (ciao-define-key map "\C-cT" 'ciao-check-assertions)
  (ciao-define-key map "\C-cO" 'ciao-optimize-buffer)
  (ciao-define-key map "\C-cM" 'ciao-browse-preprocessor-options)
;;   (ciao-define-key map "\C-c\C-p" 'ciao-set-ciaopp-output-pred)
;;   (ciao-define-key map "\C-c\C-f" 'ciao-set-ciaopp-output-full)
;;   (ciao-define-key map "\C-c\C-x" 'ciao-set-ciaopp-output-none)
  (ciao-define-key map "\C-c\C-v" 'ciao-show-preprocessor-output)
;;  (ciao-define-key map "\C-cV" 'ciao-preprocess-buffer-and-show-output)
  (ciao-define-key map "\C-c\C-r" 'run-ciao-preprocessor)

  (ciao-documentation-section 
   "Version control" 

   "The following commands can be used to carry out a simple but
effective form of @concept{version control} by keeping a @concept{log
of changes} on a file or a group of related files. Interestingly, this
log is kept in a format that is understood by @apl{lpdoc}, the Ciao
documenter @cite{lpdoc-tr}. As a result, if these version comments are
present, then @apl{lpdoc} will be able to automatically assign up to
date version numbers to the manuals that it generates. This way it is
always possible to identify to which version of the software a manual
corresponds. Also, @apl{lpdoc} can create automatically sections
describing the changes made since previous versions, which are
extracted from the comments in the changelog entries.

The main effect of these commands is to automatically associate the
following information to a set of changes performed in the file and/or
in a set of related files:

@begin{itemize}

@item a @index{version number} (such as, e.g., @tt{1.2}, where @tt{1}
is the @concept{major version number} and @tt{2} is the @concept{minor
version number}),

@item a @concept{patch number} (such as, e.g., the @tt{4} in
@tt{1.2#4}), 

@item a @concept{time stamp} (such as, e.g.,
@tt{1998/12/14,17:20*28+MET}),

@item the author of the change, @cindex{change, author} and

@item a comment explaining the change. @cindex{change, comment}
@end{itemize}

The @concept{version numbering} used can be local to a single file or
common to a number of related files. A simple version numbering policy
is implemented: when a relevant change is made, the user typically
inserts a @concept{changelog entry} for it, using the appropriate
command (or selecting the corresponding option when prompted while
saving a file). This will cause the @em{patch number} for the file (or
for the whole system that the file is part of) to be incremented
automatically and the corresponding machine-readable comment to be
inserted in the file. Major and minor version numbers can also be
changed, but this is always invoked by hand (see below).

The changelog entry is written in the form of a @decl{comment/2}
declaration.  As mentioned before, the advantage of using this kind of
changelog entries is that these declarations can be processed by the
@apl{lpdoc} automatic documenter (see the @apl{lpdoc} reference
manual @cite{lpdoc-tr} or the @lib{assertions} library documentation
for more details on these declarations). 

Whether the user is asked or not to introduce such changelog entries,
and how the patch and version numbers should be increased is
controlled by the presence in the file of a @pred{comment/2}
declaration of the type:

@tt{:- doc(version_maintenance,<type>).}

@noindent (note that this requires including the @lib{assertions}
library in the source file).  These declarations themselves are also
typically introduced automatically when using this mode (see below).

The version maintenance mode can also be set alternatively by
inserting a comment such as:

@begin{verbatim}
%% Local Variables: 
%% mode: ciao
%% update-version-comments: \"off\"
%% End:
@end{verbatim}

The lines above instruct emacs to put the buffer visiting the file in
@concept{emacs Ciao mode} and to turn version maintenance off.
Setting the version maintenance mode in this way has the disadvantage
that @apl{lpdoc}, the auto-documenter, and other related tools will
not be aware of the type of version maintenance being performed (the
lines above are comments for Ciao). However, this can be useful in
fact for setting the @index{version maintenance mode for packages} and
other files meant for inclusion in other files, since that way the
settings will not affect the file in which the package is included.

The following commands implement the version control support:
   ")

  (ciao-define-key map "\C-c\C-a" 'ciao-set-version-control-for-buffer)
  (ciao-define-key map "\C-x\C-s" 'ciao-save-buffer)
  (ciao-define-key map "\C-c\C-s" 'ciao-add-comment-and-save)
  (ciao-define-key map "\C-cn"    'ciao-new-version)
  (ciao-define-key map "\C-c\C-n" 'ciao-fetch-next-changelog-entry)

  (ciao-documentation-section 
   "Generating program documentation"

   "These commands provide some bindings and facilities for generating
and viewing the documentation corresponding to the current buffer. The
documentation is generated in a temporary directory, which is created
automatically.  This is quite useful while modifying the documentation
for a file, in order to check the output that will be produced,
whithout having to set up a documentation directory by hand or to
regenerate a large manual of which the file may be a part. 
   ")

  (ciao-define-key map "\C-cDB" 'ciao-gen-buffer-doc)
  (ciao-define-key map "\C-cDF" 'ciao-set-lpdoc-docformat)
  (ciao-define-key map "\C-cDS" 'ciao-visit-lpdoc-settings)
  (ciao-define-key map "\C-cDG" 'ciao-gen-doc)
  (ciao-define-key map "\C-cDV" 'ciao-start-viewer)
  (ciao-define-key map "\C-cDW" 'ciao-set-lpdoc-wdir-root)

  (ciao-documentation-section 
   "Setting top level preprocessor and documenter executables"
 
   "These commands allow @index{changing the executables used} when
starting the top-level, the preprocessor, or the auto-documenter. They
also allow changing the arguments that these executables take, and
changing the path where the libraries reside. In the case of the
top-level and preprocessor, this should be done only by users which
understand the implications, but it is very useful if several versions
of Ciao or the preprocessor are available in the system. All these
settings can be changed through the @index{customize} options in the
help menu (see @ref{Customization}).
   ")

  (ciao-define-key map "\C-cSA"    'ciao-customize-all)
  (ciao-define-key map "\C-cSC"    'ciao-set-ciao-system)
  (ciao-define-key map "\C-cS\C-c" 'ciao-set-ciao-system-args)
  (ciao-define-key map "\C-cSP"    'ciao-set-ciaopp-system)
  (ciao-define-key map "\C-cS\C-p" 'ciao-set-ciaopp-system-args)
  (ciao-define-key map "\C-cSL"    'ciao-set-library-path)
  (ciao-define-key map "\C-cSD"    'ciao-set-lpdoc-system)
  (ciao-define-key map "\C-cS\C-d" 'ciao-set-lpdoc-system-args)
  (ciao-define-key map "\C-cS\C-l" 'ciao-set-lpdoc-libpath)

  (ciao-documentation-section 
   "Other commands" 
   "Some other commands which are active in the Ciao mode:
   ") 

  (ciao-define-key map "\C-c\C-l" 'ciao-recenter-last-ciao-buffer)

  (ciao-documentation-section 
   "Traditional Prolog Mode Commands" 

   "These commands provide some bindings and facilities for loading
programs, which are present in emacs Prolog modes of traditional
Prolog systems (e.g., SICStus). This is useful mainly if the Ciao
emacs mode is used with such Prolog systems.  Note that these commands
(@pred{compile/1} and @pred{consult/1}) are deprecated in Ciao (due to
the more advanced, separate compilation model in Ciao) and their use
in the Ciao top-level is not recommended.
   ")

  (ciao-define-key map "\C-cK" 'ciao-compile-buffer)
  (ciao-define-key map "\C-ck" 'ciao-compile-region)
  (ciao-define-key map "\C-c\C-k" 'ciao-compile-predicate)
  (ciao-define-key map "\C-cC" 'ciao-consult-buffer)
  (ciao-define-key map "\C-cc" 'ciao-consult-region)
  (ciao-define-key map "\C-c\C-c" 'ciao-consult-predicate)

  (ciao-documentation-section 
   "Coexistence with other Prolog-like interfaces" 

   "As mentioned previously, the Ciao @apl{emacs} interface can
also be used to work with traditional Prolog or CLP systems. Also, the
Ciao @apl{emacs} interface (@em{mode}) can coexist with other
Prolog-related @apl{emacs} interfaces (@em{modes}) @cindex{emacs mode,
loading several} (such as, e.g., the @apl{SICStus} Prolog
interface). Only one of the interfaces can be active at a time for a
given buffer (i.e., for each given file opened inside @apl{emacs}). In
order the change a buffer to a given interface, move the cursor to
that buffer and type @tt{M-x ...-mode} (e.g., for the Ciao
mode, @tt{M-x ciao-mode}).

If several Prolog-related @apl{emacs} interfaces are loaded, then
typically the @em{last} one to be loaded takes precedence, in the
sense that this will be the interface in which @apl{emacs} will be set
when opening files which have a @tt{.pl} ending (this depends a bit on
how things are set up in your @tt{.emacs} file).")

  (ciao-documentation-section 
   "Getting the Ciao mode version" 
   "@cindex{Ciao mode version}")

  (ciao-define-key map "\C-cv" 'ciao-report-mode-version)

  )

(ciao-mode-commands ciao-mode-map ciao-inferior-mode-map)

(defconst ciao-mode-menus-sys
  (list "CiaoSys"
;;      "----"
;;      "TOP-LEVEL/COMPILER"
     ["(Re)Start Ciao top level"                 run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     ["Check assertions and (re)load into top level" 
                                                 ciao-load-and-check-buffer t]
     ["(Re)Load main and related modules"        ciao-load-from-main-module t]
     ["Make executable from buffer as main"      ciao-make-exec t]
     "----"
     ["Go to (next) preproc/compiler error msg"  ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     "----"
     (list "Query and main file"
	   ["Set default query"                  ciao-set-query t]
	   ["Call default query"                 ciao-load-query t]            
	   ["Clear default query"                ciao-clear-query t]
	   ["(Un)Set main module"                ciao-set-main-filename t]
	   )
     ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax t]
     "----"
     ["Update syntax-based coloring"             ciao-fontify-buffer t]
     ["Indent and format file"                   ciao-indent-file t]
     ["Insert script header"                     ciao-insert-script-header t]
     "----"
     ["Make active module from buffer"           ciao-make-activemod t]
     ["Make object file (.po) from buffer"       ciao-make-po t]
;;
;; These versions with nice comments do not work yet in xemacs --need to port.
;; 
;;      ["(Re)Start Ciao top level"                 run-ciao-toplevel
;;       :help "Starts the Ciao interactive top level."]
;;      ["(Re)Load into top level"                  ciao-load-buffer
;;       :help "Load code in current open file (+ all dependent files) into Ciao."]
;;      ["Check assertions and (re)load into top level" ciao-load-and-check-buffer
;;       :help "Checks assertions in current open file and loads into Ciao."]
;;      ["(Re)Load main and related modules"        ciao-load-from-main-module
;;       :help "Loads file declared as main + all dependent files into Ciao."]
;;      ["Make executable from buffer as main"      ciao-make-exec
;;       :help "Compile an executable with code in current open file + all dependent files."]
;;      "----"
;;      ["Go to (next) preproc/compiler/lpdoc error msg"  ciao-find-last-run-errors
;;       :help "Locate (next) error reported in the last run or the compiler, preprocessor, or documenter."]
;;      ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors
;;       :help "Clear any error highlighting marks left in different buffers."]
;;      "----"
;;      (list "Query and main file"
;; 	   ["(Un)Set main module"                ciao-set-main-filename
;;       :help "Define or undefine the main module of the current project (from which compilation will start)."]
;; 	   ["Set default query"                  ciao-set-query
;;             :help "Define a default query which be called automatically on load."]
;; 	   ["Call default query"                 ciao-load-query
;;             :help "Call the default query defined above."]            
;; 	   ["Clear default query"                ciao-clear-query
;;             :help "Clear the default query so that it is not called on load."]
;; 	   )
;;      ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax
;;       :help "Just check file syntax, including assertions, without loading."]
;;      "----"
;;      ["Update syntax-based coloring"        ciao-fontify-buffer
;;       :help "Refresh the syntax-based coloring (useful for complex code or if syntax-based coloring gets out of sync)."]
;;      ["Insert script header"                     ciao-insert-script-header
;;       :help "Insert appropriate header so that this file is treated as a script (so that it can be run directly without compilation)."]
;;      "----"
;;      ["Make active module from buffer"           ciao-make-activemod
;;       :help "Compile an active module (active process that serves remotely the exported predicates) from this file."]
;;      ["Make object file (.po) from buffer"       ciao-make-po
;;       :help "Force compilation of a relocatable object file for this file (normally done automatically by the compiler when needed). "]
;; 
;; Deprecated and not recommended:
;;      "----"
;;      (list "TRADITIONAL PROLOG COMMANDS (also for SICStus)"
;;            ["Compile buffer"    ciao-compile-buffer  t]
;;            ["Compile region"    ciao-compile-region  t]
;;            ["Compile predicate" ciao-compile-predicate t]
;;            ["Consult buffer"    ciao-consult-buffer  t]
;;            ["Consult region"    ciao-consult-region  t]
;;            ["Consult predicate" ciao-consult-predicate t]
;;      )
     )
  "Menus for the Ciao mode.")

(defconst ciao-mode-menus-debug
  (list "CiaoDbg" 
;;      "----"
;;      "TOP-LEVEL/DEBUGGER"
     ["(Un)Debug buffer source"               ciao-debug-buffer t]
     "----"
     ["Select debug mode"                     ciao-select-debug-mode t]
     ["Select multiple buffers for debug"     ciao-select-buffers-for-debug t]
     (list "Breakpoints"
	   ["Set breakpoint on current literal pred symb" ciao-debug-breakon t]
 	   ["Remove breakpoint from current literal"  ciao-debug-breakoff t]
 	   ["Remove all breakpoints"             ciao-debug-all-breakoff t]
 	   ["Redisplay breakpoints"              ciao-debug-display-breakpt t]
     )
     ["Toggle debug mode (jump to bkp or spypt)" ciao-enable-debug t]
     ["Toggle trace mode"                        ciao-enable-trace t]
     "----"
     ["(Re)Load region (for debug)"              ciao-load-region  t]
     ["(Re)Load predicate (for debug)"           ciao-load-predicate t]
     "----"
     ["Run tests in current module"              ciao-run-test-buffer t]
     ["Run tests in all related modules"         ciao-run-test-related-buffer t]
     ["Show untested predicates"                 ciao-show-untested-preds-buffer t]
   )
  "Ciao debugging menus.")

(defconst ciao-mode-menus-customize
  (list "CiaoOpts"
     ["Customize all Ciao environment settings" ciao-customize-all t] 
     "----"
     ["Customize all Ciao system environment settings" 
                                               (customize-group 'ciao) t]
     ["Set Ciao toplevel executable"           ciao-set-ciao-system t]
     ["Set Ciao toplevel args"                 ciao-set-ciao-system-args t]
     "----"
     ["Set Ciao library path"                  ciao-set-library-path t]
     "----"
     ["Customize all CiaoPP environment settings" (customize-group 'ciaopp) t]
     ["Set Ciao Preprocessor executable"       ciao-set-ciaopp-system t]
     ["Set Ciao Preprocessor executable args"  ciao-set-ciaopp-system-args t]
     "----"
     ["Customize all LPdoc environment settings" (customize-group 'lpdoc) t]
     ["Set LPdoc executable"                   ciao-set-lpdoc-system t]
     ["Set LPdoc executable args"              ciao-set-lpdoc-system-args t]
     ["Set LPdoc root working directory"       ciao-set-lpdoc-wdir-root t]
     ["Set LPdoc library path"                 ciao-set-lpdoc-libpath t]
     "----"
     ["Customize all Ciao colors/faces"        (customize-group 
						'ciao-highlighting-faces) t]
  )
  "Customization menus for the Ciao mode.")

(defconst ciao-mode-menus-help
  (list "CiaoHelp" 
     ["Go to manual page for symbol under cursor" ciao-help-on-current-symbol t]
;; MH Not backwards compatible...
;;      :help "Go to manual page describing the symbol under the cursor" ]
;; Also, these had ( ) 
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao system manual" ciao-goto-ciao-manual t]
     ["Ciao preprocessor manual" ciao-goto-ciaopp-manual t]
     ["LPdoc automatic documenter manual" ciao-goto-lpdoc-manual t]
     ["Ciao manuals area in info index" ciao-goto-ciao-manuals t]
     ["List all key bindings" ciao-describe-mode t]
     "----"
     ["Ciao environment (mode) version" ciao-report-mode-version t]
   )
  "Help menu for the Ciao mode.")


;; MH Tool bar stuff (21.1 onwards)
;; Made a function, so that it is done when maps and menus are active.
;; - This one is for adding to the default toolbar (but modifies others :-( ).
;; (defun ciao-setup-tool-bar () 
;;   (set (make-local-variable 'tool-bar-map) 
;;        (if (display-graphic-p)
;; 	   (progn 
;; 	     (tool-bar-setup) ;; from tool-bar.el
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-help-on-current-symbol "left_arrow" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "ciaoexe" ciao-mode-map)
;; 	     tool-bar-map))))

;; ;; - This one is for an independent tool bar (we add all stuff by hand):
;; (defun ciao-setup-tool-bar () 
;;   (if (display-graphic-p) 
;;       (progn
;; 	(make-local-variable 'tool-bar-map)
;; 	(setq tool-bar-map (make-sparse-keymap))
;; ;; General stuff (from standard tool bar)
;; 	(tool-bar-local-item-from-menu 'find-file "new" ;; "icons/ciaopl"
;;               tool-bar-map  global-map :help "Open or create a (Ciao) file") 
;; 	(tool-bar-local-item-from-menu 'dired "diropen" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'kill-this-buffer "close" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'save-buffer "save"
;;          tool-bar-map global-map  :visible '(or buffer-file-name
;; 						 (not (eq 'special
;; 							  (get major-mode
;; 							       'mode-class)))))
;; 	(tool-bar-local-item-from-menu 'write-file "saveas" 
;; 	 tool-bar-map global-map :visible '(or buffer-file-name
;; 						(not (eq 'special
;; 							 (get major-mode
;; 							      'mode-class)))))
;; 	(tool-bar-local-item-from-menu 'undo "undo" tool-bar-map  
;;          global-map :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [cut]) ;; 'kill-region 
;; 	 "cut" tool-bar-map global-map 
;; 	 :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [copy]) ;; 'menu-bar-kill-ring-save
;; 	 "copy" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [paste]) ;; 'yank 
;; 	 "paste" tool-bar-map global-map 
;; 	 :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 'nonincremental-search-forward "search" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'print-buffer "print" tool-bar-map)
;; ;; Ciao-specific stuff
;; 	(tool-bar-local-item-from-menu  
;; 	 'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-fontify-buffer "icons/ciaorehighlight"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-load-buffer "icons/ciaoload" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-find-last-run-errors "jump_to" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-unmark-last-run-errors "icons/clear" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-check-buffer-syntax "icons/ciaoasr" tool-bar-map ciao-mode-map) 
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-check-types-modes    "icons/checkassertions"
;; ;;            tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer-menu 
;; ;; 	      "icons/ciaopreprocask" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer    "icons/ciaopreproc"
;; ;;            tool-bar-map ciao-mode-map) 
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer-and-show-output
;; ;; 	      "icons/ciaopreprocsee" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-check-assertions "icons/checkassertions"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-browse-preprocessor-options
;; 	 "icons/ciaocustomize" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-debug-buffer "icons/ciaodebug" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-gen-buffer-doc "icons/lpdoc" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-start-viewer "icons/lpdocview" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-make-exec "icons/ciaoexeout" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-insert-script-header "icons/ciaoscrt" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-make-po "icons/ciaopo" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-make-exec "icons/ciaoitf" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu
;; 	 'ciao-goto-ciao-manuals "icons/manuals"  ;; "ciaomanuals" 
;; 	 tool-bar-map ciao-mode-map
;; 	 :help "Go to area containing the Ciao system manuals")
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-help-on-current-symbol "icons/wordhelp"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-complete-current-symbol "icons/complete"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu 
;; 	 'ciao-customize-all
;; 	 "preferences" tool-bar-map ciao-mode-map
;; 	 :help "Edit (customize) preferences for Ciao, CiaoPP, LPdoc")
;; 	)))
;; 
;; 
;; - This part is for setting tool bars in both FSF and xemacs.
;; Menu bar accummulator for xemacs version
(defvar ciao-xemacs-tool-bar-tmp nil)

(defun ciao-find-icon (icon)
  "Icon with absolute path (for xemacs)"
  (cond
   ((file-exists-p (expand-file-name icon ciao-real-lib-dir))
    (expand-file-name icon ciao-real-lib-dir))
   ((file-exists-p (expand-file-name icon 
				     (concat ciao-real-lib-dir "/../emacs-mode/")))
    (expand-file-name icon (concat ciao-real-lib-dir "/../emacs-mode/")))))

;; Portable tool-bar-add-item-from-menu function, adds to accumulators.
(defun ciao-tool-bar-local-item-from-menu 
       (def icon to-map &optional from-map &rest props)
  (if (boundp 'xemacs-logo)
      ;; xemacs
      ;; *** Still need to fish out help strings from menus and pass
      ;;     them to tooltip below
      (progn 
	(setq ciao-xemacs-tool-bar-tmp
	      (cons 
	       `[,(toolbar-make-button-list ;; icon
		   (ciao-find-icon (concat icon "-bg.xpm")))
		 ,def ;; the actual callback
		 t  ;; enabled
		 "" ;; tooltip
		 ] 
	       ciao-xemacs-tool-bar-tmp))
	ciao-xemacs-tool-bar-tmp)
    ;; FSF emacs
    (if (> emacs-major-version 21)
	(tool-bar-local-item-from-menu def icon to-map from-map props)
      (unless from-map
	(setq from-map global-map))
      (tool-bar-add-item-from-menu def icon from-map props))
    ))

(defun ciao-setup-tool-bar () 
  (if (and (not ciao-inhibit-toolbar) ;; ????
	   (or 
	    ;; xemacs case 
	    (and (boundp 'xemacs-logo)
		 (featurep 'toolbar)
		 (console-on-window-system-p))
	    ;; FSF emacs case 
	    (and (fboundp 'tool-bar-mode)
		 (display-graphic-p))
	   ))
      (ciao-do-setup-tool-bar)))

(defun ciao-do-setup-tool-bar () 
  (make-local-variable 'tool-bar-map) 
  (if (boundp 'xemacs-logo)
      (setq ciao-xemacs-tool-bar-tmp nil)
    (setq tool-bar-map (make-sparse-keymap)))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; Ciao-specific stuff - added in both FSF and xemacs
  ;; Ciao logo is a special case
  (if (boundp 'xemacs-logo)
      (progn
	(set-specifier 
	 default-toolbar 
	 (cons 
	  (current-buffer) 
	  (append
	   (specifier-specs default-toolbar 'global)
	   ;; '([:style 2d :size 30])
           '(nil) ;; separator (flush right)
	   `([,(toolbar-make-button-list ;; icon
		(ciao-find-icon "icons/ciao-bg.xpm"))
	      run-ciao-toplevel ;; the actual callback
	      t  ;; enabled
	      "" ;; tooltip
	      ])
	   ))))
    (ciao-tool-bar-local-item-from-menu 
     'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-mode-map))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-fontify-buffer "icons/ciaorehighlight"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-load-buffer "icons/ciaoload" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-find-last-run-errors "icons/jump_to" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-unmark-last-run-errors "icons/clear" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-check-buffer-syntax "icons/ciaoasr" tool-bar-map ciao-mode-map) 
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-check-types-modes    "icons/checkassertions"
;;            tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer-menu 
;; 	      "icons/ciaopreprocask" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer    "icons/ciaopreproc"
;;            tool-bar-map ciao-mode-map) 
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer-and-show-output
;; 	      "icons/ciaopreprocsee" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-check-assertions "icons/checkassertions"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-browse-preprocessor-options
   "icons/ciaocustomize" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-debug-buffer "icons/ciaodebug" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-gen-buffer-doc "icons/lpdoc" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-start-viewer "icons/lpdocview" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-make-exec "icons/ciaoexeout" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-insert-script-header "icons/ciaoscrt" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-make-po "icons/ciaopo" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-make-exec "icons/ciaoitf" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu
   'ciao-goto-ciao-manuals "icons/manuals"  ;; "ciaomanuals" 
   tool-bar-map ciao-mode-map
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu  
   'ciao-help-on-current-symbol "icons/wordhelp"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-complete-current-symbol "icons/complete"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map ciao-mode-map
   :help "Edit (customize) preferences for Ciao, CiaoPP, LPdoc")
  (ciao-xemacs-toolbar-postprocess ciao-xemacs-tool-bar-tmp))

(defun ciao-general-toolbar (tool-bar-map)
  (if (not (boundp 'xemacs-logo))
      (progn
	;; *** Comment does not show up...
	(ciao-tool-bar-local-item-from-menu 'find-file "new" ;; "icons/ciaopl"
              tool-bar-map  global-map :help "Open or create a (Ciao) file")
	(ciao-tool-bar-local-item-from-menu 
	 'dired (if (> emacs-major-version 21) "diropen" "open") 
	 tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 'kill-this-buffer "close"
                                            tool-bar-map) 
	(ciao-tool-bar-local-item-from-menu 'save-buffer "save"
         tool-bar-map global-map  :visible '(or buffer-file-name
						 (not (eq 'special
							  (get major-mode
							       'mode-class)))))
	(ciao-tool-bar-local-item-from-menu 'write-file "saveas" 
	 tool-bar-map global-map :visible '(or buffer-file-name
						(not (eq 'special
							 (get major-mode
							      'mode-class)))))
	(ciao-tool-bar-local-item-from-menu 'undo "undo" tool-bar-map  
         global-map :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [cut]) ;; 'kill-region 
	 "cut" tool-bar-map global-map 
	 :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [copy]) ;; 'menu-bar-kill-ring-save
	 "copy" tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [paste]) ;; 'yank 
	 "paste" tool-bar-map global-map 
	 :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 'nonincremental-search-forward "search" tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 'print-buffer "print" tool-bar-map)
	)))

(defun ciao-xemacs-toolbar-postprocess (ciao-xemacs-tool-bar-tmp)
  (if (boundp 'xemacs-logo)
      (progn
;; 	(set-default-toolbar-position 'left)
	(set-specifier right-toolbar-visible-p t)
	;; (set-specifier right-toolbar-width 60)
	(set-specifier right-toolbar-width 35)
	(set-specifier 
	 ;; default-toolbar 
	 ;; left-toolbar
	 right-toolbar
	 (cons 
	  (current-buffer) 
	  (append
	   ;; For adding the default stuff
	   ;; (specifier-specs default-toolbar 'global)
	   ;; Separator
	   ;; '([:style 3d :size 30])
	   (reverse ciao-xemacs-tool-bar-tmp)
	   ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ciao-customize-all () 
  (interactive)
  "Enter interface customization option browser."
  (customize-group 'ciao-environment))

(defconst ciao-mode-menus-ciaopp
  (list "CiaoPP"
;;     "CIAO PREPROCESSOR (in development)"
     "Note: CiaoPP required (in development)"
     "----"
;;      ["Preprocess buffer (choosing options)"   ciao-preprocess-buffer-menu t]
;;      ["Preprocess buffer (w/previous options)" ciao-preprocess-buffer t]
;;      ["Check types and modes"  ciao-check-types-modes t]
     ["Analyze buffer"                         ciao-analyze-buffer t]
     ["Check buffer assertions"                ciao-check-assertions t]
     ["Optimize buffer"                        ciao-optimize-buffer t]
     "----"
     ["Browse analysis/checking/optimizing options"         
                                           ciao-browse-preprocessor-options t]
     "----"
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
;;      ["Preprocess buffer (w/previous options) and show output"  
;;                                       ciao-preprocess-buffer-and-show-output t]
;;      ["Output only predicate-level analysis info" ciao-set-ciaopp-output-pred t]
;;      ["Output literal- and pred-level analysis info" ciao-set-ciaopp-output-full t]
;;      ["Do not output analysis info" ciao-set-ciaopp-output-none t]
     "----"
     ["Start Ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for CiaoPP mode.")

(defconst ciao-mode-menus-lpdoc
  (list "LPdoc"
;;      "----"
;;      "GENERATE/VIEW DOCUMENTATION"
     ["Generate documentation for buffer"        ciao-gen-buffer-doc t]
     ["View documentation in selected format"    ciao-start-viewer t]
     ["Change default doc format/visualizer"     ciao-set-lpdoc-docformat t]
     ["Goto (next) preproc/compiler error msg"   ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Visit(/create) SETTINGS.pl file"        ciao-visit-lpdoc-settings t]
     ["Generate documentation"                   ciao-gen-doc t]
     "----"
;;      "CHANGELOG / VERSION CONTROL"
     ["Set version control for file"  ciao-set-version-control-for-buffer t] 
     ["Insert changelog entry/increase patch #" ciao-add-comment-and-save t]
     ["Increase version number"              ciao-new-version t]
     ["Go to next changelog entry"           ciao-fetch-next-changelog-entry t]
     )
  "Menus for LPdoc mode.")

(defconst ciao-inferior-mode-menus
;;  (list "Ciao"
  (list "Ciao"
     ["Update syntax-based coloring"        ciao-fontify-buffer t]
     "----"
;;     "ERRORS"
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error marks in buffers"            ciao-unmark-last-run-errors t]
     "----"
;;     "COMPILER/TOP-LEVEL/DEBUGGER"
     ["Set query as default"               ciao-set-query t]
     ["Clear default query"                ciao-clear-query t]
     ["Load default query"                 ciao-load-query t]
     ["Start Ciao top level"               run-ciao-toplevel t]
     "----"
;;     "PREPROCESSOR (in development)"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Start Ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for the Ciao (inferior) mode.")

(defun ciao-setup-inferior-tool-bar () 
  (if (and (not ciao-inhibit-toolbar) ;; ????
	   (or 
	    ;; xemacs case 
	    (and (boundp 'xemacs-logo)
		 (featurep 'toolbar)
		 (console-on-window-system-p))
	    ;; FSF emacs case 
	    (and (fboundp 'tool-bar-mode)
		 (display-graphic-p))
	   ))
      (ciao-do-setup-inferior-tool-bar)))

(defun ciao-do-setup-inferior-tool-bar () 
  (make-local-variable 'tool-bar-map)
  (if (boundp 'xemacs-logo)
      (setq ciao-xemacs-tool-bar-tmp nil)
    (setq tool-bar-map (make-sparse-keymap)))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; Ciao-specific stuff - added in both FSF and xemacs
  ;; Ciao logo is a special case
  (if (boundp 'xemacs-logo)
      (progn
	(set-specifier 
	 default-toolbar 
	 (cons 
	  (current-buffer) 
	  (append
	   (specifier-specs default-toolbar 'global)
	   ;; '([:style 2d :size 30])
           '(nil) ;; separator (flush right)
	   `([,(toolbar-make-button-list ;; icon
		(ciao-find-icon "icons/ciao-bg.xpm"))
	      run-ciao-toplevel ;; the actual callback
	      t  ;; enabled
	      "" ;; tooltip
	      ])
	   ))))
    (ciao-tool-bar-local-item-from-menu
     'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-inferior-mode-map))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-fontify-buffer "icons/ciaorehighlight" 
   tool-bar-map ciao-inferior-mode-map)
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-interrupt-subjob  
       "icons/stop" tool-bar-map comint-mode-map
       :help "Interrupt top level"))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-find-last-run-errors "icons/jump_to" 
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-unmark-last-run-errors "icons/clear" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-set-query "icons/ciaostorequery" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-load-query "icons/ciaoprompt" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-clear-query "icons/ciaoclearquery" 
   tool-bar-map ciao-inferior-mode-map) 
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-previous-input
       "icons/left-arrow" tool-bar-map comint-mode-map 
       :help "Insert previous inputs at prompt"))
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-next-input
       "icons/right-arrow" tool-bar-map comint-mode-map
       :help "Insert later inputs at prompt"))
  (ciao-tool-bar-local-item-from-menu
   'ciao-goto-ciao-manuals "icons/manuals"  ;; "ciaomanuals" 
   tool-bar-map ciao-inferior-mode-map
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu  
   'ciao-help-on-current-symbol "icons/wordhelp"
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-complete-current-symbol "icons/complete"
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map ciao-inferior-mode-map
   :help "Edit (customize) preferences for Ciao, CiaoPP, LPdoc")
  (ciao-xemacs-toolbar-postprocess ciao-xemacs-tool-bar-tmp))

;;------------------------------------------------------------
;; Syntax and movement
;;------------------------------------------------------------

(defvar ciao-mode-syntax-table nil)
(if ciao-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "w" table) ; word constituent
    (modify-syntax-entry ?\\ "." table) ; punctuation
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
;;  1 means CHAR is the start of a two-char comment start sequence.
;;  2 means CHAR is the second character of such a sequence.
;;  3 means CHAR is the start of a two-char comment end sequence.
;;  4 means CHAR is the second character of such a sequence.
    (modify-syntax-entry ?/ "." table) ; punctuation
    (modify-syntax-entry ?* "." table) ; punctuation
    (modify-syntax-entry ?+ "." table) ; punctuation
    (modify-syntax-entry ?- "." table) ; punctuation
    (modify-syntax-entry ?= "." table) ; punctuation
    (modify-syntax-entry ?% "<" table) ; comment starter
    (modify-syntax-entry ?\n ">" table); comment ender
    (modify-syntax-entry ?\^m ">" table); ; comment ender
    (modify-syntax-entry ?< "." table) ; punctuation
    (modify-syntax-entry ?> "." table) ; punctuation
    (modify-syntax-entry ?\' "\"" table) ; escape
    (setq ciao-mode-syntax-table table)))

(defvar ciao-mode-abbrev-table nil)
(define-abbrev-table 'ciao-mode-abbrev-table ())

(defun ciao-mode-variables ()
  (setq local-abbrev-table ciao-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^%%\\|^$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ciao-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
;; Obsolete since before 19.5
;;   (make-local-variable 'comment-indent-hook)
;;   (setq comment-indent-hook 'ciao-comment-indent)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ciao-comment-indent)
;; Using make-variable-buffer-local above
;;  (make-local-variable 'update-version-comments)
;;  (setq update-version-comments 0) ; 0 means "uninitialized"
  ;; Source debugger variables
  (make-local-variable 'ciao-debug-last-frame) 
  (setq ciao-debug-last-frame nil)
  (make-local-variable 'ciao-debug-delete-prompt-marker)
  (setq ciao-debug-delete-prompt-marker (make-marker))
  )

(defun ciao-indent-line (&optional whole-exp)
  "Indent current line as Ciao code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "p")
  (let ((indent (ciao-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")

    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))

    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

;; JA 890605
(defun ciao-indent-level ()
  "Compute Ciao indentation level."
  (save-excursion
    (beginning-of-line)
      (skip-chars-forward " \t")
      (cond
       ((bobp) 0)                            ;Beginning of buffer
       ((looking-at "\n")                    ;a new fresh line
        (ciao-indent-for-new-clause))
       (t                                    ;indent existing clause
        (forward-line -1)
	(ciao-indent-for-new-clause)))))

;; JA 890601
(defun ciao-search-for-prev-goal ()
  "Search for the most recent Ciao symbol (in head or in body)."
  (while (and (not (bobp)) (or (looking-at "%") (looking-at "\n")))
    (forward-line -1)
    (skip-chars-forward " \t")))

;; JA 890601
(defun ciao-indent-for-new-clause ()
  "Find column for a new goal."
  (skip-chars-forward " \t")
  (ciao-search-for-prev-goal)
  (let ((prevcol (current-column)))
    (ciao-end-of-clause)
    (forward-char -1)
    (cond ((bobp) 0)
	  ((looking-at "[.]") 0)
	  ((zerop prevcol) ciao-first-indent-width)
	  ((looking-at "[\[{(;]")
	   (max ciao-first-indent-width (+ ciao-indent-width (ciao-column-of-um-lparen))))
	  ((looking-at "[,>]") (ciao-column-of-prev-term))
	  (t (ciao-column-of-um-lparen)))))

;; JA 890601
(defun ciao-column-of-prev-term ()
  (beginning-of-line)
  (skip-chars-forward " \t\[{(;")
  (current-column))

;; JA 890601
(defun ciao-column-of-um-lparen ()
  (let ((pbal 0))
    (while (and (>= pbal 0)
		(or (> (current-column) 0)
		    (looking-at "[ \t]")))
      (cond ((looking-at "[\]})]")
	     (setq pbal (1+ pbal))
	     (forward-char -1))
	    ((looking-at "[\[{(]")
	     (setq pbal (1- pbal))
	     (forward-char -1))
	    ((looking-at "'")
	     (search-backward "'" nil t)
	     (forward-char -1))
	    ((looking-at "\"")
	     (search-backward "\"" nil t)
	     (forward-char -1))
	    (t (forward-char -1)))))
  (forward-char 1)  ;; Reset buffer pointer to prev column
  (current-column))

(defun ciao-end-of-clause ()
  "Go to end of clause in this line."
  (beginning-of-line)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

;; (defun ciao-comment-indent ()
;;   "Compute Ciao comment indentation."
;;   (ciao-indent-level))
(defun ciao-comment-indent ()
  "Compute Ciao comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (ciao-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))))

;;------------------------------------------------------------
;; Help (locating manuals, calling word-help, etc.)
;;------------------------------------------------------------

(defun ciao-help-on-current-symbol () 

  "Find help for the symbol (e.g., predicate, directive, declaration, type,
etc.) that is currently under the cursor. Opens a (hopefully) relevant part
of the Ciao manuals in @apl{info} mode. Requires that the Ciao manuals in
@apl{info} format be installed and accessible to @apl{emacs} (i.e., they
should appear somewhere in the info directory when typing @tt{M-x
info}). It also requires @file{word-help.el}, which is provided with
Ciao. Refer to the installation instructions if this is not the case."

  (interactive) 
  (call-interactively 'word-help))

(defun ciao-complete-current-symbol () 

  "Find a completion for the symbol (e.g., predicate, directive,
declaration, type, etc.) that is currently under the cursor. Uses for
completion the contents of the indices of the Ciao manuals. Same
requirements as for finding help for the symbol."

  (interactive) 
  (call-interactively 'word-help-complete))

(defun ciao-goto-ciao-manuals () 
  "Go to the part of the info directory containing the Ciao manuals."
  (interactive) 

  (ciao-locate-manual-in-info-dir "Ciao system"))

(defun ciao-goto-ciao-manual () 
  "Go to the part of the info directory containing the Ciao manual."
  (interactive) 
  (ciao-goto-particular-manual "ciao:"))

(defun ciao-goto-ciaopp-manual () 
  "Go to the part of the info directory containing the Ciao
preprocessor manual." 
  (interactive) 
  (ciao-goto-particular-manual "ciaopp:"))

(defun ciao-goto-lpdoc-manual () 
  "Go to the part of the info directory containing the lpdoc
(automatic documenter) manual." 
  (interactive) 
  (ciao-goto-particular-manual "lpdoc:"))

(defun ciao-locate-manual-in-info-dir (text) 
  "Locate a manual entry in the info dir"
  (info) 
  (Info-directory)
  (if (search-forward text nil t) 
      (recenter 0)
    (error (concat "Could not find " text " manual in info dir"))))

(defun ciao-goto-particular-manual (manual) 
  "Go to a particular manual."
  (ciao-locate-manual-in-info-dir manual)
  (if (not (boundp 'xemacs-logo))
      (Info-follow-nearest-node)
    (backward-char 3)
    (Info-follow-nearest-node (point))
    ))

(defun ciao-describe-mode () 
  "Show a short description of the Ciao emacs mode, including all key
bindings." 
  (interactive) 
  (describe-mode))

;;------------------------------------------------------------
;; On-line comments and changelog management
;;------------------------------------------------------------

(defun ciao-new-version () 

  "Force a move to a new major/minor version number (the user will be
prompted for the new numbers). Only applicable if using
directory-based version maintenance. Note that otherwise it suffices
with introducing a changelog entry in the file and changing its
version number by hand."

  (interactive)
  (ciao-handle-version-control-option)
  (if (or (string= (ciao-version-maint-type) "off") 
	  (string= (ciao-version-maint-type) "on"))
      (error "Only supported if using version directory")
    (if (not (string= 
	      (read-string "Change major/minor version (y/n) ?" "n")
	      "y"))
	nil
     
      (message "Will first delete current Version/Patch files")
      (sleep-for 2)
      (delete-file (concat (ciao-version-maint-type) "/GlobalVersion"))
      (delete-file (concat (ciao-version-maint-type) "/GlobalPatch"))
      (message "Current Version/Patch files deleted")
      (sleep-for 2)
      (ciao-update-version (ciao-version-maint-type))
      )
    )
  )

(defun ciao-set-version-control-for-buffer ()
"Used to turn on or off version control for the file being visited in
the current buffer.  The user will be prompted to choose among the
following options:

   @begin{description} 

   @item{@key{y}} Turn version control on for this file. 

   @item{@key{n}} Turn version control off for this file. A version
control comment such as:

@tt{:- doc(version_maintenance,off).}

@noindent will be added to the buffer and the file saved. No version
control will be performed on this file until the line above is removed
or modified (i.e., from now on \\<ciao-mode-map> \\[ciao-save-buffer]
simply saves the buffer).

   @item{@key{q}} Turn off prompting for the introduction of changelog
entries for now. @apl{emacs} will not ask again while the buffer is
loaded, but it may ask again when saving after the next time you load
the buffer (if @tt{ciao-ask-for-version-maintenance-type} is set to
@tt{yes}).

   @end{description}

   If @key{y} is selected, then the system prompts again regarding how
and where the version and patch number information is to be
maintained. The following options are available:

   @begin{description}

   @item{@tt{on}} All version control information will be contained
within this file. When saving a buffer \\<ciao-mode-map>
(\\[ciao-save-buffer]) emacs will ask if a changelog entry should be
added to the file before saving. If a comment is entered by the user,
a new patch number is assigned to it and the comment is added to the
file. This patch number will be the one that follows the most recent
changelog entry already in the file. This is obviously useful when
maintaining version numbers individually for each file.

   @item{@tt{<directory_name>}} Global version control will be
performed coherently on several files. When saving a buffer
\\<ciao-mode-map> (\\[ciao-save-buffer]) emacs will ask if a changelog
entry should be added to the file before saving. If a comment is
given, the global patch number (which will be kept in the file:
@tt{<directory_name>/GlobalPatch}) is atomically incremented and the
changelog entry is added to the current file, associated to that patch
number. Also, a small entry is added to a file
@tt{<directory_name>/GlobalChangeLog} which points to the current
file. This allows inspecting all changes sequentially by visiting all
the files where the changes were made (see \\<ciao-mode-map> 
\\[ciao-fetch-next-changelog-entry]). This is obviously useful when
maintaining a single thread of version and patch numbers for a set of
files.

   @item{@tt{off}} Turns off version control: \\[ciao-save-buffer] then simply
   saves the file as usual. 

   @end{description}

@bf{Some useful tips:} 

@begin{itemize}

@item If a changelog entry is in fact introduced, the cursor is left
at the point in the file where the comment was inserted and the mark
is left at the original file point. This allows inspecting (and
possibly modifying) the changelog entry, and then returning to the
original point in the file by simply typing
\\[exchange-point-and-mark].

@item @cindex{moving changelog entries} The first changelog entry is
entered by default at the end of the buffer. Later, the changelog
entries can be moved anywhere else in the file. New changelog entries
are always inserted just above the first changelog entry which appears
in the file.

@item The comments in changelog entries can be edited at any time. 

@item If a changelog entry is moved to another file, and version
numbers are shared by several files through a directory, the
corresponding file pointer in the
@tt{<directory_name>/GlobalChangeLog} file needs to be changed also,
for the entry to be locatable later using
\\[ciao-fetch-next-changelog-entry].

@end{itemize}

"
  (interactive)
  (let ((option-was) (maint-type))
    (setq option-was ciao-ask-for-version-maintenance-type)
    (setq ciao-ask-for-version-maintenance-type "yes")
    (cond
     ((string= (ciao-version-maint-type) "on")
      (message "File already under version control. Edit file for changes."))
     ((string= (ciao-version-maint-type) "off")
      (message "Version control already disabled. Revisit or edit the file."))
     (t
      (setq update-version-comments 0)
      (ciao-handle-version-control-option)))
    (setq ciao-ask-for-version-maintenance-type option-was)
  ))

(defun ciao-save-buffer ()

  "This is the standard @apl{emacs} command that saves a buffer by
writing the contents into the associated @tt{.pl} file.  However, in
the Ciao mode, if version control is set to on for ths file, then this
command will ask the user before saving whether to introduce a
changelog entry documenting the changes performed.

In addition, if: 

@begin{itemize}

@item the buffer does not already contain a comment specifying the
@concept{type of version control} to be performed,

@item and the customizable variable
@tt{ciao-ask-for-version-maintenance-type} is set to @tt{yes} (go to
the Ciao options menu, LPdoc area to change this, which is by default
set to @tt{no}),

@end{itemize} 

@noindent then, before saving a buffer, the user will be also
automatically asked to choose which kind of version control is desired
for the file, as in \\<ciao-mode-map>
\\[ciao-set-version-control-for-buffer].

"
  (interactive)
  (ciao-save-buffer-option nil))

(defun ciao-add-comment-and-save ()

  "Same as \\<ciao-mode-map> \\[ciao-save-buffer] except that it
forces prompting for inclusion of a changelog entry even if the buffer
is unmodified."

  (interactive)
  (ciao-save-buffer-option t))

(defun ciao-save-buffer-option (save-option)
  "Same as above, but allows forcing save / minor version change."
  (interactive)
  (if (and (eq (buffer-modified-p) nil) (eq save-option nil))
      ;; will do nothing -- just for printing the usual message
      (save-buffer) 
    (ciao-handle-version-control-option)
    (if (and (string= (ciao-version-maint-type) "off") (eq save-option nil))
	;; just normal save
	(save-buffer)
      (if (and (eq save-option t) 
	       (not (string= (ciao-version-maint-type) "off")))
	  ;; no need to ask
	  (ciao-update-version (ciao-version-maint-type))
	(if (string= (ciao-version-maint-type) "off")
	    ;; will do nothing -- just for printing the usual message
	    (save-buffer) 
	  ;; ask 
	  (if (not (string= 
		    (read-string "Insert changelog entry (y/n) ?" "n")
		    "y"))
	      (save-buffer);; normal save and return
	    ;; update version and save
	    (ciao-update-version (ciao-version-maint-type))
	    ))))))

(defun ciao-update-version (version-dir) 
  "Inserts a changelog entry (comment and patch number change). If a
  comment is in fact introduced, the buffer is left at the file point
  of the entry for inspection and the mark is left at the original
  file point for easy return."  
  (interactive)
  (let (original-point 
	original-buffer 
	version-file 
	version-major 
	version-minor
	no-previous-version
	patch-file
	patch-buffer
	patch-number
	keep-version
	comment
	month day year time
	old-version-control
	change-file
	tmp-point)
  (setq original-point (point))
  (goto-char (point-min))
  (cond
   ((not (or (string= version-dir "on") (string= version-dir "off")))
    ;; Previous version is in external file - get it
    ;; For locking, we are taking advantage of emacs file locking by
    ;; modifying the buffer right away.
    (setq original-buffer (current-buffer))
    (setq version-file (concat version-dir "/GlobalVersion"))
    (if (file-readable-p version-file)
	(progn 
	  (find-file version-file)
	  (goto-char (point-min))
	  (setq tmp-point (point))
	  (search-forward-regexp "\\.")
	  (backward-char 1)
	  ;; kill-region modifies and sets lock...
	  (setq version-major
		(buffer-substring-no-properties tmp-point (point)))
	  (forward-char 1)
	  (setq tmp-point (point))
	  (end-of-line)
	  (setq version-minor
		(buffer-substring-no-properties tmp-point (point)))
	  (setq no-previous-version nil)
	  (kill-buffer (current-buffer))
	  )
      (if (string= 
	   (read-string 
	    (concat "Could not find " version-file ", create ?") "y")
	   "y")
	  (progn
	    (setq no-previous-version t))
	(error "No version file")))

    (setq patch-file (concat version-dir "/GlobalPatch"))
    (if no-previous-version
	nil
      ;; There is a previous version
      (if (file-readable-p patch-file)
	  ;; Readable patch file: get patch number
	  (progn 
	    (switch-to-buffer original-buffer) ;; So that relative paths work!
	    (find-file patch-file)
	    (goto-char (point-min))
	    (setq patch-buffer (current-buffer))
	    (setq tmp-point (point))
	    (end-of-line)
	    (setq patch-number 
		  (buffer-substring-no-properties tmp-point (point)))
	    (kill-buffer (current-buffer))
	    )
	;; No patch file: new patch number
	(setq patch-number "-1")))

    (switch-to-buffer original-buffer))
   ((search-forward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(" nil t)
    ;; A previous version exists in the file: get it
    (setq tmp-point (point))
    (search-forward-regexp "\\*")
    (backward-char 1)
    (setq version-major
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "\\+")
    (backward-char 1)
    (setq version-minor
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "[ \t\n]*,")
    (backward-char 1)
    (setq patch-number 
	  (buffer-substring-no-properties tmp-point (point)))
    (setq no-previous-version nil)
    )
   (t
    ;; No previous version exists: set it to 0.1+-1
    (setq no-previous-version t)
    )
   )

  (if no-previous-version
      (progn 
	(setq keep-version "n")
	(setq version-major "0")
	(setq version-minor "1")
	)
       (setq keep-version "y")
    )
	
  ;; If we keep the version or no comment
  (if (string= keep-version "y")
        ;; Version and patch number stay as they are
	nil
    ;; Else, get new version
    (setq version-major
	  (read-string "Major version ? " version-major))
    (setq version-minor
	  (read-string "Minor version ? " version-minor))
    ;; and reset patch number
    (setq patch-number "-1"))
   
  (setq comment (read-string (concat 
			      "Type a comment for new version "
			      version-major "." 
			      version-minor "#" 
			      (int-to-string 
			       (+ (string-to-number patch-number) 1))
			      ":"
			      ) 			     
			     ""))

  (if (string= comment "")
      nil
    ;; Increment patch number (will be 0 if new version)
    (setq patch-number (int-to-string (+ (string-to-number patch-number) 1))))

  ;; Hey, why not set them right here
  (setq month (format-time-string "%m"))
  (setq day   (format-time-string "%d"))
  (setq year  (format-time-string "%Y"))
  (setq time  (format-time-string "%H:%M*%S+'%Z'"))

  ;; If version came from changelog file in a directory, update the
  ;; version files 
  (if (or (string= version-dir "on") (string= comment ""))
      nil

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file version-file)
    (goto-char (point-min))
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (insert (concat version-major "." version-minor))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file patch-file)
    (goto-char (point-min))
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (goto-char (point-min))
    (insert patch-number)
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (setq change-file (concat version-dir "/GlobalChangeLog"))
    (if (file-readable-p change-file)
	  (find-file change-file)
      (find-file change-file)
      (goto-char (point-min))
;;    Sets buffer in Ciao mode: necessary for bindings!  
      (insert "\n:- module(_,_,[assertions]).\n\n")
      (ciao-insert-version-control-off-comment) 
      ;;    This one would be visible by a Ciao program (not needed)
      ;;     (insert "\n:- doc(version_maintenance,off).\n\n")
      )
    (goto-char (point-min))
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (file-relative-name (buffer-file-name original-buffer)))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))
    (switch-to-buffer original-buffer)
    )

  (if (string= comment "")
      ;; If user gave no input comments, do nothing
      (progn 
	(message "Blank comment -- no version change")
	(if (string= version-dir "on")
	    nil
	  (set-mark original-point)
	  (goto-char original-point)
          (save-buffer))
	)
    ;; Else, insert new version
    ;; in current buffer.
    ;; Position ourselves
    (ciao-goto-first-version-comment)
    ;; We are positioned: insert new comment
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (concat comment "\n    (" user-full-name ")") )
    (fill-paragraph nil)
    (set-mark original-point)
    (save-buffer)
    )
  ))

(defun ciao-insert-version-comment 
  (version-major version-minor patch-number month day year time comment)
  "Insert a Ciao changelog entry in file at current point."
  (insert (concat 
		  ":- doc(version(" version-major "*" version-minor "+"
		      patch-number "," year "/" month "/" day ","
		      time "),\n   \"" comment "\").\n\n"))
  (search-backward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(")
  )

(defun ciao-goto-first-version-comment ()
  "Position ourselves at first changelog entry if it exists"
  (goto-char (point-min))
  ;; If previous version exists
  (if (search-forward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(" nil t)
      (beginning-of-line)
    ;; If no previous version exists
;;     (goto-char (point-min))
;;     (if (search-forward-regexp "^[ \t]*:-[ \t\n]*module(" nil t) t t)
;;     (ciao-next-blank-line-or-eof)
    (goto-char (point-max))))

(defun ciao-insert-version-control-off-comment ()
  (insert (concat
		  "\n%% Local Variables: \n"
		  "%% mode: CIAO\n"
		  "%% update-version-comments: \"off\"\n"
		  "%% End:\n\n")))

(defun ciao-insert-assertions-package-reminder ()
  (insert 
   (concat
    "\n"
    "%% *** Delete this comment after reading: it is only a reminder! ***\n"
    "%% \n" 
    "%% The \"assertions\" library needs to be included in order to support\n"
    "%% \":- doc(...,...).\" declarations such as below, i.e., insert: \n"
    "%% \n" 
    "%% :- module(_,_,[assertions]).\n" 
    "%% \n" 
    "%% At the beginning of the file:\n" 
    "%% The following version comment(s) can be moved elsewhere in the \n"
    "%% file. Subsequent version comments will always be placed above \n"
    "%% the last one inserted.\n\n"
    )))

;; (defun ciao-next-blank-line-or-eof ()
;;   (if (search-forward-regexp "^[ \t]*$" nil t)
;;       t
;;     nil))

(defun ciao-fetch-next-changelog-entry () 

   "When a unique version numbering is being maintained across several
files, this command allows inspecting all changes sequentially by
visiting all the files in which the changes were made:

    @begin{itemize}

    @item If in a source file, find the next changelog entry in the
source file, open in another window the corresponding
@file{GlobalChangeLog} file, and position the cursor at the
corresponding entry. This allows browsing the previous and following
changes made, which may perhaps reside in other files in the system.

   @item If in a @file{GlobalChangeLog} file, look for the next entry
in the file, and open in another window the source file in which the
corresponding comment resides, positioning the corresponding comment
at the top of the screen. This allows going through a section of the
@file{GlobalChangeLog} file checking all the corresponding comments in
the different files in which they occur.

    @end{itemize}

"

  (interactive)
  (let ((mbeg 0) (mend 0) original-buffer (version nil))
    (setq original-buffer (current-buffer))
    (if (not (search-forward-regexp 
	      "^[ \t]*:-[ \t]*\\(comment\\|doc\\)([ \t\n]*version("
	      nil t))
	(message "No (more) changelog entries found.")
      (setq mbeg (match-end 0))
      (recenter 0)
      (goto-char mbeg)
      (search-forward-regexp ")[ \t\n]*,")
      (setq mend (- (match-beginning 0) 1))
      (goto-char mend)
      (setq version (buffer-substring-no-properties mbeg mend))
      (if (string-match "GlobalChangeLog" (buffer-name))
	  ;; It is a changelog buffer: find matches in files
	  (progn
	    (search-forward "\"")
	    (setq mbeg (match-end 0))
	    (goto-char mbeg)
	    (search-forward "\"")
	    (setq mend (match-beginning 0))
	    (find-file-other-window (buffer-substring-no-properties mbeg mend))
	    (goto-char (point-min))
	    (search-forward version)
	    (beginning-of-line)
	    (recenter 0)
	    (switch-to-buffer-other-window original-buffer)
	    )
	;; It is a normal buffer: find entry in changelog buffer
	(if (or (string= (ciao-version-maint-type) "on") 
		(string= (ciao-version-maint-type) "off"))
	    (error "No GlobalChangeLog file is associated with this file")
	  (find-file-other-window 
	   (concat (ciao-version-maint-type) "/GlobalChangeLog"))
	  (ciao-mode-nocheck) 
	  ;; set buffer to Ciao mode so that bindings are active!
	  (goto-char (point-min))
	  (search-forward version)
	  (beginning-of-line)
	  (recenter 0)
	  (switch-to-buffer-other-window original-buffer)
	  )
	))))

(defun ciao-handle-version-control-option ()
  "Look and see if there is a local variable designating whether
version control should be performed. If not (and global flag
ciao-ask-for-version-maintenance-type is set to yes) ask the user for
which type and add code to the buffer to set it up."
  (save-excursion
    (let ((option nil) (option-dir nil))
      (cond
       ((not (string= (ciao-version-maint-type) nil))
	;; local var already present: just return
	;; (message (concat "Local var found;value: "
	;;  (ciao-version-maint-type)))
	)
       ((string= ciao-ask-for-version-maintenance-type "yes")
	;; no local var: ask for it (if global flag allows)
	(setq option 
	      (read-string 
	       "Turn on changelog prompting on this file (y/n/q) ?" "q"))
	(if (string= option "q")
	    (setq update-version-comments "off")
	  (goto-char (point-max))
	  (cond
	   ((string= option "n")
	    ;; do not maintain control
	    (ciao-insert-version-control-off-comment)
	    (message "Off - see comments inserted at end of file")
	    (setq update-version-comments "off"))
	   (t 
	    ;; maintain control - normal file
	    (setq option-dir
		  (read-file-name
		   "Name of directory with version file (ret = this file) ?" 
		   "" "on" nil "on"))
	    ;; MR Added to avoid the bug when having control version in a
	    ;; directory which doesn't exist.
	    (if (string= option-dir "on")
		t
	      ;; Make sure the directory exists. If it doesn't exist then
	      ;; create the directory
	      (if (file-directory-p option-dir)
		  t
		(make-directory option-dir)))
	    
	    (ciao-insert-assertions-package-reminder)
	    (insert 
	     (concat
	      "\n:- doc(version_maintenance," 
	      (if (or (equal option-dir "on") (equal option-dir "off"))
		  option-dir
		(concat "dir('" option-dir "')" ))
	      ").\n\n"))
	    (message "On - see comments inserted at end of file")
	    (setq update-version-comments option-dir))
	   )))
       (t (setq update-version-comments "off")
	  )))))

(defun ciao-version-maint-type ()
"Determine the type of version control being done for the file."
  (interactive)
  (if (not (eq update-version-comments 0))
	update-version-comments
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp 
         "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version_maintenance[ \t\n]*,[ \t]*" 
	   nil t)
	  (let ((begin 0))
	    (search-forward-regexp "dir([ \t\n]*'*" nil t)
	    (setq begin (point))
	    (search-forward-regexp "'*[ \t]*)" nil t)
	    (goto-char (match-beginning 0))
	    (setq update-version-comments 
		  (buffer-substring-no-properties begin (point)))
	    (message (concat "DIR: " update-version-comments))
	    )
	(setq update-version-comments nil)
	update-version-comments
        ))))

(defvar ciao-mode-version-control-saving nil)

(defun ciao-mode-version-control ()
  (interactive)
  (if (and (string= (file-name-nondirectory (buffer-file-name))
	            "ciao.el.body")
	   (not ciao-mode-version-control-saving)
	   )
      (progn
	(save-excursion
	  (set-buffer (find-file-noselect "CiaoMode.pl"))
 	  (set-buffer-modified-p t)
	  (ciao-save-buffer)
	  (kill-buffer (current-buffer)))
;; To keep dependencies: touch ciao.el.body afterwards
        (setq ciao-mode-version-control-saving t)
	(sleep-for 1)
	(set-buffer-modified-p t)
	(save-buffer (current-buffer))
        (setq ciao-mode-version-control-saving nil)
	)))

(defun ciao-mode-end-version-control ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (eq filename nil))
	(if (and (string= (file-name-nondirectory filename)
		      "ciao.el.body")
		 (get-buffer "CiaoMode.pl"))
	    (kill-buffer "CiaoMode.pl"))))) 
         
(add-hook 'after-save-hook 'ciao-mode-version-control) 
(add-hook 'kill-buffer-hook 'ciao-mode-end-version-control) 

;;------------------------------------------------------------
;; Splash
;;------------------------------------------------------------

(defun ciao-startup ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
window in Ciao mode, ready to edit, and another one with the Ciao
toplevel. Useful as splash screen for the Ciao program development
system, for example when launching from a desktop (launch emacs,
calling this function)."
  (interactive)
  (let ((tmpfile 
	(concat (ciao-new-temp-code-file ciao-lpdoc-wdir-root) ".pl")))
    (delete-other-windows)
    (run-ciao-toplevel)
    (if ciao-create-sample-file-on-startup
	(progn
	  (find-file tmpfile)
	  (goto-char (point-min))
	  (insert 
"% You can type code in this buffer. 
% Save with \"File->Save Buffer As...\" or \"C-x C-s\".
% Load into toplevel with \"C-c l\"
% Explore menus and buttons above.
% See also Section \"Using Ciao inside GNU emacs\" of the Ciao manual
% (\"CiaoHelp->Ciao system manual\") 

:- module(_,_).

main(Arg) :- 
	write(Arg).

")
	  )
      (switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
      (delete-other-windows)
    )))

;;------------------------------------------------------------
;; The actual Ciao / CiaoPP / LPdoc / &-Prolog / Prolog Mode
;;------------------------------------------------------------

(defun ciao-mode ()
  "
   This is a major mode for 
   editing / debugging / documenting / compiling / running / ...
   Ciao code.

See the Ciao manual (you can use \\<ciao-mode-map>
\\[ciao-goto-ciao-manuals]) for full information on the many features
of this mode.

The following is a summary of the keyboard commands available in the
mode (see also the mode-specific entries in the menu-bar if enabled):

\\{ciao-mode-map}

Entry to this mode calls the value of ciao-mode-hook if that value is
non-nil." 

  (interactive)
  (if (get-buffer-process (current-buffer))
      ; Exit if this is a process buffer (very likely an error)
      (message 
      "Ciao mode not for process buffers, use M-x ciao-inferior-mode instead.")
    (ciao-mode-nocheck)))

(defun ciao-mode-nocheck ()
    (kill-all-local-variables)
    (use-local-map ciao-mode-map)
    (setq major-mode 'ciao-mode)
    ;; MR added to avoid errors in xemacs
    (if (boundp 'xemacs-logo)
	(define-key ciao-mode-map 'backspace 'delete-backward-char))
    (setq mode-name ciao-toplevel-buffer-name)
    (setq case-fold-search nil)
    (set-syntax-table ciao-mode-syntax-table)
    (ciao-mode-variables)
    
    ;; This weird ordering results in same layout in emacs and xemacs...
    (easy-menu-define ciao-menu-help ciao-mode-map 
      "Ciao Mode Help Menus" ciao-mode-menus-help)
    (easy-menu-define ciao-menu-customize ciao-mode-map 
      "Ciao Mode Customization Menus" 
      ciao-mode-menus-customize)
    (easy-menu-define ciao-menu-lpdoc ciao-mode-map 
      "LPdoc Mode Menus" ciao-mode-menus-lpdoc)
    (easy-menu-define ciao-menu-ciaopp ciao-mode-map 
      "CiaoPP Mode Menus" ciao-mode-menus-ciaopp)
    (easy-menu-define ciao-menu-debug ciao-mode-map 
      "Ciao Mode Debug Menus" ciao-mode-menus-debug)
    (easy-menu-define ciao-menu-sys ciao-mode-map 
      "Ciao Mode System Menus" ciao-mode-menus-sys)
  
    (easy-menu-add ciao-menu-sys)
    (easy-menu-add ciao-menu-debug)
    (easy-menu-add ciao-menu-ciaopp)
    (easy-menu-add ciao-menu-lpdoc)
    (easy-menu-add ciao-menu-customize)
    (easy-menu-add ciao-menu-help)

    ;; MR added to support font-lock
    (if (ciao-emacs-cannot-do-font-lock)
	nil
      ;; MH Tool bar stuff (21.1 onwards)
      (if (or (fboundp 'tool-bar-mode) (boundp 'xemacs-logo))
	  (ciao-setup-tool-bar))
      ;; 
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults 
	    '(ciao-mode-font-lock-keywords 
	      t nil nil 
	      ;; Use all buffer refontifying...
	      beginning-of-buffer 
	      (font-lock-mark-block-function . 
 	       ;; Alternative: mark-paragraph
	       ;; Use whole buffer for refontifying...
	       (lambda () 
		(push-mark (point-max))
		(goto-char (point-min)))
	       ))))
    (run-hooks 'ciao-mode-hook))

;; Not necessary to do it this way?
(add-hook
 'ciao-mode-hook
 (function
  (lambda ()
    (define-key ciao-mode-map "\C-x\C-s" 'ciao-save-buffer)
)))

(defun ciao-emacs-cannot-do-font-lock ()
  "We are not capable of fontifying (possible in windowing system,
modern emacses, and also in ascii mode with emacs>= 21.1)."
  (and (not window-system) (not (fboundp 'tool-bar-mode))))

;; Turn on font lock
(if (ciao-emacs-cannot-do-font-lock)
    nil
  (add-hook 'ciao-mode-hook 'turn-on-font-lock)
  (add-hook 'ciao-inferior-mode-hook 'turn-on-font-lock))


;;------------------------------------------------------------
;; Inferior process management
;;------------------------------------------------------------

(defun ciao-inferior-mode ()

  "Inferior mode for interaction with Ciao toplevel, preprocessor,
etc.

This is a major emacs mode used in Ciao-related interactive buffers,
i.e., buffers in which it is possible to interact with an inferior
process running the Ciao top-level, Ciao preprocessor, documenter,
etc.


You can talk to the Ciao top-level or the preprocessor by typing
commands directly in the corresponding buffer as if in a normal
shell. You can also send files or parts of files to be preprocessed or
compiled by the processes running under this inferior mode from any
buffer which is in ciao-mode (see the emacs commands available in such
buffers).

All commands available in standard emacs shell packages (comint) are
available in these interactive buffers. In addition, there are many
new commands which are specific to this mode.  The following is a list
of all the available commands:

\\{ciao-inferior-mode-map}

Entry to this mode calls the value of ciao-mode-hook with no arguments,
qif that value is non-nil.  Likewise with the value of comint-mode-hook.
ciao-mode-hook is called after comint-mode-hook.

"
  (interactive)
  ;; Makes sure Ciao mode is initialized (keymaps, etc.). We then
  ;; override with inferior mode.
  (ciao-mode-nocheck)
  (cond ((not (eq major-mode 'ciao-inferior-mode))
	 (kill-all-local-variables)
	 (comint-mode)
	 ;; Unfortunately variable not in emacs-22 :-(
         (setq comint-highlight-prompt nil) ; avoid unwanted coloring
	 (setq major-mode 'ciao-inferior-mode)
	 (setq mode-name "Ciao/CiaoPP/LPdoc Listener")
	 (setq mode-line-process '(": %s"))
	 (setq comint-input-filter 'ciao-input-filter)
         (set-syntax-table ciao-mode-syntax-table)
	 (ciao-mode-variables)
	 ;; Source debugger stuff
	 (setq ciao-debug-last-line nil)
	 (cond ((string= (buffer-name) 
			 (concat "*" ciao-toplevel-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-debug-filter))
	       ((string= (buffer-name) 
			 (concat "*" ciao-ciaopp-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-ciaopp-filter))
	       ;; Uncomment for supporting hooks in LPdoc
	       ((string= (buffer-name) 
			 (concat "*" ciao-lpdoc-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-lpdoc-filter))
	       ;; This case is usually used in normal shell. The filter is
	       ;; to handle source-level embedded debugger messages
	       (t (set-process-filter (get-buffer-process (current-buffer))
				     'ciao-debug-filter))
	 )
	 (set-process-sentinel (get-buffer-process (current-buffer))
			       'ciao-inferior-process-sentinel)
         ;; 
	 (use-local-map ciao-inferior-mode-map)
	 ;; These are shared:
	 (easy-menu-define ciao-inferior-menu-help ciao-inferior-mode-map 
	   "Ciao Inferior Mode Help Menu" ciao-mode-menus-help) 
	 (easy-menu-add ciao-inferior-menu-help)
	 (easy-menu-define ciao-inferior-menu-customize ciao-inferior-mode-map 
	   "Ciao Mode Customization Menus" ciao-mode-menus-customize)
	 (easy-menu-add ciao-inferior-menu-customize)
	 (easy-menu-define ciao-inferior-menu ciao-inferior-mode-map 
	   "Ciao Mode Menu" ciao-inferior-mode-menus)
	 (easy-menu-add ciao-inferior-menu)
	 (setq comint-prompt-regexp ciao-any-prompt-pattern)
	 
	 ;; MR added to support font-lock
 	 (if (ciao-emacs-cannot-do-font-lock)
 	     nil
	   ;; MH Tool bar stuff (21.1 onwards)
	   (if (or (fboundp 'tool-bar-mode) (boundp 'xemacs-logo))
	       (ciao-setup-inferior-tool-bar))
	   ;; 
 	   (make-local-variable 'font-lock-defaults)
 	   (setq font-lock-defaults 
		 '(ciao-inferior-font-lock-keywords 
		   t nil nil 
		   ;; Use all buffer refontifying...
		   beginning-of-buffer 
		   (font-lock-mark-block-function . 
	           ;; Alternative: mark-paragraph
	           ;; Use whole buffer for refontifying...
	             (lambda () 
		       (push-mark (point-max))
		       (goto-char (point-min))))
		   )))
	 (run-hooks 'ciao-mode-hook)))
  (if (string= "*" (char-to-string (elt (buffer-name) 0)))
      (setq ciao-last-process-buffer-used 
	    (substring (buffer-name) 1 (- (length (buffer-name)) 1)))
    (setq ciao-last-process-buffer-used (buffer-name))
    (rename-buffer (concat "*" (buffer-name) "*")))
  )

(defun ciao-input-filter (str)
  ;; putting "[ \t]*" instead of " *" breaks in xemacs...
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
	((not (eq major-mode 'ciao-inferior-mode)) t)
	((= (length str) 1) nil)	;one character
	((string-match "\\`[rf][ \t]*[0-9]*\\'" str) nil) ;r(edo) or f(ail)
	(t t)))

(defun ciao-insert-logos-toplevel ()
  (ciao-insert-logos ciao-toplevel-buffer-name))

(defun ciao-insert-logos-ciaopp ()
  (ciao-insert-logos ciao-ciaopp-buffer-name))

(defun ciao-insert-logos (buffer-name)
  "Insert a splash screen for the Ciao program development system at
the beginning of the current buffer."
  (set-buffer (concat "*" buffer-name "*"))
  (if window-system
      (let ((beg 0) (end 0))
 	(goto-char (point-min))
 	(setq beg (point))
 	(open-line 3)
 	(next-line 1)
	(ciao-insert-image 'xpm ciao-clip-logo "CLIP")
 	(insert " ")
	(ciao-insert-image 'xpm ciao-logo "Ciao")
 	(setq end (point))
 	;; (put-text-property beg end 'face 'Info-title-1-face)
	(goto-char (point-max))
 	)
    (goto-char (point-max))))

(defun ciao-insert-image (type image default)
  "Portable image insertion (emacs, xemacs). Third argument is text to
be used if images not supported (e.g., in text mode)"
  (let
      (imagefile imagefile-fullpath first-char)
    (setq first-char (substring image 0 1))
    (if (or (string= first-char "/")              ;; /foo 
	    (string= first-char ".")              ;; ./foo
	    (string= first-char "\\")             ;; \foo
	    (string= (substring image 1 2) ":"))  ;; C:foo
	;; Path given: keep in all cases
	(progn
	  (setq imagefile image)
	  (setq imagefile-fullpath image))
      ;; Probably no path: look under icons for emacs, 
      (setq imagefile (concat "icons/" image))
      ;; put full lib path for xemacs
      (setq imagefile-fullpath 
	    (ciao-find-icon (concat "icons/" image))))
    (cond 
     ((and (fboundp 'tool-bar-mode) window-system);; emacs, graphical
      (insert-image 
       (find-image (list (list :type type :file imagefile )))))
     ((and (boundp 'xemacs-logo) window-system);; xemacs, graphical
      (ciao-xemacs-insert-glyph ;; xemacs needs full path
       (make-glyph (vector type :file imagefile-fullpath ))))
     (t ;; text mode
      (insert default)))))

(defun ciao-xemacs-insert-glyph (gl)
  "Insert a glyph at the left edge of point."
  (let ((prop 'ciaoimage)        ;; ciaoimage is an arbitrary name
	extent)
    ;; First, check to see if one of our extents already exists at
    ;; point.  For ease-of-programming, we are creating and using our
    ;; own extents (multiple extents are allowed to exist/overlap at the
    ;; same point, and it's quite possible for other applications to
    ;; embed extents in the current buffer without your knowledge).
    ;; Basically, if an extent, with the property stored in "prop",
    ;; exists at point, we assume that it is one of ours, and we re-use
    ;; it (this is why it is important for the property stored in "prop"
    ;; to be unique, and only used by us).
    (if (not (setq extent (extent-at (point) (current-buffer) prop)))
	(progn
	  ;; If an extent does not already exist, create a zero-length
	  ;; extent, and give it our special property.
	  (setq extent (make-extent (point) (point) (current-buffer)))
	  (set-extent-property extent prop t)
	  ))
    ;; Display the glyph by storing it as the extent's "begin-glyph".
    (set-extent-property extent 'begin-glyph gl)
    ))

(defun ciao ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
single window."
  (interactive)
  (run-ciao-toplevel)
  (switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
  (delete-other-windows)
  )

(defun prolog ()
  "Start up Ciao."
  (interactive)
  (ciao))

(defun run-ciao-toplevel ()

  "Ensure that an inferior Ciao top-level process is running. 

   This opens a top-level window (if one did not exist already) where
queries can be input directly. Programs can be loaded into this top
level by typing the corresponding commands in this window (such as
use_module, etc.), or, more typically, by opening the file to be
loaded in an emacs window
(where it can be edited) and issuing a load command (such as
\\<ciao-mode-map> \\[ciao-load-buffer] or
\\[ciao-load-from-main-module]) directly from there (see the loading
commands of this mode and their bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since execution of
any of the other functions related to the top level (e.g., loading
buffers into the top level) ensures that a top level is started
(starting one if required)."

  (interactive)
  (message "Starting Ciao toplevel... ")
  (ciao-ensure-inferior-process ciao-toplevel-buffer-name)
  (add-hook 'ciao-prompt-inferior-hook 
	    'ciao-insert-logos-toplevel t)
  (message "Starting Ciao toplevel... done."))

(defun ciaopp ()
  "Same as \\<ciao-mode-map> \\[run-ciao-preprocessor], but starts
with a single window.  Useful as splash screen for the Ciao
preprocessor, for example when launching it from a desktop (launch
emacs, calling this function)."
  (interactive)
  (run-ciao-preprocessor)
  (switch-to-buffer (concat "*" ciao-ciaopp-buffer-name "*")) 
  (delete-other-windows)
  )

(defun run-ciao-preprocessor ()
  "Ensure that an inferior Ciao preprocessor process is running. 

   This opens a preprocessor top-level window (if one did not exist
already) where preprocessing commands and preprocessing menu options
can be input directly. Programs can be preprocessed by typing commands
in this window, or, more typically, by opening the file to be
preprocessed in an emacs window (where it can be edited) and issuing a
command (such as \\<ciao-mode-map> \\[ciao-analyze-buffer],
\\[ciao-check-assertions], \\[ciao-optimize-buffer], or
\\[ciao-browse-preprocessor-options]) directly from there (see the
preprocessing commands of this mode and their bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since execution of
any of the other functions related to the top level (e.g., loading
buffers into the top level) ensures that a top level is started
(starting one if required)."
  (interactive)
  (message "Starting Ciao preprocessor... ")
  (ciao-ensure-inferior-process ciao-ciaopp-buffer-name)
  (add-hook 'ciao-ciaopp-prompt-inferior-hook 
	    'ciao-insert-logos-ciaopp t)
  (message "Starting Ciao preprocessor... done."))

;; MH Made it recenter, and then the functions below are trivial
(defun ciao-ensure-inferior-process (buffname)
  (let (origbuff system system-args newbuff)
    (setq origbuff (buffer-name))
    (cond
     ;; Complication, because we are sharing the inferior mode
     ((string= buffname ciao-toplevel-buffer-name)
      (setq system ciao-system)
      (setq system-args ciao-system-args))
     ((string= buffname ciao-ciaopp-buffer-name)
      (setq system ciao-ciaopp-system)
      (setq system-args ciao-ciaopp-system-args))
     ;; *** Temporary until lpdoc-2.0+ (actually, still used for lpdoc-2.0+)
     ((string= buffname ciao-lpdoc-buffer-name)
      (setq system "/bin/tcsh")
      (setq system-args "")
      ;;  (setq system ciao-lpdoc-system)
      ;;  (setq system-args ciao-lpdoc-system-args)
      )
     )
    (setq 
     newbuff
     (if (equal ""
		;; Done differently because of filenames with blanks...
		;; (ciao-get-string-after-blank system)
		system-args
		)
	 (progn 
	   (make-comint buffname 
		      ;; Done differently because of filenames with blanks...
		      ;; (ciao-get-string-before-blank system)
		      system
		      ))
       (make-comint buffname 
		    ;; Done differently because of filenames with blanks...
		    ;; (ciao-get-string-before-blank system) ; command name
		    system
		    nil                                   ; filename
		    ;; Done differently because of filenames with blanks...
		    ;; (ciao-get-string-after-blank system)  ; arguments
		    system-args
		    )))
    (if (string= (buffer-name) (buffer-name newbuff)) ;; We are already there..
	()
      (switch-to-buffer-other-window newbuff))
    (ciao-inferior-mode)
    (goto-char (point-max))
    (if (string= (buffer-name) origbuff) ;; We are already there...
	()
      (switch-to-buffer-other-window origbuff))
    (setq ciao-last-process-buffer-used buffname)))

;; Had to do this differently because of filenames with blanks...
;; (defun ciao-get-string-before-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string 0 (string-match " " string))
;;   string))
;; 
;; (defun ciao-get-string-after-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string (+ (string-match " " string) 1) nil)
;;   nil))

(defun ciao-recenter-last-ciao-buffer () 
  "Recenter the most recently used Ciao inferior process buffer
(top level or preprocessor)."
  (interactive)
  (if ciao-last-process-buffer-used
    (ciao-ensure-inferior-process ciao-last-process-buffer-used)
   (message "No process has been started.")
  ))

(defvar ciao-tmp-calling-buff nil
  "Temp var to pass calling buffer to hooks.")
(defvar ciao-tmp-buffername nil
  "Temp var to pass buffername to hooks.")
(defvar ciao-tmp-command nil
  "Temp var to pass command to hooks.")

;; General interface to subprocess
(defun ciao-send-command (buffername command recenter-opt)
  ;; remember the buffer we are at
  (setq ciao-tmp-calling-buff (buffer-name))
  (save-some-buffers)
  (if (eq (comint-check-proc (get-buffer (concat "*" buffername "*"))) nil)
      (progn
	(ciao-ensure-inferior-process buffername)
	(setq ciao-tmp-buffername buffername)
	(setq ciao-tmp-command command)
	(cond ((string= buffername ciao-toplevel-buffer-name) 
	       (add-hook 'ciao-prompt-inferior-hook 
			 'ciao-do-send-command-global t))
	      ((string= buffername ciao-ciaopp-buffer-name)
	       (add-hook 'ciao-ciaopp-prompt-inferior-hook 
			 'ciao-do-send-command-global t))
	      ((string= buffername ciao-lpdoc-buffer-name)
	       (add-hook 'ciao-lpdoc-prompt-inferior-hook 
			 'ciao-do-send-command-global t))))
    (ciao-do-send-command buffername command recenter-opt))
  ;; MH Added to improve tracking of last inferior buffer used.
  (setq ciao-last-process-buffer-used buffername)
  )

;; Terrible kludge to pass arguments (just for the first command)
(defun ciao-do-send-command-global ()
  (ciao-do-send-command ciao-tmp-buffername ciao-tmp-command nil))

(defun ciao-do-send-command (buffername command recenter-opt)
  (let ((at-menu nil))
    (if (string= (concat (buffer-name (current-buffer))) 
		 (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))
	(progn
	  ; (set-buffer (concat "*" buffername "*"))
	  (switch-to-buffer (concat "*" buffername "*"))
	  (setq at-menu t)
	  )
      (switch-to-buffer-other-window (concat "*" buffername "*")))
      ; (set-buffer (concat "*" buffername "*"))
    (goto-char (point-max))
    (if (eq recenter-opt t) 
	(recenter 0))
    (insert command)
    (comint-send-input)
    (if at-menu
	; ()
	(switch-to-buffer (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))
      (switch-to-buffer-other-window ciao-tmp-calling-buff))
    ))

;; MH Alternative (but doesn't work?)
;; (defun ciao-send-command (buffername command)
;;   (comint-proc-query buffername command))

;;------------------------------------------------------------
;; Locating errors
;;------------------------------------------------------------

(defun ciao-any-errors ()
  "True if there were any errors in the previous run."
  (save-excursion
    (let (process-buffer)
      (setq process-buffer (concat "*" ciao-last-process-buffer-used "*"))
      (if (and ciao-last-process-buffer-used 
	       (get-buffer process-buffer)) ;; buffer still exists
	  (progn
	    ;; Go to process buffer
	    (set-buffer process-buffer)
	    (goto-char (point-max))
	    (move-to-column 0) ;; skip prompt if at prompt
	    ;; Go back to previous prompt or beginning of buffer
	    (search-backward-regexp ciao-any-prompt-pattern nil t)
	    (end-of-line)
	    (not (ciao-no-more-errors)))))))

(defun ciao-no-more-errors ()
  (or (not (search-forward-regexp (ciao-error-or-prompt-pattern) nil t))
      (string=  (buffer-substring-no-properties 
		 (- (point) (length ciao-prompt)) (point))
		ciao-prompt)
      (string=  (buffer-substring-no-properties 
		 (- (point) (length ciao-ciaopp-prompt)) (point))
		ciao-ciaopp-prompt)
      (eq (string-match ciao-os-shell-prompt-pattern
		    (buffer-substring-no-properties 
		     (match-beginning 0) (match-end 0))
		    ) 0)
      ))

(defun ciao-find-last-run-errors ()
  "Go to the location in the source file containing the next error reported by
the last Ciao subprocess (preprocessor or toplevel) which was run."
  (interactive)
  (let ((process-buffer (concat "*" ciao-last-process-buffer-used "*")))
    (if (and ciao-last-process-buffer-used 
	     (get-buffer process-buffer))
	(if ciao-finding-errors
	    (progn
	      ;; Go to process buffer
	      (if (string= (buffer-name (current-buffer)) process-buffer)
		  ()
		(switch-to-buffer-other-window process-buffer)
		)
	      (ciao-find-error process-buffer))
	  ;; Mark that we are starting a finding errors session
	  (setq ciao-finding-errors (current-buffer))
	  ;; Go to process buffer, split in two
	  (if (string= (buffer-name (current-buffer)) process-buffer)
	      ()
	    ;; Start with a single window
	    (delete-other-windows)
	    (switch-to-buffer-other-window process-buffer)
	    )
	  (goto-char (point-max))
	  (move-to-column 0) ;; skip prompt if at prompt
	  ;; Go back to previous prompt or beginning of buffer
	  (search-backward-regexp ciao-any-prompt-pattern nil t)
	  (end-of-line)
	  (ciao-find-error process-buffer))
      (message "No recent program processing active."))
    ))

(defun ciao-find-error (process-buffer)
  "Go to location in source file containing next error, highlight."
  (let (beginline endline filename error)
    ;; first repaint (eliminates any previous error marks in buffer)
    ;; No need to do anything if file is not being visited any more
    (if (and ciao-previous-error
	     (get-file-buffer (car (cdr (cdr ciao-previous-error)))))
	(progn 
	  (set-buffer (get-file-buffer 
		       (car (cdr (cdr ciao-previous-error)))))
	  (if (> (car ciao-previous-error) 0)
	      (progn 
		(ciao-uncolor (car ciao-previous-error)
			      (car (cdr ciao-previous-error))
			      'ciao-error)))
	  (setq ciao-previous-error nil)))
    (set-buffer process-buffer)
    ;; In process buffer, get error data
    (setq error (ciao-get-next-error-data))
    (if (eq error nil)
	;; There are no (more) errors
	(progn
	  (goto-char (point-max)) ;; goto end of process buffer
	  ;; Return to original buffer if not already there
	  (if (eq ciao-finding-errors (current-buffer))
	      ()
	    (switch-to-buffer-other-window ciao-finding-errors))
	  ;; MH Put this back in to return to single original window 
	  ;; *** (delete-other-windows) ***
	  (setq ciao-finding-errors nil)
	  (message "There were no (more) errors."))
      ;; Error located, get info, go to file, if known.
      (setq beginline (car error))
      (setq endline (car (cdr error)))
      (setq filename (car (cdr (cdr error))))
      (if (eq filename nil)
	  (message "No corresponding file could be determined.")
	(find-file-other-window filename)
	(if (< beginline 0)
	    ;; No line numbers: just visit file
	    (progn 
	      (goto-char (point-min))
	      (message "Error within this file.")
	      (setq ciao-previous-error nil))
	  ;; Else, highlight region in opened file...
	  (push-mark (point) t)
	  (goto-line beginline)
	  (recenter 0)
	  (ciao-color beginline endline 
		      ciao-face-highlight-code 'ciao-error)
	  (setq ciao-previous-error error)
	  (goto-line (+ endline 1))
	  (backward-char 1)
	  (message "Mark set")
	  )
	))))

(defun ciao-get-next-error-data ()
  "Locates next error, and highlights it. Returns:
     nil -- if no more errors
     '(beginline endline file) -- if an error found, where
        beginline/endline = location of error in process buffer
        file = source file containing error (if nil: no file was located)"
;; ALT:
;;         beginline/endline = can also contain predicate name / clause number
;;             (this is a temporary kludge while proprocessor error
;;              reporting is improved)

  ;; If we have a previous error found and colored, uncolor it
  (if ciao-inferior-error
      (progn
	(ciao-uncolor ciao-inferior-error
		      ciao-inferior-error
		      'ciao-error)
	(goto-line ciao-inferior-error)
	(setq ciao-inferior-error nil)))

;; From 21.1 on , this does not go over the prompt. Using column instead:
;;  (beginning-of-line)
;;  (move-to-column 0)
  (end-of-line)
  (if (ciao-no-more-errors)
      ;; No (more) errors found
      (setq ciao-inferior-error nil)
    nil
    (let ((messpoint (point)) beginline endline openpoint filename)
      (recenter 1)
      (move-to-column 0)
      (if (not (search-forward "lns " (+ (point) 80) t))
;; MH OLD
	      ;; No line number info: -1 -1
	      (progn
		(setq beginline -1)
		(setq endline -1))
;; MH ALT
;;  	  (if (not (search-forward " at " (+ (point) 80) t))
;; 	      ;; No line number info: -1 -1
;; 	      (progn
;; 		(setq beginline -1)
;; 		(setq endline -1))
;; 	    ;; locate by e.g. "at partition/4/3/1" 
;; 	    ;; This is a kludge while messages from preprocessor improve
;;  	    (let ((beg (point)) predicate clausenumber)
;;  	      (search-forward "/")
;;  	      (backward-char 1)
;;  	      (setq predicate (buffer-substring-no-properties beg (point)))
;;  	      (forward-char 1)
;;  	      ;; ignore arity (approximation)
;;  	      (search-forward "/")
;;  	      (setq beg (point))
;;  	      (search-forward "/")
;;  	      (setq clausenumber
;;  		    (string-to-number (buffer-substring-no-properties beg
;;  								   (point))))
;; 	      ;; MH ***
;; 	      (message (append "ERROR DATA: " predicate " "
;; 			       (int-to-string clausenumber )))
;; 	      ;; This typically done elsewhere, but kludge to get line numbers
;; 	      (save-excursion 
;; 		(find-file-other-window filename)
;; 		(search-forward-regexp (concat "^" beginline) nil t endline)
;; 		(setq beginline (point))
;; 		(search-forward-regexp (concat "\\(^" beginline "\\|^$\\)")  nil t)
;; 		(setq endline (point)))
;; 	      )
;; 	    (progn 
;; 	      (setq beginline -1)
;; 	      (setq endline -1))
;; 	    )
	;; Get line number info.
;;	(search-forward "lns " (+ (point) 80) t)
	(let ((beg (point)))
	  (search-forward "-")
	  (backward-char 1)
	  (setq beginline 
		(string-to-number (buffer-substring-no-properties beg (point)))))
	(forward-char 1)
	(let ((beg (point)))
	  (search-forward ")")
	  (backward-char 1)
	  (setq endline 
		(string-to-number (buffer-substring-no-properties beg (point)))))
	)
      ;; Beginning of ERROR/WARNING/... line
      (move-to-column 0)
      (setq ciao-inferior-error (ciao-what-line))
      (ciao-color ciao-inferior-error
		  ciao-inferior-error
		  ciao-face-highlight-code
		  'ciao-error)

      ;; Try to find opening "{" by inserting a "}"
      (insert "}")
      ;; Change syntax of parenthesis
      (modify-syntax-entry ?( "_")
      (modify-syntax-entry ?) "_")
      (modify-syntax-entry ?[ "_")
      (modify-syntax-entry ?] "_")
      ;; Scan to "{"
      (condition-case nil
	  (setq openpoint (scan-sexps (point) -1))
	(error (setq openpoint 0)))
      ;; Return syntax of parenthesis
      (modify-syntax-entry ?( "()")
      (modify-syntax-entry ?) ")(")
      (modify-syntax-entry ?[ "(]")
      (modify-syntax-entry ?] ")[")      
      ;; Delete the "}" inserted
      (delete-char -1)
      (if (= openpoint 0)
	  (setq filename nil)
	(goto-char openpoint)
	(search-forward "/")
	(backward-char 1)
	(let ((beg (point)))
          (search-forward-regexp 
	   "\\(\\.\\(po\\|itf\\|asr\\|pls\\|pl\\|cgi\\)\\>\\|$\\)")
          (setq filename 
		(fix-cygwin-drive-letter
		 (concat (buffer-substring-no-properties 
			  beg (match-beginning 0)) 
			 ;; MH cygdrive case for .pls, fixed bug
			 (cond
			  ((string= (funcall ciao-match-string 0) ".po") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".itf") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".asr") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".pls") 
			   ".pls")
			  ((string= (funcall ciao-match-string 0) ".pl") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) "cgi") 
			   ".cgi")
			  ((string= (funcall ciao-match-string 0) "") 
			   "")
			  )))))
	(goto-char messpoint)
        ;; (beginning-of-line)
	(move-to-column 0)
	)
      (cons beginline (cons endline (cons filename nil)))
      )))

;; MH cygdrive Fixed for newer version of cygwin
;; MH //c/ and also /cygdrive/
(defun fix-cygwin-drive-letter (filename)
  (if (eq (string-match "//./" filename) 0)
      (concat (substring filename 2 3) ":" (substring filename 3))
    (if (eq (string-match "/cygdrive/" filename) 0)
	(concat (substring filename 10 11) ":" (substring filename 11))
      filename
    )))

(defun ciao-unmark-last-run-errors()
  "Remove error marks from last run (and also debugging marks if present)."
  (interactive)
  (if ciao-last-process-buffer-used
      (save-excursion
	(setq ciao-finding-errors nil)
	(if ciao-previous-error
	    (let ((error-buffer
		   (get-file-buffer 
			   (car (cdr (cdr ciao-previous-error))))))
	      (if (not error-buffer) ;; nil=buffer does not exist any more
		  ()
		(set-buffer error-buffer)
		(ciao-uncolor (car ciao-previous-error)
			      (car (cdr ciao-previous-error))
			      'ciao-error)))
	  (message "No error mark(s) found.")
	  (setq ciao-previous-error nil))
	(if ciao-inferior-error
	    (let ((last-buffer
		   (concat "*" ciao-last-process-buffer-used "*")))
	      (if (get-buffer last-buffer) ;; else already deleted
		  (progn
		    (set-buffer last-buffer)
		    (ciao-uncolor ciao-inferior-error
				  ciao-inferior-error
				  'ciao-error)))
	      (setq ciao-inferior-error nil)))
	;; This returns nil if not debugging, so it does not hurt and
	;; is handy
	(ciao-debug-remove-marks)
	)
    (message "No recent program processing active.")
    ))

;;------------------------------------------------------------
;; Assertions and syntax cheking
;;------------------------------------------------------------

(defun ciao-check-buffer-syntax ()

  "Check the @em{syntax} of the code and assertions in the current
buffer, as well as imports and exports.  This uses the standard top
level (i.e., does not call the preprocessor and thus does not require
the preprocessor to be installed). Note that full (semantic) assertion
checking must be done with the preprocessor."

  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (if (and ciao-assrt-lib-loaded ;; if lib loaded and process still running...
	   (comint-check-proc 
	    (get-buffer-create (concat "*" ciao-toplevel-buffer-name "*"))))
      (ciao-do-check-buffer-syntax)
    (ciao-ensure-inferior-process ciao-toplevel-buffer-name)
    (add-hook 'ciao-prompt-inferior-hook 'ciao-load-assrt-lib t)
    (add-hook 'ciao-prompt-inferior-hook 'ciao-do-check-buffer-syntax t)
    )
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t)))

(defun ciao-load-assrt-lib ()
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   "use_module(library('assertions/assrt_lib'))."
   t)
  (setq ciao-assrt-lib-loaded t))

(defun ciao-do-check-buffer-syntax ()
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "prolog_flag(verbose_compilation,_Old,off),"
           "check_code_and_assrt_syntax('" (buffer-file-name) "'),"
           "prolog_flag(verbose_compilation,_,_Old)." 
	   )
   t))

;;------------------------------------------------------------
;; Some aid for inserting text (very limited for now)
;;------------------------------------------------------------

(defun ciao-insert-script-header ()

  "Insert a (Unix) header at the top of the current buffer so that the
Ciao script interpreter will be called on this file if @em{run} from
the command line. It also makes the file ``executable'' (e.g.,
'@tt{chmod +x <file>}' in Unix). See @ref{The script interpreter} for
details."

  (interactive)
  (goto-char (point-min))
  (insert 
   (concat "#!/bin/sh\n"
	   "exec ciao-shell $0 \"$@\" # -*- mode: ciao; -*-\n"
	   "\n"))
  (set-file-modes (buffer-file-name) 448))

(defun ciao-indent-file ()

  "Indent a Ciao or Prolog file."
  
  (interactive)
  (setq tmp_file_1 (concat (buffer-file-name) ".1.tmp"))
  (setq current-point (point))
  (setq current-start (window-start))
  (setq source-buffer (current-buffer))
  (with-temp-file tmp_file_1
    (insert-buffer source-buffer))
  (shell-command (concat (concat ciao-bin-dir "/plindent-1.14") " " tmp_file_1 " -") source-buffer)
  (delete-file tmp_file_1)
  (set-window-start (selected-window) current-start)
  (goto-char current-point)
  )


;;------------------------------------------------------------
;; Preprocess buffer - CiaoPP graphical menu instrumental variables
;;------------------------------------------------------------

(defvar ciao-widget-str-list nil
  "This variable accummulates the text that
ciao-ciaopp-process-graphical-menu gets with the previous calls to
this hook in order to be able to parse it (needed since Emacs returns
pieces of strings, like: 'men' 'u option: ' '[a,b]' '? ', at each
call.")

(defvar ciao-widget-id 0
   "Every time a line that matches a menu string (function is_a_menu)
is printed, a new graphical menu widget is created. These widgets are
identified by a widget identifier, that is currently a number. This
number is the same as the line number that was used to create the
widget. This widget id is used to find out in which buffer-based menu
question the value of the widget has to be typed.")

(defvar ciao-widget-last-changed-option -1
  "This variable identifies which widget was modified. The algorithm
will introduce answers in the CiaoPP buffer (as is the user had typed
them) while the current menu question number is less or equal that the
value of this variable.")

(defvar ciao-widget-values nil
   "A list that contains the current values of the widgets created. It
should look like: '((1 option_widget1) (2 option_widget2)).")

(defvar ciao-cancel-widget-values nil
  "Each time the menu is started, this variable saves the current menu
values. This is in order to be able to restore these values will be
restored if the cancel button is pressed in the graphical menu.")

(defvar ciao-gm-recovering 0
  "Sometimes the CiaoPP top-level menu can generate a 'Note: Incorrect
Option'. When this happends, the ciao-gm-recovering variable is
increased in 1 unit, and the graphical menu hook
(ciao-ciaopp-process-graphical-menu) starts inserting carriage returns
in to avoid more errors. When a prompt is reached, the current prompt
hook (ciao-ciaopp-show-graphic-menu) will restart the menu process if
this variable is greater than 0. After Incorrect Option is obtained
twice, it will stop and print an error mesage notifying user the about
the error.")

;;------------------------------------------------------------
;; Preprocess buffer (including CiaoPP graphical menu)
;;------------------------------------------------------------

;; (defun ciao-preprocess-buffer-menu ()
;;   "Preprocess the buffer, selecting options. Instructs the
;; preprocessor to load the current buffer and start an interactive
;; dialog in which the different options available in the preprocessor
;; can be set. "
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'menu nil))
;; 
;; (defun ciao-preprocess-buffer ()
;;   "Preprocess the buffer, using the previously selected options. If no
;; options were set previously, then the preprocessor defaults are used."
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'nomenu nil))
;; 
;; (defun ciao-preprocess-buffer-and-show-output ()
;;   "Preprocess the buffer, using the previously selected (or default)
;; options, waits for preprocessing to finish and displays the
;; preprocessor output (leaving the cursor at the same point if already
;; on a preprocessor output file). This allows running the preprocessor
;; over and over and watching the output while modifying the source
;; code."
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'nomenu t))
;; 
;; (defun ciao-check-types-modes ()
;;   "Uses the preprocessor to perform compile-time checking of types and
;; modes (pptypesfd and shfr analyses). "
;;   (interactive)
;;   (message "Checking types and modes... ")
;;   (ciao-do-preprocess-buffer 'typesmodes nil))

(defun ciao-analyze-buffer ()
  "Call the preprocessor to perform a number of pre-selected analyses
on the current buffer (and related modules)."
  (interactive)
  (message "Analyzing buffer... ")
  (ciao-do-preprocess-buffer 'analyze t))

(defun ciao-check-assertions ()
  "Call the preprocessor to perform compile-time checking of the
assertions (types, modes, determinacy, nonfailure, cost, ...) in the
current buffer (and against those in related modules)."
  (interactive)
  (message "Checking assertions... ")
  (ciao-do-preprocess-buffer 'checkassrt nil))

(defun ciao-optimize-buffer ()
  "Uses the preprocessor to perform optimizations (partial evaluation,
abstract specialization, parallelization, ...) on the current buffer
(and related modules)."
  (interactive)
  (message "Optimizing buffer... ")
  (ciao-do-preprocess-buffer 'optimize t))

(defun ciao-browse-preprocessor-options ()
  "Browse and select (using the preprocessor menus) the actions to be
performed by the preprocessor when performing analisys used by
\\<ciao-mode-map> \\[ciao-] \\[ciao-analyze-buffer],
\\[ciao-check-assertions], \\[ciao-optimize-buffer], and the
corresponding toolbar buttons."
  (interactive)
  (message "Browsing preprocessor options... ")
  (ciao-remember-last-menu-key  -1)
  (setq ciao-ciaopp-prog-lang 0)
  (ciao-do-preprocess-buffer 'customize t))

(defun ciao-do-preprocess-buffer (action showoutput)
  "Main function to call the preprocessor. Implements the others via options."
  (message "Preprocessing buffer... ")
  (if (not (string= (buffer-name (current-buffer)) 
		    (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))) 
      (progn
;	(setq ciao-gm-recovering 0)
	(setq ciao-last-source-buffer-used 
	      (current-buffer))
	(setq ciao-g-showoutput showoutput)))
  (setq ciao-gm-recovering 0)
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-ciaopp-buffer-name 
    (cond
;    ((eq action 'menu)        (ciao-build-ciaopp-command "[]"))
;    ((eq action 'nomenu)      (ciao-build-ciaopp-command nil ))
;    ((eq action 'typesmodes)  (ciao-build-ciaopp-specific-command "ctcheck"))
     ((eq action 'checkassrt)  (ciao-build-ciaopp-specific-command 
				"auto_check_assert"))
     ((eq action 'analyze)     (ciao-build-ciaopp-specific-command
				"auto_analyze"))
     ((eq action 'optimize)    (ciao-build-ciaopp-specific-command
				"auto_optimize"))
     ((and ciao-ciaopp-use-graphical-menu
      (or
       (eq action 'customize)
       (eq action 'customize-no-set-hook)))

      (if (eq ciao-ciaopp-prog-lang 0)           
	  "customize(all)."
	  "customize_java(all)."
      )
     )

     ; this action has been though to be called from the OK button
     ; hook of the graphic menu AND only after customize action
     ((or (eq action 'customize-and-exec) 
	  (and (eq action 'customize)
	       (not ciao-ciaopp-use-graphical-menu)))

      (if (not (eq ciao-last-source-buffer-used 'nil))

	  (if (eq ciao-ciaopp-prog-lang 0)           
	    (concat "customize_and_exec('" 
		  (buffer-file-name ciao-last-source-buffer-used) "').")
 
	    (concat "customize_and_exec_java('" 
		  (buffer-file-name ciao-last-source-buffer-used) "').")
	    )
	(error "INTERNAL ERROR: cannot find the last source buffer used!")
	nil))

     (t nil))
    t)

  
  (if ciao-ciaopp-use-graphical-menu
      (if (eq action 'customize-and-exec)
	  (progn 
	    (setq ciao-widget-str-list   nil)
	    (setq ciao-widget-id           0)
	    (setq ciao-ciaopp-gmenu-hook 
		  'ciao-ciaopp-process-gmenu-just-write-answers))
	(if (or (eq action 'customize) (eq action 'customize-no-set-hook))
	    (progn 
	      (setq ciao-widget-str-list   nil)
	      (setq ciao-widget-id           0)
	  
	      (if (eq action 'customize)
		  (progn
		    (setq ciao-ciaopp-gmenu-hook 
			  'ciao-ciaopp-process-graphical-menu)
		    (add-hook 'ciao-ciaopp-prompt-inferior-hook 
			      'ciao-ciaopp-show-graphic-menu t))))
	  (if showoutput
	      (add-hook 'ciao-ciaopp-prompt-inferior-hook 
			'ciao-find-errors-or-show-output t)
	    (if ciao-locate-errors-after-run
		(add-hook 'ciao-ciaopp-prompt-inferior-hook 
		  'ciao-launch-find-last-run-errors-from-orig-buffer t)))))
    ; this means: if cannot use graphical menu, then put the hooks
    ; (switch-to-buffer (exe-buffer-name ciao-ciaopp-buffer-name))
    (if showoutput
	(add-hook 'ciao-ciaopp-prompt-inferior-hook 
		  'ciao-find-errors-or-show-output t)
      (if ciao-locate-errors-after-run
	  (add-hook 'ciao-ciaopp-prompt-inferior-hook 
		    'ciao-launch-find-last-run-errors-from-orig-buffer t)))))

(defun java-browse-preprocessor-options ()
  "Browse and select (using the preprocessor menus) the actions to be
performed by the preprocessor when performing analisys used by the
corresponding toolbar buttons."
  (interactive)
  (message "Browsing preprocessor options... ")
  (ciao-remember-last-menu-key  -1)
  (setq ciao-ciaopp-prog-lang 1 )
  (message "Sets the ciao-ciaopp-prog-lang variable to Java")
  (ciao-do-preprocess-buffer 'customize t)
  )

(defun ciao-ciaopp-process-graphical-menu (line-str)
  (let* ( (new-line (concat ciao-widget-str-list line-str))
	  widget-mark
	  default_opt)
    
    (if (<= ciao-widget-id ciao-widget-last-changed-option)
	(setq default_opt (ciao-get-graphic-menu-option ciao-widget-id))
      (setq default_opt 'nil))
    
    (save-excursion
      ; we do have to create the buffer (INIT process)
      (if (and (eq ciao-widget-id 0)
	       (is_a_menu new-line)
;;	       (eq (get-buffer (exe-buffer-name
;;                              ciao-ciaopp-gmenu-buffer-name)) 'nil) 
	       )
	  (ciao-create-widgets-buffer)
	(set-buffer (get-buffer-create 
		     (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))))
      
      (goto-char (point-max))
      (setq widget-mark 
	    (ciao-create-widget ciao-widget-id new-line default_opt))
      (setq ciao-widget-str-list (substring new-line widget-mark)))
    
      ; TRICK: As menu is always waiting for an input, menu is always
      ; the last thing we can expect
	(if (is_a_menu new-line)
	    (let ( (def_opt (ciao-get-toplevel-menu-default-option 
			     (match-string 3 new-line))) )
		; we do have to rewrite the options because in the new pass
	        ; they do not have meaning anymore. For example:
	        ; (Format: QX-> Question X. AY -> AnswerY (Widget ID))
	        ; QA: AA (1)
		; QB: AB (2)
	        ; QC: AC (3)
	        ; if we change AB (Answer B), with the widget id 2, on the 
	        ; new menu, 3 will be probably QD, that is not QC.

	      (goto-char (point-max))
	  
	      (if (and (eq ciao-gm-recovering 0)
		       (<= ciao-widget-id ciao-widget-last-changed-option))
		       (insert (ciao-remove-all-spaces default_opt))
		(ciao-change-graphic-menu-option ciao-widget-id def_opt))
	      
;	      (prin1 (list "Current id:" ciao-widget-id " last id: "
;                     ciao-widget-last-changed-option 
;		      " values: " ciao-widget-values " Rec: "
;                     ciao-gm-recovering "\n")) 

	      (setq ciao-widget-id (1+ ciao-widget-id))
	      (comint-send-input)))))

(defun ciao-ciaopp-process-gmenu-just-write-answers (line-str)
  (let* ( (new-line       (concat ciao-widget-str-list line-str))
	  (ciaoppbuffname (concat "*" ciao-ciaopp-buffer-name "*"))
	  widget-mark
	  default_opt)

 ; TRICK: As menu is always waiting for an input, menu is always the
 ; last thing we can expect (Note *other things* are notes or errors
    (if (is_a_menu new-line)
	(let ( (the_mark (match-end 3))
	       (def_opt (ciao-get-toplevel-menu-default-option 
			 (match-string 3 new-line))))

	  (setq ciao-widget-str-list 
		(substring new-line (+ 3 the_mark)))
		; we do have to rewrite the options because in the new pass
	        ; they dont have meaning anymore. For example:
	        ; (Format: QX-> Question X. AY -> AnswerY (Widget ID))
	        ; QA: AA (1)
		; QB: AB (2)
	        ; QC: AC (3)
	        ; if we change AB (Answer B), with the widget id 2, on the 
	        ; new menu, 3 will be probably QD, that is not QC.

	  (goto-char (point-max))
	  
	  (if (<= ciao-widget-id ciao-widget-last-changed-option)
	      (insert (ciao-remove-all-spaces 
		(ciao-get-graphic-menu-option ciao-widget-id)))
	    (insert 'nil))
	  
	  (setq ciao-widget-id (1+ ciao-widget-id))

	  ; if we have answered all the questions => Stop the hooks
	  (if (eq ciao-widget-id ciao-widget-last-changed-option)
	      (progn
		; *** (switch-to-buffer-other-window (concat "*"
		;      ciao-ciaopp-buffer-name "*")) 
		(set-buffer (concat "*" ciao-ciaopp-buffer-name "*"))
		(kill-buffer (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))
		(setq ciao-ciaopp-gmenu-hook nil)
		(setq ciao-cancel-widget-values nil)

		(ciao-ciaopp-end-graphic-menu)
		(comint-send-input))
	    (comint-send-input)))
      (setq ciao-widget-str-list new-line))))

(defun ciao-ciaopp-show-graphic-menu ()
  "Shows the graphic ciaopp menu process hook"
  (setq ciao-ciaopp-gmenu-hook nil)

;    (save-excursion
; ***      (switch-to-buffer-other-window (concat "*"
;          ciao-ciaopp-buffer-name "*")) 
;      (switch-to-buffer (get-buffer-create (exe-buffer-name
;       ciao-ciaopp-gmenu-buffer-name))) 
      (if (string= (buffer-name (window-buffer (selected-window))) 
		   (concat "*" ciao-ciaopp-buffer-name "*"))
	  (switch-to-buffer (exe-buffer-name ciao-ciaopp-gmenu-buffer-name))
	(set-buffer     (get-buffer-create (exe-buffer-name
					    ciao-ciaopp-gmenu-buffer-name))) 
	(display-buffer (get-buffer-create (exe-buffer-name
					    ciao-ciaopp-gmenu-buffer-name))) 

	(if (eq ciao-cancel-widget-values 'nil)
	    (progn
	      (setq ciao-cancel-widget-values 
		    (copy-sequence ciao-widget-values)))))
      (goto-char (point-max))
      (ciao-create-widgets-buttons)
      (ciao-run-widget-buffer)
;      )
)


(defun ciao-ciaopp-end-graphic-menu ()
  "Removes the graphic ciaopp menu process hook"
  (setq ciao-ciaopp-gmenu-hook nil)

  (if ciao-g-showoutput
      (add-hook 'ciao-ciaopp-prompt-inferior-hook 
		'ciao-find-errors-or-show-output t)
    (if ciao-locate-errors-after-run
	(add-hook 'ciao-ciaopp-prompt-inferior-hook 
		  'ciao-launch-find-last-run-errors-from-orig-buffer t))))


(defun ciao-widget-hook (widget &rest ignore)
  "This funcion is invoked whenever a widget is changed."
  (let* (
	 (value     (widget-value widget))
	 (key       (widget-get   widget :widgetid))
	 )
    (ciao-change-graphic-menu-option key value)
    (ciao-remember-last-menu-key     key)
  
    ; FOR TESTING!!!
    ; (if (eq ciao-gm-recovering 0)
    ;     (ciao-change-graphic-menu-option key "potatoe"))

    ; Now invokes the CiaoPP menu till the question numbered key
    (ciao-do-preprocess-buffer 'customize nil)))

(defun ciao-ok-button-widget-hook (widget &rest ignore)
  "This funcion is invoked when the OK button is released."

   ; Remember the number of answers we have to write
   (ciao-remember-last-menu-key ciao-widget-id)
 
   ; Now invokes the CiaoPP menu until the question numbered key
   (ciao-do-preprocess-buffer 'customize-and-exec nil))


(defun ciao-cancel-button-widget-hook (widget &rest ignore)
  "This funcion is invoked when the OK button is released."
  
  ; Remember the number of answers we have to write
  (ciao-remember-last-menu-key ciao-widget-id)
  
  (setq ciao-ciaopp-gmenu-hook
	'ciao-ciaopp-process-gmenu-just-write-answers)
  
  ; Remember the number of answers we have to write
  (ciao-remember-last-menu-key (length ciao-cancel-widget-values))

  ; Restore the values of the menu before starting to play with it
  (setq ciao-widget-values (copy-sequence ciao-cancel-widget-values))
  
  ; Now invokes the CiaoPP menu till the question numbered key
  (ciao-do-preprocess-buffer 'customize-no-set-hook nil))

(defun ciao-change-graphic-menu-option (key newvalue)
  "We have an alist in the variable ciao-widget-values. Whenever we
change a value, we create a new alist with the new key an newvalue and
save it in the same symbol."
  (let ((par (cons key newvalue)))
    (if (boundp 'ciao-widget-values)
	(setq ciao-widget-values
	      (cons par (assq-delete-all  key ciao-widget-values)))
      (setq ciao-widget-values (list par)))))

(defun ciao-get-graphic-menu-option (key)
  "Returns the value of the widget-id 'key'."
  (if (boundp 'ciao-widget-values)
      (cdr (assoc key ciao-widget-values))
    'nil))

(defun ciao-remember-last-menu-key (key)
  (setq ciao-widget-last-changed-option key))

(defun ciao-launch-find-last-run-errors-from-orig-buffer ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (ciao-find-last-run-errors))

(defun ciao-find-errors-or-show-output ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (if (and ciao-locate-errors-after-run (ciao-any-errors))
      (ciao-find-last-run-errors)
    (ciao-show-preprocessor-output)
    ;; In this case, probably best to go back to original buffer
    (switch-to-buffer-other-window ciao-last-source-buffer-used)))

;; The following are obsolete with new option browser.
;; 
;; (defun ciao-set-ciaopp-output-pred ()
;;   "Make ciaopp output only predicate-level analysis information."
;;   (interactive)
;;   (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(pred)." t))
;; 
;; (defun ciao-set-ciaopp-output-full ()
;;   "Make ciaopp output both literal- and predicate-level analysis information."
;;   (interactive)
;;   (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(yes)." t))
;; 
;; (defun ciao-set-ciaopp-output-none ()
;;   "Make ciaopp output no analysis information."
;;   (interactive)
;;   (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(no)." t))

(defun ciao-build-ciaopp-command (options)
  (concat "precompile('" (buffer-file-name)
	  (if (string= options nil)
	      "')."
	    (concat "'," options ").") )))

(defun ciao-build-ciaopp-specific-command (command-name)
  (concat command-name "('" (buffer-file-name) "').") )

(defun ciao-show-preprocessor-output ()
  "Show last output file produced by Ciao preprocessor. The preprocessor
works by producing a file which is a transformed and/or adorned (with
assertions) version of the input file. This command is often used after
running the preprocessor in order to visit the output file and see the
results from running the preprocessor."
  (interactive)
  (let ((ciaoppbuffname (concat "*" ciao-ciaopp-buffer-name "*"))
	(origbuffer (current-buffer)))
    (if (not (get-buffer ciaoppbuffname))
	(message "Preprocessor buffer not active.")
      (if (string= (buffer-name (current-buffer)) ciaoppbuffname)
	  ()
	(switch-to-buffer-other-window ciaoppbuffname))
      (save-excursion
	(let ((mbeg 0) (mend 0) (file nil))
	  (goto-char (point-max))
	  (move-to-column 0) ;; skip prompt if at prompt
;;   	  (search-backward-regexp ciao-any-prompt-pattern nil t)
;; It is safe (and more precise) to be more specific here:
	  (search-backward-regexp ciao-ciaopp-prompt-pattern nil t)
	  (end-of-line)
	  (if (search-forward-regexp "written file " nil t)
	      (progn
		(setq mbeg (match-end 0))
		(goto-char mbeg)
		(search-forward-regexp "}")
		(setq mend (match-beginning 0))
		(setq file (buffer-substring-no-properties mbeg mend))
		(if (get-file-buffer file)
		    ;; The complication is to not complain if disk more recent!
		    (progn 
		      (switch-to-buffer (get-file-buffer file))
		      (let ((local-buff-point (point)))
			(kill-buffer (get-file-buffer file))
			(find-file file)
			(goto-char local-buff-point)))
		  (find-file file)
		  ))
	    (message "No output file written out by preprocessor.")
	    ;; If not output to visit, get cursor back to original buffer
	    (if (not (eq origbuffer (current-buffer)))
		(switch-to-buffer-other-window origbuffer))
	    ))))))

(defun ciao-ciaopp-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      ;; Was (incorrectly) save-excursion (EG fix)
      ;; We must allow Ciao to affect the point so that we
      ;; return to the end of output.
      (save-current-buffer
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	
	;; Used for ciaopp hooks
	(ciao-ciaopp-if-prompt-run-hook string)

	;; DTM: I need a new hook for the graphical menu :S
	(ciao-ciaopp-if-gmenu-run-hook string))))

;;------------------------------------------------------------
;; Compiler/Top-level, file based.
;;------------------------------------------------------------

(defun ciao-make-exec ()
  "Make an executable from the code in the current buffer. The buffer
must contain a @pred{main/0} or @pred{main/1} predicate. Note that
compiler options can be set to determine whether the libraries and
auxiliary files used by the executable will be statically linked,
dynamically linked, auto-loaded, etc."
  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_exec('" (buffer-file-name) "',_)." 
;; This was useful but now 'make_exec(FILE,_)' works (better!)
;; 	   (substring (buffer-name) 0 (string-match ".pl" (buffer-name))) 
;; 	   "')." 
	   )
   t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

(defun ciao-make-po ()
  "Make a Ciao object (.po) file from the code in the current
buffer.  This is useful for example while debugging during development
of a very large application which is compiled into an excutable, and
only one or a few files are modified. If the application executable is
dynamically linked, i.e., the component .po files are loaded
dynamically during startup of the application, then this command can
be used to recompile only the file or files which have changed, and
the correct version will be loaded dynamically the next time the
application is started. However, note that this must be done with care
since it only works if the inter-module interfaces have not changed.
The recommended, much safer way is to generate the executable again,
letting the Ciao compiler, which is inherently incremental, determine
what needs to be recompiled."
  (interactive) 
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command
   ciao-toplevel-buffer-name 
   (concat "make_po('" (buffer-file-name) "').") t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

(defun ciao-make-activemod ()
  "Make an active module executable from the code in the current
buffer. An active module is a remote procedure call server (see the
@lib{activemod} library documentation for details)."
  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_actmod('" (buffer-file-name) "','" 
    (read-string "Address publishing method: " 
	         "actmods/filebased_publish")
           "')." )
   t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

;;------------------------------------------------------------
;; Loading
;;------------------------------------------------------------

;; This is, as so many other things, an approximation...
(defun ciao-get-module-name ()
  (save-excursion 
    (goto-char (point-min))
    (let ((mbeg 0) (mend 0) (module-name nil))
      (setq module-name
	    (if (eq (search-forward-regexp 
		     "^[ \t]*:-[ \t\n]*\\(module\\|class\\)([ \t\n]*" 
		     20000 t) nil)
		"user"
	      (goto-char (match-end 0))
	      (setq mbeg (match-end 0))
	      (search-forward-regexp "[ \t\n]*\\(,\\|)\\)")
	      (setq mend (match-beginning 0))
	      (goto-char (match-beginning 0))
	      (buffer-substring-no-properties mbeg mend)))
      (if (eq (string-match "_" module-name) 0)
	  ;; if _ take the file name
	  (file-name-nondirectory 
	   (file-name-sans-extension
	    (buffer-file-name (current-buffer))))
	;; else, module-name, but eliminate quotes if they appear
	(ciao-replace-regexp-in-string 
	 "'[ \t\n]*$" "" 
	 (ciao-replace-regexp-in-string "^[ \t\n]*'" "" module-name)))
      )))

(defun ciao-load-command (filename delfile)
  (let (command)
    (save-excursion 
      (find-file filename)
      (goto-char (point-min))
      (setq command 
	    (concat
	     (if (string= (ciao-get-module-name) "user")
		 "ensure_loaded('"
	       (goto-char (point-min))
	       (if (eq (search-forward-regexp 
			"^[ \t]*:-[ \t\n]*class([ \t\n]*" 10000 t) nil)
		   "use_module('"
		 (if ciao-objects-lib-loaded
		     "use_class('"
		   (setq ciao-objects-lib-loaded 't)
		   "use_package(objects).\nuse_class('")))
;; 	     (if (boundp 'xemacs-logo)
;; 		 (replace-in-string filename "\\\\" "\\\\" t)
;; 	       (replace-regexp-in-string "\\\\" "\\\\" filename t t))
	     (ciao-replace-regexp-in-string "\\\\" "\\\\" filename t t)
	     "')."))
      (if (eq delfile nil)
	  command
	(kill-buffer (buffer-name))
	command)
      )))

(defun ciao-load-buffer ()
  "Load the current buffer (and any auxiliary files it may use) into the
top level. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether the
buffer has been marked for debugging or not -- see below. In case you try
to load a file while in the middle of the debugging process the debugger is
first aborted and then the buffer is loaded. Also, if there is a defined
query, the user is asked whether it should be called."
  (interactive)
  (ciao-unmark-last-run-errors)
  (ciao-load-buffer-current-or-main nil))

(defun ciao-run-test-related-buffer ()
  "Run the test over the current buffer and all related modules.

The test should be specified using a test assertion in the module."
  (interactive)
  (ciao-run-generic-command "run_test_related_modules")
  )
  
(defun ciao-run-test-buffer ()
  "Run the test over the current buffer.

The test should be specified using a test assertion in the module."
  (interactive)
  (ciao-run-generic-command "run_test_module")
  )

(defun ciao-show-untested-preds-buffer ()
  "Show the predicates that do not have any test assertion.

This show the predicates without test assertions to ensure that all
the predicates have tested."
  (interactive)
  (ciao-run-generic-command "show_untested_preds")
  )

(defun ciao-run-generic-command (generic_command)
  (ciao-run-generic-query
   (concat generic_command "('" (buffer-file-name) "')."))
  )

(defun ciao-run-generic-query (generic_query)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command
   ciao-toplevel-buffer-name generic_query t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )


(defun ciao-load-and-check-buffer ()
  "Load CiaoPP and then the current buffer (and any auxiliary files it
may use) into the top level. Use CiaoPP auto_check_assrt predicate to
check current buffer assertions and then load the buffer if there was
no error."
  (interactive)
  (ciao-unmark-last-run-errors)
  (ciao-check-assertions)
  (add-hook 'ciao-ciaopp-prompt-inferior-hook 
	    'ciao-find-errors-or-load-buffer t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-ciaopp-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t)))


(defun ciao-find-errors-or-load-buffer ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (if (and ciao-locate-errors-after-run (ciao-any-errors))
      (ciao-find-last-run-errors)
    (ciao-load-buffer)
    ;; In this case, probably best to stay in original buffer
    (switch-to-buffer-other-window ciao-last-source-buffer-used))
  )



(defun ciao-load-from-main-module ()
  "Load the module designated as @index{main module} (and all related files
that it uses) into the top level. If no main module is defined it will load
the current buffer. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether
the buffer has been marked for debugging or not -- see below. In case
you try to load a file while in the middle of the debugging process
the debugger is first aborted and then the buffer is loaded. Also, if
there is a defined query, the user is asked whether is should be 
called."
  (interactive)
  (ciao-unmark-last-run-errors)
  ;; Load current if main buffer undefined
  (if (string= ciao-main-filename "")
      (ciao-load-buffer-current-or-main nil)
    ;; Else, load main
    (ciao-load-buffer-current-or-main t)))

(defun ciao-load-buffer-current-or-main (main)
  (setq ciao-last-source-buffer-used (current-buffer))
  (if (eq (comint-check-proc (get-buffer 
			      (concat "*" ciao-toplevel-buffer-name "*"))) nil)
      ;; If Ciao buffer doesn't exist then go directly to load
      (ciao-real-load-buffer-current-or-main main)
    ;; Abort while debugging and then continue the normal process
    (let ((column
           (save-excursion
             (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
             (goto-char (point-max))
             (current-column))))
      (if (< column 10)
	  (ciao-real-load-buffer-current-or-main main)
	(add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
	(if main
	    (add-hook 'ciao-prompt-inferior-hook 
		      'ciao-real-load-from-main-module t)
	  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-load-buffer t))
	(ciao-send-command ciao-toplevel-buffer-name "a" t)))))

(defun ciao-real-load-buffer ()
  "This function really loads the buffer. And in case a default query has been
defined it asks the user if this query should be called."
  (interactive)
  (ciao-real-load-buffer-current-or-main nil))

(defun ciao-real-load-from-main-module ()
  (interactive)
  (ciao-real-load-buffer-current-or-main t))

(defun ciao-real-load-buffer-current-or-main (main)
;; SEE ABOVE
;;  ;; Uncolor previous error if there was any 
;;   (if ciao-previous-error
;;       (progn
;; 	(get-file-buffer (car (cdr (cdr ciao-previous-error))))
;; 	(if (> (car ciao-previous-error) 0)
;; 	    (ciao-uncolor (car ciao-previous-error)
;; 			  (car (cdr ciao-previous-error))
;; 			  'ciao-error))
;; 	(setq ciao-previous-error nil)))
;;   (if ciao-inferior-error
;;       (save-excursion
;; 	(switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
;; 	(ciao-uncolor ciao-inferior-error
;; 		      ciao-inferior-error
;; 		      'ciao-error)
;; 	(setq ciao-inferior-error nil)))
  (if main
      (ciao-send-command ciao-toplevel-buffer-name
		       (ciao-load-command ciao-main-filename nil) t)
    ;;                 (concat "use_module('" ciao-main-filename "').") t)
    (ciao-send-command ciao-toplevel-buffer-name 
		       (ciao-load-command (buffer-file-name) nil) t))
  (add-hook 'ciao-prompt-inferior-hook 'ciao-errors-or-load-query t))


(defun ciao-errors-or-load-query ()
  (if ciao-locate-errors-after-run
      (ciao-launch-find-last-run-errors-from-orig-buffer))
  (if (or (string= ciao-query "") (ciao-any-errors))
      t
    (ciao-load-query)))
  
(defun ciao-load-query ()
  "Issue predefined query."
  (interactive)
  (ciao-run-generic-query ciao-query))
;  (ciao-send-command ciao-toplevel-buffer-name ciao-query t))

(defun ciao-load-query-ask ()
  "Issue predefined query (asking the user first)."
  (interactive)
  (if (y-or-n-p (concat "Do you wish call the query '" ciao-query "'? "))
      (ciao-send-command ciao-toplevel-buffer-name ciao-query t)
    t))

(defun ciao-load-region (start end)
  "Load the current region (between the cursor and a previous mark)
into the top level. Since loading a region of a file is typically done
for debugging and/or testing purposes, this command always loads the
region in debugging mode (interpreted)." 
  (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "debug_module(user), ensure_loaded('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

(defun ciao-load-predicate ()
  "Load the predicate around the cursor into the top level. Since loading a 
single predicate is typically done for debugging and/or testing purposes,
this command always loads the predicate in debugging mode (interpreted)."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-load-region (car boundaries) (cdr boundaries))))

(defun ciao-select-debug-mode ()
  "Mark, or unmark, the current buffer for debugging (traditional
debugging or source debugging). Note that if the buffer has already been
loaded while it was unmarked for debugging (and has therefore been loaded
in ``compile'' mode) it has to be loaded again. The minibuffer shows how
the module is loaded now and allows selecting another mode for it. There
are three posibilities: N for no debug, S for source debug and D for
traditional debug."
  (interactive)
  (add-hook 'ciao-prompt-emacs-hook 'ciao-real-select-debug-mode t)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t))

(defun ciao-real-select-debug-mode (&optional list)
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N")
	(string)
	(default)
	(option))
    (if list
	(setq buffers-debug list)
      (setq buffers-debug (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") (car buffers-debug))
	(setq actually "D"))
    (if (string-match (concat "\\<" module "\\>") (cdr buffers-debug))
	(setq actually "S"))
    (cond ((string= actually "N")
	   (setq string "Module not selected for debug. ")
	   (setq default "S"))
	  ((string= actually "D")
	   (setq string "Module selected for trad debug. ")
	   (setq default "N"))
	  ((string= actually "S")
	   (setq string "Module selected for source debug. ")
	   (setq default "N")))
    (setq string (concat string "Select debug mode (N/S/D)? "))
    (setq option
 	  (read-string string default nil))
    (if (string= option "") (setq option default))
    ;; Was simply:  (but xemacs does not support the last argument)
    ;;	  (read-string string default nil default))
    ;; Send the apropiate command to Ciao
    (cond ((and (or (string= actually "N")
		    (string= actually "S"))
		(string= option "D"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "debug_module('" module "').") t))
	  ((and (or (string= actually "N")
		    (string= actually "D"))
		(string= option "S"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "debug_module_source('" module "').") t))
	  ((and (or (string= actually "S")
		    (string= actually "D"))
		(string= option "N"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "nodebug_module('" module "').") t)))))

(defun ciao-mark-buffer-source-debug ()
  "Mark a module for source debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "debug_module_source('" (ciao-module-name)"').")
   t))

(defun ciao-un-mark-buffer-debug ()
  "Unmark a module for debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name
   (concat "nodebug_module('" (substring (buffer-name) 0 -3) "').") t))

(defun ciao-enable-trace ()
  "Set the debugger to the trace state. In this state, the program is
executed step by step."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "trace." t))

(defun ciao-enable-debug ()
  "Set the debugger to the debug state. In this state, the program will
only stop in breakpoints and spypoints. Breakpoints are specially supported
in @apl{emacs} and using source debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "debug." t))

(defun ciao-no-debug ()
  "Set the debugger to the no debug state. In this state, the program will
execute until the end, without stopping in any step of the program."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "nodebug." t))

(defun ciao-debug-buffer ()
  "Debug (or stop debugging) buffer source. This is a shortcut which
is particularly useful when using the source debugger on a single
module. It corresponds to several lower-level actions.  Those
lower-level actions depend on how the module was selected for
debugging. In case the module was not marked for source-level
debugging, it marks the module corresponding to the current buffer for
source-level debugging, reloads it to make sure that it is loaded in
the correct way for debugging (same as \\<ciao-mode-map>
\\[ciao-load-buffer]), and sets the debugger in trace mode (i.e.,
issues the @tt{trace.} command to the top-level shell). Conversely, if
the module was already marked for source-level debugging then it will
take the opposite actions, i.e., it unmarks the module for
source-level debugging, reloads it, and sets the debugger to non-debug
mode."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-debug-buffer t))

(defun ciao-real-debug-buffer ()
  (interactive)
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N"))
    (setq buffers-debug (cdr (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") buffers-debug)
	(setq actually "S"))
    (cond ((string= actually "S")
	   ;; Buffer is marked for source debug
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-no-debug t)
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-load-buffer t)
	   (ciao-un-mark-buffer-debug))
	  ((string= actually "N")
	   ;; Buffer is marked for traditional debug or not marked for
	   ;; debug.
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-load-buffer t)
	   (ciao-mark-buffer-source-debug)))))

(defun ciao-module-name ()
  (let ((module-name (ciao-get-module-name)))
    (if (and (> (length module-name) 3)
             (or (string= (substring module-name -3) ".pl")
	         (string= (substring module-name -4) ".pls")
	         (string= (substring module-name -4) ".cgi")))
	(file-name-sans-extension module-name)
      module-name)))

(defun ciao-select-buffers-for-debug ()
  "Visits all Ciao files which are currently open in a buffer
allowing selecting for each of them whether to debug them or not and
the type of debugging performed. When working on a multiple module
program, it is possible to have many modules open at a time. In this
case, you will navigate through all open Ciao files and select
the debug mode for each of them (same as doing \\<ciao-mode-map>
\\[ciao-select-debug-mode] for each)."

  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-select-buffers-for-debug
	    t))

(defvar ciao-buffers nil)

(defun ciao-real-select-buffers-for-debug ()
  (interactive)
  (let* ((buffers (ciao-how-debugged))
	 (ciao-select-ciao-buffers
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (if (eq major-mode 'ciao-mode)
			  (setq ciao-buffers (cons buffer ciao-buffers))))))
	 (select-debug-module 
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (switch-to-buffer buffer t)
		      (ciao-real-select-debug-mode buffers))))
	 module)

    (if (not ciao-buffers)
	(mapcar ciao-select-ciao-buffers (buffer-list)))
    
    (setq module (car ciao-buffers))
    (setq ciao-buffers (cdr ciao-buffers))
    (funcall select-debug-module module)
    (if ciao-buffers
	(add-hook 'ciao-prompt-inferior-hook
		  'ciao-real-select-buffers-for-debug t))))

(defun ciao-how-debugged ()
  "Return a pair containning buffers selected for traditional debug and
buffers selected for source debug."
  (interactive)
  (let (buffers-debug end)
    (save-excursion
      (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
      (search-backward "display_debugged.")
      ;; Match all tradicional debugged modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (funcall ciao-match-string 1))
	(setq buffers-debug ""))
      ;; Match all source debug modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (cons buffers-debug 
				    (funcall ciao-match-string 1)))
	(setq buffers-debug (cons buffers-debug ""))))))
  
;;------------------------------------------------------------
;; Traditional commands: Consulting
;;------------------------------------------------------------
;; These and the following commands reuse the same temp file, which is
;; left at /tmp in the end. This eliminates the  need for
;; synchronization with the Ciao process, which is complicated by
;; the SICStus "The procedure xxx/yyy is being redefined" messages
;; (but unfortunately leaves  garbage behind, in the same way as the
;; ususal prolog.el mode).

(defun ciao-consult-buffer ()
  "Consult the entire buffer."
  (interactive)
  (ciao-send-command 
   ciao-toplevel-buffer-name (concat "consult('" (buffer-file-name) "')." )
   t))

(defun ciao-consult-region (start end)
  "Consult a given region."
   (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "consult('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

(defun ciao-consult-predicate ()
  "Consult the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-consult-region (car boundaries) (cdr boundaries))))

;;------------------------------------------------------------
;; Traditional commands: Compiling
;;------------------------------------------------------------

(defun ciao-compile-buffer ()
  "Compile the entire buffer."
  (interactive)
  (ciao-send-command 
   ciao-toplevel-buffer-name (concat "compile('" (buffer-file-name) "')." )
   t))

(defun ciao-compile-region (start end)
  "Compile a given region."
   (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "compile('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

;; PO 890606
(defun ciao-compile-predicate ()
  "Compile the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-compile-region (car boundaries) (cdr boundaries))))

;; Original version: JA 890531
;; (defun build-ciao-command (commstring)
;;   (concat "ciao:zap_file('"
;;   (concat "zap_file('"
;; 	  (ciao-temp-code-file) "', '"
;; 	  (or (buffer-file-name) "user") "', " commstring ")."))

;;------------------------------------------------------------
;; Region handling
;;------------------------------------------------------------

;; MH save-excursion
;; Must be improved. Cannot handle predicates with clauses
;; separated by newlines...
;; PO 890606
(defun predicate-boundaries ()
  ;; Find "beginning" of predicate
  (beginning-of-line)
  (save-excursion 
    (while (and (not (looking-at "\n")) (not (bobp)))
      (forward-line -1)
      (skip-chars-forward " \t"))
    (let ((start (point)))

	 ;; Find "end" of predicate
	 (forward-line 1)
	 (skip-chars-forward " \t")
	 (while (and (not (looking-at "\n")) (not (eobp)))
	   (forward-line 1)
	   (skip-chars-forward " \t"))
	 (cons start (point)))))

(defun ciao-write-region (minpoint maxpoint filename)
  (let (original-buffer buffercont temp-buffer)
    (setq original-buffer (current-buffer))
    (setq buffercont (buffer-substring-no-properties minpoint maxpoint))
    (setq temp-buffer (generate-new-buffer "temp-buffer"))
    (set-buffer temp-buffer)
    (insert buffercont "\n")
    (write-region (point-min) (point-max) filename nil nil)
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun ciao-del-temp-files () 
  (delete-file-if-possible ciao-last-temp-file)
  (delete-file-if-possible (concat ciao-last-temp-file ".dep"))
  (delete-file-if-possible (concat ciao-last-temp-file ".err"))
  (delete-file-if-possible (concat ciao-last-temp-file ".asr"))
  (delete-file-if-possible (concat ciao-last-temp-file ".itf"))
  (delete-file-if-possible (concat ciao-last-temp-file ".po")))

(defun delete-file-if-possible (file)
  (if (and (file-exists-p file) (file-writable-p file))
      (delete-file file)
    nil))

;; M.H. 
;; In distributed execution, need to halt siblings...
;; (setq kill-buffer-hook 'ciao-halt-process)
(defun ciao-halt-process ()
  (if (not (comint-check-proc 
	    (concat "*" ciao-toplevel-buffer-name "*"))) 
      ()
    (progn 
      (process-send-string ciao-toplevel-buffer-name "halt.")
      (sleep-for 2))
    ))

;;------------------------------------------------------------
;; Commands related to the source code debugger
;;------------------------------------------------------------

(defun ciao-debug-display-frame (buffname)
  (interactive)
  (if ciao-debug-last-frame
      (progn
	;; (ciao-debug-set-buffer)
	(let ((port    (car ciao-debug-last-frame))
	      (file    (car (cdr ciao-debug-last-frame)))
	      (l0      (car (cdr (cdr ciao-debug-last-frame))))
	      (l1      (car (cdr (cdr (cdr ciao-debug-last-frame)))))
	      (numpred (car (cdr (cdr (cdr (cdr ciao-debug-last-frame))))))
	      (pred    (cdr (cdr (cdr (cdr (cdr ciao-debug-last-frame)))))))

	  ;; (setq file (ciao-debug-transform-file-name file))
	  (ciao-debug-display-line file l0 l1 pred numpred port buffname) 
	  (setq ciao-debug-last-frame nil)))))

(defun ciao-debug-display-line (file start end pred numpred port buffname)
  (let* ((count 0) (init 0) (finish 0) (test t) (pos 0)
	 (last-nonmenu-event t)  ; Prevent use of dialog box for questions.
	 ;; Problem for embedded debugger
	 (buffer
	  (save-excursion
	    (or (string= (buffer-name) buffname) ; was (current-buffer) and eq!
		(set-buffer buffname))
	    (ciao-debug-find-file file)))
	 (window (and buffer (or (get-buffer-window buffer)
				 (display-buffer buffer)))))

    ; Unmark the last region marked
    (ciao-debug-uncolor-line)

    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      ;; (goto-line start)
	      ;; Due to impression in detecting the start line of a clause
	      ;; we move to the end and clause and then search backward
	      ;; until find the beginning of the clause.
	      (goto-line end)
	      (end-of-line)
	      (re-search-backward "^[a-z']" (point-min) t)

	      ;; Search the numpred-th pred and put the marker at the
	      ;; beginning of the line. Doesn't consider PRED in
	      ;; comment
 	      (end-of-line)
 	      (setq finish (point))
 	      (beginning-of-line)
 	      (setq init (point))
 	      (while (and test (not (eq count numpred)))
 		(while (and test (not (search-forward pred finish t)))
 		  (forward-line)
		  (if (or (< end (ciao-what-line))
			  (and (eq init (point)) (eq (point) finish)))
		      (setq test nil))
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point)))
 		;; Found a PRED, search if it is in a comment
 		(if (and test (not (search-backward "%" init t)))
 		    (setq count (+ count 1))
 		  (forward-line)
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point))))
	      
	      (if (< count numpred) 
		  ;; Not found pred, overlay the whole region
		  (progn
		    (setq overlay-arrow-string "")
		    (goto-line end)
		    (end-of-line)
		    (re-search-backward "^[a-z']" (point-min) t)
		    (ciao-color (ciao-what-line)
				end
				ciao-face-debug-expansion
				'ciao-debug)
		    ;; Save information for uncoloring the last line
		    (setq ciao-debug-last-line
			  (cons (current-buffer)
				(ciao-what-line)))

		    )
		;; Save information for uncoloring the last line
		(setq ciao-debug-last-line
		      (cons (current-buffer)
			    (ciao-what-line)))
		
		;; Color line
		(ciao-color (ciao-what-line)
			    (ciao-what-line)
			    (ciao-debug-obtain-color port)
			    'ciao-debug)
		(setq overlay-arrow-string (ciao-debug-transform-port port))

		)
	      ;; Arrow position
	      (beginning-of-line)
	      (setq pos (point))
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

(defun ciao-what-line ()
  "Return the line number. This function is a fix for the fact that in
xemacs the function what-line does not behave as in emacs."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))
	 
(defun ciao-debug-transform-port (port)
  "Arrow to show in source file. It's determines from PORT."
  (cond ((string= "Call" port) "C=>")
	((string= "Exit" port) "E=>")
	((string= "Fail" port) "F=>")
	((string= "Redo" port) "R=>")))

(defun ciao-debug-obtain-color (port)
  (cond ((string= "Call" port) ciao-face-debug-call)
	((string= "Exit" port) ciao-face-debug-exit)
	((string= "Fail" port) ciao-face-debug-fail)
	((string= "Redo" port) ciao-face-debug-redo)))

(defun ciao-debug-uncolor-line ()
  (if (and ciao-debug-last-line
	   (buffer-name (car ciao-debug-last-line))) ;; And buffer not killed
      (save-excursion
	(set-buffer (car ciao-debug-last-line))
	(ciao-uncolor (cdr ciao-debug-last-line)
		      (cdr ciao-debug-last-line)
		      'ciao-debug))))
  
(defun ciao-debug-remove-marks ()
  (ciao-debug-uncolor-line)
  (setq overlay-arrow-position nil))

(defun ciao-debug-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if ciao-debug-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later
	    (setq ciao-debug-filter-pending-text 
		  (concat (or ciao-debug-filter-pending-text "") string))

	  (let ((ciao-debug-filter-defer-flag t))
	    ;; Process now any text we previously saved up
	    (if ciao-debug-filter-pending-text
		(setq string (concat ciao-debug-filter-pending-text string)
		      ciao-debug-filter-pending-text nil))
            ;; Was (incorrectly) save-excursion (EG fix)
            ;; We must allow Ciao to affect the point so that we
	    ;; return to the end of output.
	    (save-current-buffer
	      (set-buffer (process-buffer proc))
	      ;; If we have been so requested, delete the debugger prompt.
	      (if (marker-buffer ciao-debug-delete-prompt-marker)
		  (progn
		    (delete-region (process-mark proc)
				   ciao-debug-delete-prompt-marker)
		    (set-marker ciao-debug-delete-prompt-marker nil)))
	      
	      ; Here we obtain the output to show in the buffer
	      (setq output (ciao-debug-marker-filter string))
	      
	      (setq process-window
		    (and ciao-debug-last-frame
			 (>= (point) (process-mark proc))
			 (get-buffer-window (current-buffer))))

	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	      (add-hook 'ciao-prompt-emacs-hook 'ciao-debug-remove-marks t)

	      (ciao-if-prompt-run-hook output)

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion 
	    ;; in case the source file is our current buffer.
	    (if process-window
		(save-selected-window
		 (select-window process-window)
		 (ciao-debug-display-frame (buffer-name)))   
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore
	      ;; point.
	      (let ((old-buf (current-buffer)))
		(set-buffer (process-buffer proc))
		(unwind-protect
		    (ciao-debug-display-frame (buffer-name))
		  (set-buffer old-buf)))))
	  ;; If we deferred text that arrived during this processing
	  ;; handle it now.
	  (if ciao-debug-filter-pending-text
	      (ciao-debug-filter proc "")))))) 
	  
(defun ciao-debug-find-file (file)
  (save-excursion
    (let ((buf (find-file-noselect (fix-cygwin-drive-letter file))))
      (set-buffer buf)
      buf)))

(defun ciao-debug-marker-filter (string)
  "Search the string for the debugging information"
  (setq ciao-debug-marker-acc (concat ciao-debug-marker-acc string))
  (let ((output ""))
    ; Process all the complete markers in this chunk
    (while (string-match ciao-debug-marker-regexp ciao-debug-marker-acc)
      (setq
       ;; Extract the frame position from the marker
       ciao-debug-last-frame
       (cons (substring ciao-debug-marker-acc (match-beginning 6)
			(match-end 6))
	     (cons (substring ciao-debug-marker-acc 
			      (match-beginning 1) (match-end 1))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 2) (match-end 2)))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 3) (match-end 3)))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 5) (match-end 5)))
		   (substring ciao-debug-marker-acc 
			      (match-beginning 4) (match-end 4)))))))
	            
       ;; Append Any Text Before the marker to the output we're going to
       ;; return - we don't include the marker in this text
       output (concat output 
		      (substring ciao-debug-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text
       ciao-debug-marker-acc (substring ciao-debug-marker-acc (+ (match-end
							       5) 1))))

    ;; Does the remaining text look like it might end with the beginning of
    ;; another marker? If it does, the keep it in ciao-debug-marker until
    ;; we receive the rest of it. Since we know the full marker regexp
    ;; above failed, it's pretty simple to test for marker starts.
    (if (string-match "         In " ciao-debug-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output
	  (setq output (concat output (substring ciao-debug-marker-acc 0
						 (match-beginning 0))))
	  (setq ciao-debug-marker-acc (substring ciao-debug-marker-acc
						 (match-beginning 0))))
      (setq output (concat output ciao-debug-marker-acc)
	    ciao-debug-marker-acc ""))
    output))

;; Remember to check ciao-ciaopp-if-prompt-run-hook and
;; ciao-lpdoc-if-prompt-run-hook in case of any modification
(defun ciao-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-prompt-marker-acc (concat ciao-prompt-marker-acc string))
    (if (string-match ciao-prompt-pattern ciao-prompt-marker-acc)
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-prompt-marker-acc 
		(substring ciao-prompt-marker-acc (match-end 0)))
	  (if ciao-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-prompt-inferior-hook))
		(setq ciao-prompt-inferior-hook 
		      (cdr ciao-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-prompt-emacs-hook)
	    (setq ciao-prompt-emacs-hook nil))))))

;; Remember to check ciao-if-prompt-run-hook and
;; ciao-lpdoc-if-prompt-run-hook in case of any modification
(defun ciao-ciaopp-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-ciaopp-prompt-marker-acc 
	  (concat ciao-ciaopp-prompt-marker-acc string))
;; Added one case:
    (if (or 
	 (string-match ciao-ciaopp-prompt-pattern
		       ciao-ciaopp-prompt-marker-acc)
	 (string-match "\nCiao/CiaoPP/LPdoc Listener finished"
		       ciao-ciaopp-prompt-marker-acc))
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-ciaopp-prompt-marker-acc 
		(substring ciao-ciaopp-prompt-marker-acc (match-end 0)))
	  (if ciao-ciaopp-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-ciaopp-prompt-inferior-hook))
		(setq ciao-ciaopp-prompt-inferior-hook 
		      (cdr ciao-ciaopp-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-ciaopp-prompt-emacs-hook)
	    (setq ciao-ciaopp-prompt-emacs-hook nil))))))

;; Remember to check ciao-if-prompt-run-hook and
;; ciao-ciaopp-if-prompt-run-hook in case of any modification
(defun ciao-lpdoc-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-lpdoc-prompt-marker-acc 
	  (concat ciao-lpdoc-prompt-marker-acc string))
    (if (string-match ciao-lpdoc-prompt-pattern ciao-lpdoc-prompt-marker-acc)
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-lpdoc-prompt-marker-acc 
		(substring ciao-lpdoc-prompt-marker-acc (match-end 0)))
	  (if ciao-lpdoc-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-lpdoc-prompt-inferior-hook))
		(setq ciao-lpdoc-prompt-inferior-hook 
		      (cdr ciao-lpdoc-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-lpdoc-prompt-emacs-hook)
	    (setq ciao-lpdoc-prompt-emacs-hook nil))))))

(defun ciao-ciaopp-if-gmenu-run-hook (string)
    (if (not (equal ciao-ciaopp-gmenu-hook 'nil))
	  (funcall ciao-ciaopp-gmenu-hook string)))

(defun ciao-inferior-process-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Need to reload certain things if needed.
	 (setq ciao-objects-lib-loaded nil)
	 (setq ciao-assrt-lib-loaded nil)
	 (setq ciao-inferior-error nil)
	 ;; (setq ciao-error nil)
	 (setq ciao-debug-filter-pending-text "")

	 ;; Stop displaying an arrow in a source file.
	 (ciao-debug-remove-marks)

	 ;; Reset stuff needed for prompt hook in ciao, ciaopp and lpdoc
	 (setq ciao-prompt-emacs-hook nil)
	 (setq ciao-prompt-inferior-hook nil)
	 (setq ciao-prompt-marker-acc "")
	 (setq ciao-ciaopp-prompt-emacs-hook nil)
	 (setq ciao-ciaopp-prompt-inferior-hook nil)
	 (setq ciao-ciaopp-prompt-marker-acc "")
	 (setq ciao-lpdoc-prompt-emacs-hook nil)
	 (setq ciao-lpdoc-prompt-inferior-hook nil)
	 (setq ciao-lpdoc-prompt-marker-acc "")

	 (set-process-buffer proc nil))

	((memq (process-status proc) '(signal exit))
	 ;; Need to reload certain things if needed.
	 (setq ciao-objects-lib-loaded nil)
	 (setq ciao-assrt-lib-loaded nil)
	 (setq ciao-inferior-error nil)
	 ;; (setq ciao-error nil)
	 (setq ciao-debug-filter-pending-text "")

	 ;; Stop displaying an arrow in a source file.
	 (ciao-debug-remove-marks)

	 ;; Reset stuff needed for prompt hook in ciao, ciaopp and lpdoc
	 (setq ciao-prompt-emacs-hook nil)
	 (setq ciao-prompt-inferior-hook nil)
	 (setq ciao-prompt-marker-acc "")
	 (setq ciao-ciaopp-prompt-emacs-hook nil)
	 (setq ciao-ciaopp-prompt-inferior-hook nil)
	 (setq ciao-ciaopp-prompt-marker-acc "")
	 (setq ciao-lpdoc-prompt-emacs-hook nil)
	 (setq ciao-lpdoc-prompt-inferior-hook nil)
	 (setq ciao-lpdoc-prompt-marker-acc "")

	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ":"
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (force-mode-line-update)
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gud buffer.
	     (set-buffer obuf))))))

(defun ciao-debug-breakon ()
  "Set a breakpoint on the current literal (goal). This can be done at any
time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}. Breakpoints are only useful when using source-level
debugging."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (< column 6)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))

  (ciao-color (ciao-what-line)
	      (ciao-what-line)
	      ciao-face-debug-breakpoint
	      'ciao-break)
  (ciao-send-command ciao-toplevel-buffer-name
		     (concat "breakpt(" (ciao-debug-breakparams (point))
			     ").") t))
  
(defun ciao-debug-breakoff ()
  "Remove a breakpoint from the current literal (goal). This can be done
at any time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (< column 6)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))
  
  (ciao-uncolor (ciao-what-line)
		(ciao-what-line)
		'ciao-break)
  (ciao-send-command ciao-toplevel-buffer-name
		     (concat "nobreakpt(" (ciao-debug-breakparams (point))
			     ").") t))

(defun ciao-debug-all-breakoff ()
  "Remove all breakpoints. This can be done at any time (while debugging
or not)."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (eq column 3)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))
  
  (ciao-send-command ciao-toplevel-buffer-name "nobreakall." t)
  (ciao-debug-uncolor-all-breakpt))
  
(defun ciao-debug-breakparams (point)
  (let* ((boundaries (ciao-debug-predicate-boundaries point))
	(pred-name (find-tag-default)) 
	(src-file (expand-file-name (buffer-name (current-buffer))))
	(begin-line (car boundaries))
	(end-line (cdr boundaries)) 
	(number 0)
	string)
    (save-excursion
      (goto-line begin-line)
      (while (< (point) point)
	(if (re-search-forward
	     (concat "\\<" (regexp-quote pred-name) "\\>") nil nil)
	    (setq number (+ number 1)))))
    (concat  "'" pred-name "','" src-file "',"
			 (int-to-string begin-line) "," 
			 (int-to-string end-line) "," 
			 (int-to-string number) "," 
			 (int-to-string (ciao-what-line)))))

(defun ciao-debug-predicate-boundaries (point)
  (let ((start) 
	(bound)
	(begin)
	(test t))
    ;; Find the beginning of the predicate boundary
    (save-excursion
      (search-backward-regexp "^[^ \t]" 1 t)
      (setq start (ciao-what-line)))
    ;; Find the end of the predicate boundary
    (save-excursion 
      ;; Search line to line to establish limits
      (setq test t)
      (setq begin (point))
      (end-of-line)
      (setq bound (point))
      (goto-char begin)
      (while test
	(while (not (search-forward-regexp "\\.[ \t]*\\(%\\|$\\)" bound t))
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin))
	;; We reach this point just when find the regexp. Are we in a
	;; comment?
	(if (not (search-backward "%" begin t))
	    (setq test nil)
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin)))	  
      (cons start (ciao-what-line)))))

(defsubst ciao-color (startline endline color over)
  "Highlight region from STARTLINE to ENDLINE using COLOR with overlay name
OVER."
  (let (start end overlay)
    (save-excursion
      (goto-line startline)
      (setq start (point))
      (goto-line endline)
      (end-of-line)
      (if (or (eq over 'ciao-error) (eq over 'ciao-debug))
	  (setq end (+ (point) 1))
	(setq end (point))))
    (setq overlay (make-overlay start end))
    (overlay-put overlay 'face color)
    (overlay-put overlay over t)))

(defun ciao-uncolor (startline endline over)
  "Unhighlights the region from STARTLINE to ENDLINE with the overlay name
OVER."
  (let (start)
    (save-excursion
      (goto-line startline)
      (setq start (point)))
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr over) 
			     (delete-overlay ovr))))
	    (overlays-at start))))

(defun ciao-debug-uncolor-all-breakpt ()
  "Remove breakpoint coloring in all Ciao files."
  (interactive)
  (save-excursion
    (mapcar (function (lambda (buffer)
			(set-buffer buffer)
			(if (eq major-mode 'ciao-mode)
			    (ciao-debug-uncolor-buffer))))
	    (buffer-list))))

(defun ciao-debug-uncolor-buffer ()
  "Remove breakpoint faces color in a Ciao buffer"
  (let (beg end)
    (setq beg (point-min))
    (setq end (point-max))
    (mapcar (function (lambda (over)
			(and (overlay-get over 'ciao-break)
			     (delete-overlay over))))
	    (overlays-in beg end))))
  
(defun ciao-debug-display-breakpt ()
  "Redisplay breakpoints in all Ciao buffers. This ensures that the marks
in the source files and the Ciao toplevel are synchronized."

  (interactive)
  (ciao-debug-uncolor-all-breakpt)
  (if (comint-check-proc 
       (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (progn
	(add-hook 'ciao-prompt-emacs-hook 
		  'ciao-debug-redisplay-breakpt t)
	(ciao-send-command ciao-toplevel-buffer-name "list_breakpt." t))))

(defun ciao-debug-redisplay-breakpt ()
    (let ((buffer (current-buffer)))
      (save-excursion
	(let ((file 0) (l0 0) (l1 0) (pred 0) (numpred 0) (bound 0))
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq bound (point))
	  (search-backward "list_breakpt.")
	  (while (search-forward-regexp 
		  (concat "Breakpoint in file \\(.*\\)" 
			  " \\([0-9]+\\)-\\([0-9]+\\) "
			  "on literal \\(.*\\)-\\([0-9]+\\)")
		  bound t)
	    (setq file (buffer-substring-no-properties (match-beginning 1)
						       (match-end 1))
		  l0 (string-to-number (buffer-substring-no-properties 
				     (match-beginning 2) (match-end 2)))
		  l1 (string-to-number (buffer-substring-no-properties 
				     (match-beginning 3) (match-end 3)))
		  pred (buffer-substring-no-properties (match-beginning 4)
						       (match-end 4))
		  numpred (string-to-number (buffer-substring-no-properties 
					  (match-beginning 5) (match-end 5))))
	    (save-excursion
	      (set-buffer (get-file-buffer file))
	      (goto-line l0)
	      ;; To change when considering comments in clause
	      (search-forward pred nil t numpred)
	      (ciao-color (ciao-what-line)
			  (ciao-what-line)
			  ciao-face-debug-breakpoint
			  'ciao-break)))))
    (switch-to-buffer buffer)))

;;------------------------------------------------------------
;; Generating documentation using LPdoc
;;------------------------------------------------------------

(defun ciao-visit-lpdoc-settings ()
  "Visit, or create, the default @tt{SETTINGS.pl} file (which
controls all auto-documenter options)."
  (interactive)
  (let ((libsettings (concat ciao-lpdoc-libpath "/SETTINGS_DEFAULT.pl"))
	(thisfile (buffer-name (current-buffer)))
	(docsettings (concat (ciao-lpdoc-buffer-tmpdir 
			      (buffer-name (current-buffer))) 
			     "/SETTINGS.pl"))
	(sourcedir   (directory-file-name
		      (file-name-directory 
		       (buffer-file-name (current-buffer))))))
	(make-directory (ciao-lpdoc-buffer-tmpdir thisfile) t)
	(if (file-exists-p docsettings)
	    (find-file-other-window docsettings)
	  (copy-file libsettings docsettings t) 
	  (find-file-other-window docsettings)
	  (goto-char (point-min))
	  (search-forward "filepath := ")
	  (insert "\'")
	  (insert sourcedir)
	  (insert "\'.")
	  (kill-line)
	  (goto-char (point-min))
	  (search-forward "systempath := ")
	  (insert "\'")
	  (insert (concat ciao-real-lib-dir "/lib\'|\'"))
	  (insert (concat ciao-real-lib-dir "/library\'|\'"))
	  (insert (concat ciao-real-lib-dir "/contrib\'."))
	  (kill-line)
	  (goto-char (point-min))
	  (search-forward "doc_structure := ")
	  (kill-line)
	  (insert "\'")
	  (insert thisfile)
	  (backward-char 3)
	  (insert "\'.")
	  (kill-line)
	  (goto-char (point-min))
	  (search-forward "lpdoclib := ")
	  (insert "\'")
	  (insert ciao-lpdoc-libpath)
	  (insert "\'.")
	  (kill-line)
	  (goto-char (point-min))
	  (search-forward "htmldir := ")
	  (kill-line)
	  (insert "\'\'.")
	  (goto-char (point-min))
	  (search-forward "output_name := ")
	  (kill-line)
	  (insert "_ :- fail.")
	  (goto-char (point-min))
	  (save-buffer)
	  )
	)
  )

(defun ciao-lpdoc-buffer-tmpdir (filename)
  (let ((tmpdir (cdr (assoc filename ciao-lpdoc-buffer-tmpdir-list))))
    (if tmpdir
	tmpdir
      (setq tmpdir (ciao-new-temp-code-dir filename))
      (setq ciao-lpdoc-buffer-tmpdir-list
	    (cons 
	     (cons filename tmpdir)
	     ciao-lpdoc-buffer-tmpdir-list)))
      tmpdir
    ))

(defun ciao-gen-doc ()
  "Generate the documentation according to @tt{SETTINGS.pl} in the
default format. This allows generating complex documents but it
assumes that @tt{SETTINGS.pl} exists and that the options that it
contains (main file, component files, paths, etc.) have been set
properly. Documentation is generated in a temporary directory. Note
however that for generating complex manuals the best approach is to
set up a permanent documentation directory with the appropriate
@tt{SETTINGS.pl} and @tt{Makefile} files (see the LPdoc manual)."
  (interactive)
  (message "Generating documentation... ")
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (let ((thisfile (buffer-name (current-buffer))))
    (if (not (file-exists-p 
	      (concat (ciao-lpdoc-buffer-tmpdir thisfile) "/SETTINGS.pl")))
	(message 
	 "You need to first visit SETTINGS.pl and perhaps choose options")
      ;; Not necessary and creates a problem: first time errors are
      ;; not found because sending command gets ahead of starting process
      ;; (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
      (ciao-send-command 
       ciao-lpdoc-buffer-name 
       (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " 
	       ciao-lpdoc-system " " ciao-lpdoc-docformat)
       t)
      (if ciao-locate-errors-after-run
	  (add-hook 'ciao-lpdoc-prompt-inferior-hook 
		    'ciao-launch-find-last-run-errors-from-orig-buffer t))
      ))
  (message "Generating documentation... done.")
  )

(defun ciao-gen-buffer-doc ()
  "Generate the documentation for the current buffer in the default
format. This allows generating a simple document for the current
buffer. Basically, it creates a simple, default @tt{SETTINGS.pl}
file, sets @tt{mainfile} in @tt{SETTINGS.pl} to the current buffer
and then generates the documentation in a temporary directory. This is
useful for seeing how the documentation of a file will format. Note
that for generating manuals the best approach is to set up a permanent
documentation directory with the appropriate @tt{SETTINGS.pl} file
(see the LPdoc manual)."
  (interactive)
  (message "Generating documentation for buffer... ")
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (let ((thisfile (buffer-name (current-buffer)))
	(original-buffer (current-buffer))
	(settings (concat (ciao-lpdoc-buffer-tmpdir 
			   (buffer-name (current-buffer))) "/SETTINGS.pl")))
    (message (concat "Settings is: " settings))
    (if (file-exists-p settings)
	t
      (ciao-visit-lpdoc-settings)
      (switch-to-buffer-other-window original-buffer)
      )
    (find-file settings)
    (goto-char (point-min))
    (search-forward "doc_structure := ")
    (insert "\'")
    (insert thisfile)
    (backward-char 3)
    (insert "\'.")
    (kill-line)
    (goto-char (point-min))
    (search-forward "lpdoclib := ")
    (insert "\'")
    (insert ciao-lpdoc-libpath)
    (insert "\'.")
    (kill-line)
    (goto-char (point-min))
    (search-forward "htmldir := ")
    (kill-line)
    (insert "\'\'.")
    (goto-char (point-min))
    (search-forward "output_name := ")
    (kill-line)
    (insert "_ :- fail.")
    (save-buffer)
    (bury-buffer)
    ;; Not necessary and creates a problem: first time errors are
    ;; not found because sending command gets ahead of starting process
    ;;    (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
    (ciao-send-command 
     ciao-lpdoc-buffer-name 
     (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " ciao-lpdoc-system 
	     " " ciao-lpdoc-docformat)
     t)
    (if ciao-locate-errors-after-run
	(add-hook 'ciao-lpdoc-prompt-inferior-hook 
		  'ciao-launch-find-last-run-errors-from-orig-buffer t))
    )
  (message "Generating documentation for buffer... done.")
  )

(defun ciao-start-viewer ()
  "Start a viewer on the documentation for the current buffer in the
   default format." 
  (interactive)
  (let ((thisfile (buffer-name (current-buffer)))
	(thisfileroot 
         (file-name-sans-extension (buffer-name (current-buffer)))))
    (if (not (file-exists-p (concat (ciao-lpdoc-buffer-tmpdir thisfile) 
				    "/SETTINGS.pl")))
	(message "You need to first choose options in SETTINGS.pl")
      (cond
       ((string= ciao-lpdoc-docformat "ascii") 
	(find-file-other-window 
	 (concat 
	  (ciao-lpdoc-buffer-tmpdir thisfile) "/" thisfileroot ".ascii")))
       ((string= ciao-lpdoc-docformat "info") 
	(info-other-window
	 (concat 
	  (ciao-lpdoc-buffer-tmpdir thisfile) "/" thisfileroot ".info")))
       (t
	(ciao-send-command 
	 ciao-lpdoc-buffer-name 
	 (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " 
		 ciao-lpdoc-system " " 
		 (if (string= ciao-lpdoc-docformat "dvi")
		     ;; "large" Optional, for demos
		     "")
		 ciao-lpdoc-docformat "view")
	 t))))))

(defun ciao-lpdoc-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      ;; Was (incorrectly) save-excursion (EG fix)
      ;; We must allow Ciao to affect the point so that we
      ;; return to the end of output.
      (save-current-buffer
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	
	;; Used for lpdoc hooks
	(ciao-lpdoc-if-prompt-run-hook string))))

;;------------------------------------------------------------
;; Auxiliary
;;------------------------------------------------------------

;; Functions for generating documentation for the ciao.el mode functions
;; in lpdoc format (!) M. Hermenegildo

(defun ciao-do-document-bindings (sec-commands)
  "Generate documentation for all the bindings in lpdoc format."
   (cond
    ((eq sec-commands nil) nil)
    ((equal (car (car sec-commands)) 'section)
     (insert "@section{")
     (insert (car (cdr (car sec-commands))))
     (insert "}\n\n")
     (insert (car (cdr (cdr (car sec-commands)))))
     (insert "\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ((equal (car (car sec-commands)) 'paragraph)
     (insert "\n\n")
     (insert (car (cdr (car sec-commands))))
     (insert "\n\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    (t ;; else, list of bindings
     (insert "@begin{description}\n")
     (ciao-print-function-info (car sec-commands))
     (insert "@end{description} @p \n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ))

(defun ciao-print-function-info (info)
  "Print the information on a function as an item in lpdoc format. If
function is a string it is taken to be the comment."
  (insert
   (concat 
    "\n@item{"
    (ciao-print-keys (car info))
    "} "
    (let ((function (car (cdr info))))
      (if (stringp function)
	  function
	(documentation function)))
    "\n"
    ))
  )

(defun ciao-print-keys (str) 
  "Format key binding sequences in lpdoc format."
  (cond 
   ((string= str "") "")
   ((eq (string-match "M-x" str 0) 0)
    (concat "@key{M-x} @tt{" (substring str 3) "}"))

   ((eq (string-match "M-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "A-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "C-" str 0) 0)
    (concat "@key{^" (upcase (substring str 2 3)) "} "
	    (ciao-print-keys (substring str 3))))

;;    ((eq (string-match " " str 0) 0)
;;     (concat "@key{SPC} " 
;; 	    (ciao-print-keys (substring str 1))))

;; Not correct, but tries to fix spurious spaces which are passed
   ((eq (string-match " " str 0) 0)
    (concat "" 
 	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "SPC" str 0) 0)
    (concat "@key{SPC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\t" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "TAB" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\e" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "ESC" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "RET" str 0) 0)
    (concat "@key{RET} " 
	    (ciao-print-keys (substring str 3))))
   (t 
    (concat "@key{" 
;;	    (text-char-description (string-to-char (substring str 0 1) ))
	    (key-description (substring str 0 1) )
	    "} "
	    (ciao-print-keys 
	     (substring str 1))))))

(defun ciao-document-variables ()
  "Generate documentation for all user-defined variables in lpdoc format."
  (let ((sym-list)
	(ciao-vars nil)
	(ciaopp-vars nil)
	(lpdoc-vars nil)
	(ciao-faces nil))
    
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
	       (lambda (sym)
		 (if (string-match "ciao" (symbol-name sym))
		     (setq sym-list (cons sym sym-list))))))
    
    ;; Classify variables
    (mapcar (function (lambda (sym)
			(cond ;; Must be before others
			      ((string-match "face" (symbol-name sym))
			       (setq ciao-faces (cons sym ciao-faces)))
			      ((string-match "ciaopp" (symbol-name sym))
			       (setq ciaopp-vars (cons sym ciaopp-vars)))
			      ((string-match "lpdoc" (symbol-name sym))
			       (setq lpdoc-vars (cons sym lpdoc-vars)))
			      (t 
			       (setq ciao-vars (cons sym ciao-vars))))))
	    sym-list)

    ;; Generate the documentation
    (insert "\n@subsection{Ciao general variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-vars 'string<))
    (insert "@end{description}\n")
    (insert "\n@subsection{CiaoPP variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciaopp-vars 'string<))
    (insert "@end{description}\n")
    (insert "\n@subsection{LPdoc variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort lpdoc-vars 'string<))
    (insert "@end{description}\n")
    (insert 
     "\n@subsection{Faces used in syntax-based highlighting (coloring)}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-faces 'string<))
    (insert "@end{description}\n")))

(defun ciao-describe-func (s)
  "Format the description of a symbol."
  (cond
   ;; It is a customizable variable 
   ((and (boundp s) (get s 'custom-type)) 
    (insert 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{"))
    (if (listp (get s 'custom-type))
	(insert
	 (symbol-name 
	  (type-of
	   (car (cdr (car (cdr 
			   (get 's 'custom-type))))))))
      (insert (symbol-name (get s
				       'custom-type))))
    (insert "})}\n")
    (insert 
     (concat 
      (documentation-property s 'variable-documentation)
      "\n")))
   ;; It is a face
   ((documentation-property s 'face-documentation)
    (insert 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{face})}\n"
	     (documentation-property s 'face-documentation)
	     "\n")))
   ))

(defun compile-ciao-mode ()
  "With this handy function this file can be compiled as
   emacs -batch -l ciao.el -f compile-ciao-mode"
   (message "Compiling ciao.el")
   (byte-compile-file "ciao.el")
   (message "Compiling word-help.el")
   (byte-compile-file "word-help.el"))
;; This are really no use...
;;  (byte-force-recompile "."))
;;  (byte-recompile-directory "." t))

(defun ciao-report-mode-version ()
  "Report the version of the emacs Ciao mode."
  (interactive)
  (message (concat "Ciao, CiaoPP, LPdoc mode version: " 
		   ciao-mode-version )))

;; Local version of replace-regexp-in-string, since it is not 
;; present in older versions of emacsen
(defun ciao-replace-regexp-in-string (regexp rep string &optional
					     fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (ciao-replace-regexp-in-string 
   \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1) 
  => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

;;------------------------------------------------------------
;; Kludge to fix old version maintenance entries...
;;------------------------------------------------------------

; Probably does not work in xemacs...
(defun ciao-fix-old-version-maintenance ()
  (interactive)
  (goto-char (point-min))
  (if (search-forward "%% Control version comment prompting for" nil t)
      (let (tmp)
	(beginning-of-line)
	(kill-line 3)
	(next-line 1)
	(kill-line 1)
	(previous-line 1)
	(beginning-of-line)
	(set-mark (point))
	(search-forward "version-comments:")
	(search-forward "\"")
	(kill-region (mark) (point))
	(set-mark (point))
	(search-forward "\"")
	(backward-char 1)
	(setq tmp (buffer-substring-no-properties (mark) (point)))
	(kill-region (mark) (point))
	(kill-line 1)
	(insert 
	 (concat
	  ":- doc(version_maintenance,"
	  (cond
	   ((equal tmp "on") "on")
	   ((equal tmp "off") "off")
	   (t (concat "dir('" tmp "')")))
	  ").\n"
	  )))
    (error "Could not find version maintenance comment")))

;;;------------------------------------------------------------

;;------------------------------------------------------------
;; Graphical Menu
;;------------------------------------------------------------

(defvar ciao-g-showoutput nil
  "Stored the value of local variable of
ciao-build-ciaopp-specific-command because it is need in a hook after
clicking the OK button in the graphical menu.")


;; (let ((frame (make-frame '(
;; 			   (title . "ypuiii2")
;; 			   (background-color . "gray")
;; 			   (foreground-color . "black")
;; 			   (width . 50)
;; 			   (height . 50)
;; 			   (vertical-scroll-bars . right)
;; 			   (minibuffer . nil)
;; 			   (cursor-type . nil)
;; 			   (scroll-bar-foreground . "gray")
;; 			   (scroll-bar-background . "gray")
;; 			   )
;; 			 )
;; 	     ))
;;   (set-frame-size frame 60 30)
;;   (set-frame-position frame 
;; 		      (* (/ (- (screen-width)  (frame-width frame )) 2) (frame-char-width  frame))
;; 		      (* (/ (- (screen-height) (frame-height frame)) 2) (frame-char-height frame))
;; 		      )
;;   )



; magic line! (x-popup-menu `((0 0) ,(frame-first-window (car (frame-list)))) '("Titulo general" ("titulo" ("Print Buffer" . print-buffer) ("---"))) )
;; (x-popup-dialog `((0 0) ,(frame-first-window (car (frame-list))))
;; 	      '("Titulo" ("titulo" ("Print Buffer" . p1) ("---") ("Print Buffer2" . p2)) 
;; 		("titulo222" ("Print Buffer" . p3) ("---") ("Print Buffer2" . p4) 
;; 		 ("titulo444" ("Print Buffer" . p3) ("---") ("Print Buffer2" . p4)))
;; 		) 
;; )


(eval-when-compile (require 'wid-edit))

(defface ciao-button-widget-face 
  '((((type x w32 mac) (class color) (background dark))
     (:box (:line-width 2 :style released-button) 
	   :background "dark slate gray"
	   :foreground "goldenrod"
	   ))
    (((type x w32 mac) (class color) (background light))
     (:box (:line-width 2 :style released-button) 
	   :background "lightgrey"
	   :foreground "black"
	   ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)


(defface ciao-button-pressed-widget-face '((
     ((type x w32 mac) (class color) (background dark))
     (
;;    :inherit    ciao-button-widget-face
;;    :width      normal
;;    :italic     nil
;;    :weight     normal
;;    :italic     nil
      :background "dark slate gray"
      :foreground "goldenrod"
      :box        (:line-width 2 :style pressed-button)
      :slant       normal
      ))
    (((type x w32 mac) (class color) (background light))
     (
      :box        (:line-width 2 :style pressed-button)
      :background "lightgrey"
      :foreground "black"
      :slant       normal
      ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)


(defface ciao-edit-widget-face '(
     (((class color) (background dark))
     (:foreground "goldenrod"
      :background "dark slate gray"
;     :background "lightgrey" :foreground "black")
      :slant       italic
      ))
     (((class color) (background light))
     (:foreground "black"
      :background "lightgrey"
      :slant      italic
     ))
     (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defface ciao-text-widget-face '(
     (((class color) (background dark))
     (:foreground "lightgrey"))
     (((class color) (background light))
      (:foreground "black"))
     (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defface ciao-menu-error-widget-face '(
     (((class color) (background dark))
      (:foreground "firebrick"))
     (((class color) (background light))
      (:foreground "firebrick"))
     (t nil))
  "Face used for menu error representation in graphical interface."
  :group 'ciao-faces)

(defface ciao-menu-note-widget-face '(
     (((class color) (background dark))
      (:foreground "slate blue"))
     (((class color) (background light))
      (:foreground "slate blue"))
     (t nil))
  "Face used for menu note representation in graphical interface."
  :group 'ciao-faces)

(defvar ciao-widget-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t"           'widget-forward)
    (define-key map [(shift tab)]  'widget-backward)
    (define-key map [backtab]      'widget-backward)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map "\C-m"         'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.")


;; (defimage combo-box-arrow
;;   '((:type png :file "./icons/cb_arrow.png")
;;     (:type xbm :file "./icons/cb_arrow.xbm")
;;     ))


(defun ciao-create-enumerated-list (args num)
"Starting by number 'num', it returns a list of pairs (value number)
composed by 1 element of the list 'args' and successive numbers of
'num'.

 Example:
 (ciao-createenumerated-list '(a b c) 0)
  => ((0 a) (1 b) (2 c))"
(let ( (ele   (car args)) )
  (if (not (cdr args))
      `((,(number-to-string num) ,ele))
    (cons `(,(number-to-string num) ,ele)
	  (ciao-createenumerated-list (cdr args) (1+ num))))))

(defun ciao-createpair-list (args)
"For a given list (a b c), it creates the list ((a a) (b b) (c c))"
(let ((ele (car args)))
    (if (not (eq ele '()))
        (cons `(,ele ,ele) (ciao-createpair-list (cdr args)))
      '() )))

(defun ciao-gm-define-combo-box-list (args)
"For a given list of pair option-value, it returns a list of usable
combo-box items.

Example:
 (ciao-gm-define-combo-box-list '((aa 1) (bb 2) (cc 3)))
  => ((item :tag aa :value 1) (item :tag bb :value 2) (item :tag cc :value 3))"
(let ( (tag    (car  (car args)))
       (value  (cadr (car args)))
       (rest_a (cdr args)))
  (if (eq args '( ))
      '()
    ( cons
      `'(choice-item 
	 :tag         ; ,tag 
	 ,(concat tag (truncate-string-to-width "" (- 9 (length tag)) 0 ?  ))
	 :value       ,value 
	 :sample-face ciao-edit-widget-face
	 :format      "%[%{ %t %}%]"
	 :keymap widget-field-keymap
	 )
      (ciao-gm-define-combo-box-list rest_a)))))

(defun ciao-create-combo-box (id title args default_option)
"Returns a combo-box with args elements as options"
(let* ( (combo-list (eval (append
	   '(widget-create 'menu-choice
			   :widgetid    id
			   :tag         title
			   :menu-tag    "Choose"
			   :case-fold   'nil
			   :sample-face 'ciao-text-widget-face
			   :format      "%{%t:%} %v"
			   :value       default_option
			   :notify      'ciao-widget-hook
			   )
	   (ciao-gm-define-combo-box-list (ciao-createpair-list args))
	   )))
	(arrow      (widget-create 'push-button
			   :tag       "\\/"
			   :tag-glyph "icons/cb_arrow"
			   :sample-face 'ciao-edit-widget-face 
					; custom-button-face
					;   :button-face
					;   '(ciao-button-widget-face
					;   custom-button-face
					;   custom-button-pressed-face)
			   :format "%[%v %]\n"
;				    :ascent 'center
			   )))
  (widget-put arrow :parent combo-list)
  (widget-put arrow :action 'widget-parent-action)))

(defun ciao-widget-indent-text (title)
"Put spaces before title to make all text fit on the same columns

(ciao-widget-indent-text \"helooow\")
=> \"                          helooow\"

WARNING: do not remove the space between \"? )\", because '? ' MEANS
SPACE character"
  (let ((menu_width  33))
    (concat (make-string (max 0 (- menu_width (length title))) ? ) title)))

(defun ciao-widget-indent-center-text (title)
"Put spaces before title to make all text be centered."
  (let ((menu_width  80))
    (concat (make-string (max 0 (/ (- menu_width (length title)) 2))
			 ?  ) title)))

(defun ciao-widget-cb-indent-text (title)
  "Same than ciao-widget-indent-text but with another lengt (for combo-boxes)"
  (let*((menu_width  20)
	(half        (max 0 (/ (- menu_width (length title)) 2)))
	(rest        (max 0 (- (- menu_width (length title)) half)))
 	)
    (concat (make-string half ? ) title (make-string rest ? ))))

(defun ciao-remove-all-spaces (str)
  (string-match "\\([ ]*\\)\\([_a-z0-9A-Z]*\\)" str)
  (match-string 2 str))

(defun ciao-create-edit-box (wid title defopt)
"Returns an edit-box with defopt as value of the input field"
  (widget-create 'item
		 :sample-face 'ciao-text-widget-face
		 :format "%{%t: %}"
		 :tag (ciao-widget-indent-text title))
  (widget-create 'item
		 :sample-face 'ciao-edit-widget-face
		 :format "%{ %}")
  (widget-create 'editable-field
		 :widgetid  wid
		 :size 9
		 :action 'ciao-widget-hook
		 :valid-regexp "[a-zA-Z0-9, _]"
		 :value-face 'ciao-edit-widget-face
		 :format "%{%v%}"
		 defopt)
  (widget-insert " \n"))

;
; The _MAGIC_ regular expresion for parsing menu lines
;
(defvar ciao-ciaopp-toplevel-menu-regexp
  "\\([A-Z][a-zA-Z \\-]*\\): *\\(\\[[a-zA-Z0-9, _\t\n]*\\]\\)?[ \t\n]*\\(([^)]*)\\)[ \t\n]*\\(\\?\\)"
  "The regular expresion to parse a menu line.")

;; ;
;; ; The _MAGIC_ regular expresion for parsing a prompt
;; ;
;; (defun ciao-ciaopp-toplevel-prompt-regexp nil
;; ;  "\\([a-zA-Z]*\\) \\?-"
;;   "\\(ciao\\) \\?-"
;; )

;; Note that [...] below is defines the set of chars ]a-zA-Z0-9, [_:\t\n.-
(defvar ciao-ciaopp-toplevel-menu-error-regexp 
  "\\({ERROR:\\) *\\([]a-zA-Z0-9, [_:\t\n.-]*\\)}"
  "The regular expresion for parsing menu ERRORs")

(defvar ciao-ciaopp-toplevel-menu-note-regexp 
  "\\(Note:\\) *\\([]a-zA-Z0-9, [_:-]*\\)"
  "The regular expresion to parse menu Notes (which is how we mark the
ones we do not want to show)")

(defvar ciao-ciaopp-toplevel-menu-note-complex-regexp 
  "\\({NOTE \\(([^)]*)\\):\\) *\\([]a-zA-Z0-9,. [_:\t\n-]*\\)}"
  "The regular expresion to parse menu NOTES")

(defun is_an_error (str)
  "Decides if str belongs to the menu error format"
  (string-match ciao-ciaopp-toplevel-menu-error-regexp str))

(defun is_a_note (str)
  "Decides if str belongs to the menu note format"
  (string-match ciao-ciaopp-toplevel-menu-note-complex-regexp str))

(defun is_a_menu (str)
  "Decides if str belongs to the menu note format"
  (string-match ciao-ciaopp-toplevel-menu-regexp str))

(defun ciao-notify-error-widget (str)
"Generate a widget (item) to say that there was an error"
  (widget-create 'item
		 :tag (ciao-widget-indent-text str)
		 :sample-face 'ciao-menu-error-widget-face
		 :format "  %{%t%}\n\n"))

;; (defun ciao-notify-note-widget (str)
;; "Generate a widget (item) to say that there was a note"
;;   (widget-create 'item
;; 		 :tag (concat (ciao-widget-indent-text "") str)
;; 		 :sample-face 'ciao-menu-note-widget-face
;; 		 :format "  %{%t%}\n\n"))
(defun ciao-notify-note-widget (str)
"Generate a widget (item) to say that there was a note"
  (widget-create 'item
		 :tag (concat 
		       (ciao-widget-indent-center-text (concat "{" str "}"))
		       "\n")
		 :sample-face 'ciao-menu-note-widget-face
		 :format "%t"))

(defun ciao-get-combo-box-components-from-str (str)
"(ciao-get-combo-box-components-from-str 
    \"Select Menu Level:               [naive, expert] (naive) ?\")
 => (\"Select Menu Level\" (quote (\"naive\" \"expert\")) 
    (quote \"naive\"))"


  (string-match ciao-ciaopp-toplevel-menu-regexp str)
  (let ((title      (match-string 1 str))
	(ops_str    (match-string 2 str))
	(def_op     (match-string 3 str))
	(menu_width  43))
   `(
      ,(ciao-widget-indent-text            title)
     ',(ciao-get-toplevel-menu-options          ops_str)
     ',(ciao-get-toplevel-menu-default-option   def_op)
     )))

(defun ciao-get-toplevel-menu-default-option (str)
"Remove ( and ) from the parsed menu option. So, 
 \"(default_option)\" is transformed into \"default_option\""
  (if (eq str 'nil)
      'nil
    (string-match "(\\([_a-z0-9A-Z]*\\))" str)
    (ciao-widget-cb-indent-text (match-string 1 str))))

(defun ciao-get-toplevel-menu-options (str)
"Extract options from a list (written in a string):
(ciao-get-toplevel-menu-options \"[naive, expert]\")
 => (\"naive\" \"expert\")"
  (if (eq str 'nil)
      'nil
      (ciao-get-toplevel-menu-options-aux 1 str 1)))

(defun ciao-get-toplevel-menu-options-aux (argnum str strnum)
  (string-match "\\([_a-zA-Z0-9]*\\)\\([, \t\n]*\\)" str strnum)
  (let* (
	 (argnum1    (1+ argnum))
	 (result_op  (match-string 1 str))
	 (result_esp (match-string 2 str))
	 (the_end1   (match-end 1))
	 (the_end2   (match-end 2))
	 (iresult_op (ciao-widget-cb-indent-text result_op))
	 )
    (if (eq the_end1 the_end2)
	`(,iresult_op)
      (cons iresult_op (ciao-get-toplevel-menu-options-aux 
			(1+ argnum1) str the_end2)))))

;; (defun ciao-ciao-create-combo-box-components-from-str (id str)
;;   (eval (cons 
;; 	 'ciao-create-combo-box
;; 	 (cons id
;; 	  (ciao-get-combo-box-components-from-str str)
;; 	  ))))

(defun ciao-create-widget (id str &optional def_opt)
"Create a widget (combo or edit-box) depending on how the string is."
(let ( (a 0)
       (b 0)
       (c 0) 
       note_str )
  
  (if (is_an_error str)
      (progn 
	(setq a (match-end 2))
	(ciao-notify-error-widget (match-string 2 str))))
   
  (if (is_a_note str)
      (progn 
	(setq b (match-end 2))
	(setq note_str (match-string 3 str))
	(message (concat "*** String received:" note_str "***"))
	(ciao-notify-note-widget note_str)
	(message (concat "*** String received, again:" note_str "***"))
	(if (string= "Incorrect Option" note_str)
	    (progn
	      ; let's not try
	      (setq ciao-gm-recovering (1+ ciao-gm-recovering))
	      ; This is to avoid repeating the last "bad" option
	      ; We assume that id = ciao-widget-id
	      (setq ciao-widget-last-changed-option (1- id))
	      (setq ciao-widget-id (1- id))))))
  
  (if (is_a_menu str)
      (progn
	(setq c (+ 3 (match-end 3)))
	(let* ( (arglist (ciao-get-combo-box-components-from-str str))
		(title   (nth 0 arglist))
		(options (nth 1 arglist))
		(defopt  (if (equal def_opt 'nil)
			     (nth 1 (nth 2 arglist))
			   def_opt))
		(comboarg (cond 
			   ((equal def_opt 'nil) arglist)
			   ((not (equal def_opt 'nil))
			    (list title options `(quote ,def_opt)))))
		)
	  (if (equal options ''nil)
	      (ciao-create-edit-box id title defopt)
	    (if (not (equal (member defopt (cadr options)) nil))
		(eval (cons 'ciao-create-combo-box (cons id comboarg)))
;	      (prin1 (list "defopt: " defopt "  args: " options "\n\n"))
	      )))))
  (max a b c)))

(defun ciao-create-widgets-from-list (n l)
  "For a given list of strings (assumed to be ciaopp-menu strings), it
creates widgets for each element."
(let ((rest (cdr l)))
  (ciao-create-widget n (car l))
  (if (not (equal rest 'nil))
      (ciao-create-widgets-from-list (1+ n) rest)
    'nil
    )))

(defun ciao-create-widgets-buffer nil
       "Create the CiaoPP widgets buffer."
       (interactive)
       (kill-buffer (get-buffer-create 
		     (exe-buffer-name ciao-ciaopp-gmenu-buffer-name)))
       (set-buffer (get-buffer-create 
		    (exe-buffer-name ciao-ciaopp-gmenu-buffer-name)))
;       (switch-to-buffer (get-buffer-create (exe-buffer-name
;       ciao-ciaopp-gmenu-buffer-name)))
       (kill-all-local-variables)

;;       (custom-set-variables
;;	'(widget-image-directory "/home/clip/Systems/graphic-ciao-emacs-menu"))

; (set-default-font "-adobe-courier-bold-r-normal--*-240-*-*-m-*-iso8859-1")  
; (set-default-font "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")  
; (set-background-color "peach puff")
; (set-background-color "black")

;  (set (make-local-variable 'widget-field-face)
;                            'ciao-edit-widget-face)
       (set (make-local-variable 'widget-button-pressed-face)
	                          'ciao-button-pressed-widget-face)
       (set (make-local-variable 'widget-mouse-face)
	                         'ciao-button-widget-face) 

       (set (make-local-variable 'cursor-type) nil)

       (insert "              ")
       ;; (ciao-insert-image 'xpm ciao-clip-logo "CLIP")
       ;; (insert " ")
       (if (eq ciao-ciaopp-prog-lang 0)
	   (ciao-insert-image 'xpm ciao-logo "Ciao")
	   (ciao-insert-image 'gif java-logo "Java")
       )
       (insert "   ")
       (ciao-insert-with-face 
	"Preprocessor Option Browser" 'ciao-face-startup-mess)
       (insert "   ")
       (ciao-insert-image 'xpm "ciaocustomize.xpm" "Customize")
       (insert "\n\n")
)

(defun ciao-insert-with-face (string face)
  "Puts string in current buffer with face face."
  (let ((begin (point)))
    (insert string)
    (overlay-put 
     (make-overlay begin (+ (length string) begin) (current-buffer))
     'face face)))

(defun ciao-run-widget-buffer nil
  (use-local-map ciao-widget-keymap)
  (widget-setup)
  )

(defun ciao-create-widgets-buttons nil
  (widget-insert "\n                             ")
  (widget-create 'push-button
		 :tag         "cancel"
		 :tag-glyph   "icons/cancel2"
		 :button-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-cancel-button-widget-hook
		 "Push button")
  (widget-insert "  ")
  (widget-create 'push-button
		 :tag         "go"
		 :tag-glyph   "icons/go"
		 :button-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-ok-button-widget-hook
		 "Push button")
  )

(defun ciao-create-widget-buffer-from-list (str-list)
       "Create the widgets from str list."
       
       (ciao-create-widgets-buffer)
       (switch-to-buffer 
	(get-buffer-create (exe-buffer-name ciao-ciaopp-gmenu-buffer-name)))
       (widget-insert "\n")
       (ciao-create-widgets-from-list 1 str-list)

       (ciao-create-widgets-buttons)
       (ciao-run-widget-buffer)
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old tests --has to dissapear from here at some point.

(defun mytest ()
;;  (set-default-font "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")  
;;  (set-background-color "peach puff")
;  (set-background-color "black")
;  (set-background-color "white")
;  (ciao-startup)
;;  (find-file "qsort_func.pl")
;;   (split-window (selected-window) 20)
;;   (other-window 1)
  (ciao-widget-example)
  (setq cursor-type nil)
;;  (other-window 1)
  )


(defun ciao-widget-example ()  
  (interactive)
  ( let ((ciao-widget-list))
    (setq ciao-widget-list nil)

    (setq ciao-widget-list 
	  (cons "An Edit box:                     (on) ? " ciao-widget-list))
    (setq ciao-widget-list 
	  (cons "Select Menu Level:               [naive, expert] (naive) ?" 
		ciao-widget-list))

    (setq ciao-widget-list 
	  (cons "Select Action Group:             [analyze, check_assertions, optimize]
                                  (analyze) ? " 
		ciao-widget-list))

    (setq ciao-widget-list (cons "{NOTE (aaa): This is a compouse note
so it can have several lines}" ciao-widget-list))
    
    (setq ciao-widget-list (cons "Select Cost Analysis:            [none, steps_ub, steps_lb, steps_ualb, steps_o]
                                  (none) ? " ciao-widget-list))

    (setq ciao-widget-list (cons "Perform Non-Failure Analysis:    [none, nf, nfg] (none) ? " ciao-widget-list))
    (setq ciao-widget-list (cons "Perform Determinism Analysis:    [none, det] (none) ? " ciao-widget-list)) 
    (setq ciao-widget-list (cons "{Note: Just an strange note}
                            Print Program Point Info:        [off, on] (off) ? " ciao-widget-list))
    (setq ciao-widget-list (cons "Collapse AI Info:                [off, on] (on) ? " ciao-widget-list))
    (setq ciao-widget-list (cons "{NOTE (aaa): Just a note.}" ciao-widget-list))
    (setq ciao-widget-list (cons "{ERROR: Please specify more: [asdf,111aaa,asdf].}" ciao-widget-list))
    (setq ciao-widget-list (cons  "Use Saved Menu Configuration:    [none, mc1, mc1, mc1, mc1, default, demo1,
                                  demo1, d1, d1, algo, ver, ver1, ver2, df_h, 
                                  comp, rtcp2, rtc3, rtcp, ctc, ctrt] (none) ? " ciao-widget-list))

    (ciao-create-widget-buffer-from-list ciao-widget-list))
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
