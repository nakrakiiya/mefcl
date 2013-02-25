;;; auto-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (toggle-auto-compile auto-compile-on-save-mode
;;;;;;  auto-compile-mode) "auto-compile" "auto-compile.el" (20635
;;;;;;  6124 0 0))
;;; Generated autoloads from auto-compile.el

(autoload 'auto-compile-mode "auto-compile" "\
Compile Emacs Lisp source files after the visiting buffers are saved.

After a buffer containing Emacs Lisp code is saved to its source file
update the respective byte code file.  If the latter does not exist do
nothing.  Therefore to disable automatic compilation remove the byte code
file.  See command `toggle-auto-compile' for a convenient way to do so.

This mode should be enabled globally, using it's globalized variant
`auto-compile-on-save-mode'.

\(fn &optional ARG)" t nil)

(defvar auto-compile-on-save-mode nil "\
Non-nil if Auto-Compile-On-Save mode is enabled.
See the command `auto-compile-on-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-compile-on-save-mode'.")

(custom-autoload 'auto-compile-on-save-mode "auto-compile" nil)

(autoload 'auto-compile-on-save-mode "auto-compile" "\
Toggle Auto-Compile mode in all buffers.
With prefix ARG, enable Auto-Compile-On-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Compile mode is enabled in all buffers where
`turn-on-auto-compile-mode' would do it.
See `auto-compile-mode' for more information on Auto-Compile mode.

\(fn &optional ARG)" t nil)

(autoload 'toggle-auto-compile "auto-compile" "\
Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to the
visited Emacs Lisp source file or `default-directory' if no such file is
being visited in the current buffer.  If the user exits with a directory
selected then all source files in that directory will have their status
set, otherwise just the selected file.

Toggling happens by either compiling the source files(s) or by removing
the respective byte code file(s).  See `auto-compile-mode'.

The appropriate action is determined by the existence respectively absence
of the byte code file for the selected source file.  If a directory was
selected but a source file was current when this command was invoked
use that file to determine the action.  Otherwise prompt the user.

To explicitly select an action use a positive prefix argument to compile
the source file(s) or a negative prefix argument to remove the respective
byte code file(s).

Note that even when a directory was selected, the action is determined
only once and then applied to all source files regardless of the presence
or absence of the respective byte code files.

\(fn FILE ACTION)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-compile-pkg.el") (20635 6124 375000
;;;;;;  0))

;;;***

(provide 'auto-compile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-compile-autoloads.el ends here
