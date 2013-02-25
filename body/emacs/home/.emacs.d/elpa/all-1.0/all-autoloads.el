;;; all-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (all) "all" "all.el" (20634 1666 0 0))
;;; Generated autoloads from all.el

(autoload 'all "all" "\
Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer.

\(fn REGEXP &optional NLINES)" t nil)

;;;***

;;;### (autoloads nil nil ("all-pkg.el") (20634 1667 338000 0))

;;;***

(provide 'all-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-autoloads.el ends here
