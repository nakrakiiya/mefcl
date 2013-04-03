(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/tabbar")

(require 'cl)

;; Title Bar
(setq frame-title-format "My Emacs For Common Lisp - %b")

;; load packages ( by default, the packages are loaded after ~/.emacs.d/init.el after this file )
(require 'package)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize cygwin integration
;(load "cygwin-init.el")

;; set default temp directory to ~/../tmp.
;; if use the default value `$HOME/Local Settings/Temp', it may cause an error when using clisp in Windows XP
(setq temporary-file-directory (expand-file-name "~/../tmp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Starting Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
(require 'powerline)
;; (powerline-default-theme)
;; (setq powerline-arrow-shape 'arrow)   ;; the default
;; (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Lucida Console" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicad
(require 'unicad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep+
(require 'grep+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime
;; set common lisp path according to $CURRENT_COMMON_LISP_TYPE
(setq *current-common-lisp-type* (getenv "CURRENT_COMMON_LISP_TYPE"))
(cond 
 ((string= *current-common-lisp-type* "CCL32")
  (setq inferior-lisp-program "~/../../ccl/run-ccl32.bat"))
 ((string= *current-common-lisp-type* "CCL64")
  (setq inferior-lisp-program "~/../../ccl/run-ccl64.bat"))
 ((string= *current-common-lisp-type* "CLISP")
  (setq inferior-lisp-program "~/../../clisp/run-clisp.bat"))
 ((string= *current-common-lisp-type* "ECL32-MINGW")
  (setq inferior-lisp-program "~/../../ecl-mingw/run-ecl32-mingw.bat"))
 ((string= *current-common-lisp-type* "ABCL")
  (setq inferior-lisp-program "~/../../abcl/abcl.bat"))
 ((string= *current-common-lisp-type* "SBCL32")
  (setq inferior-lisp-program "~/../../sbcl/run-sbcl32.bat"))
 ((string= *current-common-lisp-type* "SBCL64")
  (setq inferior-lisp-program "~/../../sbcl/run-sbcl64.bat"))
 ;; TODO add more implementations here
 (t nil))
(setq common-lisp-hyperspec-root (expand-file-name "~/.emacs.d/clhs7/HyperSpec/"))
(slime-setup '(slime-fancy)) ; almost everything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ac-slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
 '(add-to-list 'ac-modes 'slime-repl-mode 'slime-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show documentation when hit C-c C-h in 
(defun jsj-ac-show-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (pos-tip-show (concatenate 'string
                               (if (equal major-mode 'emacs-lisp-mode)
                                   (ac-symbol-documentation s)
                                 (ac-slime-documentation (symbol-name s)))
                               "


") ; it eat up the last 2 lines of the document !!!
                  'popup-tip-face
                  ;; 'alt-tooltip
                  (point)
                  nil
                  -1)))

(define-key lisp-mode-shared-map (kbd "C-c C-h") 'jsj-ac-show-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prolog mode

(require 'prolog)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)  ; optional, the system you are using;
                           ; see `prolog-system' below for possible values
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode) ; default to prolog mode
                                ("\\.m$" . mercury-mode)
                                ("\\.P$" . xsb-mode) ;; <-- for XSB only
                                ;; ("\\.ecl$" . prolog-mode) ;; <-- for ECLiPSe only
                                )
                               auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eclipse mode
(autoload 'eclipse-mode "eclipse.el" "ECLiPSe editing mode" t)
(autoload 'eclipse-esp-mode "eclipse.el" "ECLiPSe-ESP editing mode" t)
(setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.esp" . eclipse-esp-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cedet

;; (load "cedet.el")
(load "~/.emacs.d/site-lisp/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)                     

;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-parentheses

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow delimiters
(global-rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autopair
(require 'autopair)
(require 'auto-pair+)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))
(add-hook 'lisp-mode-hook
          #'(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'lisp-mode-hook #'(lambda () (autopair-mode)))
;(add-hook 'slime-repl-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (autopair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent when ENTER pressed in lisp mode
(defun my-newline-and-indent ()
  (interactive)
  (newline-and-indent)
  (slime-reindent-defun))
(define-key lisp-mode-shared-map (kbd "RET") 'my-newline-and-indent) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-indentation
(add-hook 'slime-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'slime-repl-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags-table
(require 'etags-select)
(require 'etags-table)
(setq etags-table-search-up-depth 10)
(setq etags-table-alist
      (list
      
       ;; TODO create the system-level tags
      
       ;; For jumping to Mercury standard headers:
       ;; '(".*\\.m" "~/.emacs.d/tags/MERCURY-TAGS")
       ;; For jumping to XSB system's libraries:
       ;; '(".*\\.P$" "~/.emacs.d/tags/XSB-TAGS")
       ;; For jumping to Ciao system's libraries:
       ;; '(".*\\.pl$" "~/.emacs.d/tags/CIAO-TAGS")
       ;; For jumping to ECLiPSe system's libraries
       ;; '(".*\\.ecl$" "~/.emacs.d/tags/ECLIPSE-TAGS")
       ;; '(".*\\.pl$" "~/sys-tags/eclipse/TAGS")
       ;; For jumping to C system's libraries
       ;; '(".*\\.[ch]$" "~/sys-tags/c/TAGS")  <-- always crashes
       ;; For jumping across project:
       ;; '("/home/devel/proj1/" "/home/devel/proj2/TAGS" "/home/devel/proj3/TAGS")
       ;; '("/home/devel/proj2/" "/home/devel/proj1/TAGS" "/home/devel/proj3/TAGS")
       ;; '("/home/devel/proj3/" "/home/devel/proj1/TAGS" "/home/devel/proj2/TAGS")
       ))
(defun etags-select-get-tag-files ()
  "Get tag files."
  (if etags-select-use-xemacs-etags-p
      (buffer-tag-table-list)
    (mapcar 'tags-expand-table-name tags-table-list)
    (tags-table-check-computed-list)
    tags-table-computed-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete-etags
(require 'auto-complete-etags)
;; (add-to-list 'ac-sources 'ac-source-etags)
;; (setq ac-etags-use-document t)

(defun add-ac-source-etags ()
  (add-to-list 'ac-sources 'ac-source-etags))

(add-hook 'prolog-mode-hook 'add-ac-source-etags)
(add-hook 'eclipse-mode-hook 'add-ac-source-etags)

(require 'auto-complete-etags-docs)
(aced-update-ac-source-etags)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote nil))
 '(ac-ignore-case t)
 '(ac-modes (quote (slime-repl-mode emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode slime-mode lisp-mode prolog-mode prolog-inferior-mode ciao-mode mercury-mode xsb-mode eclipse-mode)))
 '(ac-use-menu-map t)
 '(ac-use-overriding-local-map t)
 '(ahs-modes (quote (actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode slime-mode slime-repl-mode lisp-mode mercury-mode)))
 '(ahs-suppress-log nil)
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("1df4f61bb50f58d78e88ea75fb8ce27bac04aef1032d4ea6dafe4667ef39eb41" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" default)))
 '(ecb-auto-activate t)
 '(ecb-clear-caches-before-activate nil)
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("*slime-events*") ("*inferior-lisp*") ("*prolog*"))))
 '(ecb-compilation-major-modes (quote (compilation-mode slime-repl-mode)))
 '(ecb-compile-window-height 0.3)
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-display-default-dir-after-start t)
 '(ecb-enlarged-compilation-window-max-height 1.0)
 '(ecb-layout-always-operate-in-edit-window (quote (delete-other-windows switch-to-buffer)))
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote edit-and-compile))
 '(ecb-select-edit-window-on-redraw t)
 '(ecb-source-path (quote ("~")))
 '(ecb-split-edit-window-after-start nil)
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.2)
 '(ede-project-directories (quote nil))
 '(global-auto-highlight-symbol-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-semantic-decoration-mode t nil (semantic-decorate-mode))
 '(global-semantic-highlight-edits-mode t nil (semantic-util-modes))
 '(global-semantic-highlight-func-mode t nil (semantic-util-modes))
 '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic-idle))
 '(global-semantic-idle-scheduler-mode t nil (semantic-idle))
 '(global-semantic-idle-summary-mode t nil (semantic-idle))
 '(global-semantic-mru-bookmark-mode t nil (semantic-util-modes))
 '(global-semantic-show-parser-state-mode t nil (semantic-util-modes))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(global-semanticdb-minor-mode t)
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(home-end-enable t)
 '(indicate-empty-lines t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(scroll-bar-mode (quote right))
 '(semantic-edits-verbose-flag nil)
 '(semantic-idle-summary-function (quote semantic-format-tag-summarize-with-file))
 '(semantic-idle-work-parse-neighboring-files-flag t)
 '(semantic-idle-work-update-headers-flag t)
 '(semantic-tag-folding-show-tooltips t)
 '(show-paren-mode t)
 '(slime-auto-connect (quote always))
 '(slime-auto-select-connection (quote always))
 '(slime-autodoc-use-multiline-p t)
 '(slime-complete-symbol*-fancy t)
 '(slime-kill-without-query-p t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(slime-when-complete-filename-expand t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that, for
;; example.
(setq-default indent-tabs-mode nil)

;; if indent-tabs-mode is off, untabify before saving
;; (add-hook 'write-file-hooks
;;           (lambda () (if (not indent-tabs-mode)
;;                                          (untabify (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Some utilities for windows.
(recentf-mode)                      ;; Add menu-item "File--Open recent"
;; Define some additional "native-Windows" keystrokes (^tab, Alt/F4, ^A, ^F, ^O,
;; ^S, ^W) and redefine (some of) the overridden Emacs functions.
(global-set-key [C-tab] 'other-window)
(global-set-key [M-f4] 'save-buffers-kill-emacs)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key [f3] 'isearch-repeat-forward)
(global-set-key "\C-o" 'find-file)
(global-set-key "\C-s" 'save-buffer)
(global-set-key "\C-w" 'kill-this-buffer)
(global-set-key (kbd "C-S-o") 'open-line)
(global-set-key (kbd "C-S-w") 'kill-region)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
;;;; End of utilities.

;; tabbar again
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
 This function group all buffers into 2 groups, depending to the result value of `ecb-compilation-buffer-p'.
 This allows grouping in eclipse style."
  (cond
   ((ecb-compilation-buffer-p (buffer-name))
    '("Compilation Buffers")
    )
   (t
    '("Editing Buffers")
    )
   ))
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)  
(tabbar-mode)


;;;; Some goodies
(global-set-key (kbd "C-x C-b") 'bs-show)         ; Using BufferSelection for switching between buffers.
;;;; End of goodies

(ecb-activate)

;; Start SLIME automatically
(slime)
