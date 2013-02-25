;;; auto-complete-etags-docs.el --- Auto-completion source for etags

;; Copyright 2012 Xiaofeng Yang
;;
;; Author: Xiaofeng Yang
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags-docs)
;;   (aced-update-ac-source-etags)

;;; Code:

(require 'auto-complete-etags)
(require 'etags-select)
(require 'cl)

(defun aced-etags-select-insert-matches (tagname tag-file tag-count)
  "Insert matches to tagname in tag-file."
  (let (;; (tag-table-buffer (etags-select-get-tag-table-buffer tag-file))
        (tag-file-path (file-name-directory tag-file))
        (tag-regex (concat "^.*?\\(" "\^?\\(.+[:.']" tagname "\\)\^A"
                           "\\|" "\^?" tagname "\^A"
                           "\\|" "\\<" tagname "[ \f\t()=,;]*\^?[0-9,]"
                           "\\)"))
        (case-fold-search (etags-select-case-fold-search))
        full-tagname tag-line filename current-filename)
    (cons
     (with-output-to-string
       (with-temp-buffer
         ;; (set-buffer tag-table-buffer)
         (insert-file-contents-literally tag-file)
         
         (modify-syntax-entry ?_ "w")
         (goto-char (point-min))
         (while (search-forward tagname nil t)
           (beginning-of-line)
           (when (re-search-forward tag-regex (point-at-eol) 'goto-eol)
             (setq full-tagname (or (etags-select-match-string 2) tagname))
             (setq tag-count (1+ tag-count))
             (beginning-of-line)
             (re-search-forward "\\s-*\\(.*?\\)\\s-*\^?")
             (setq tag-line (etags-select-match-string 1))
             (end-of-line)
             (save-excursion
               (re-search-backward "\f")
               (re-search-forward "^\\(.*?\\),")
               (setq filename (etags-select-match-string 1))
               (unless (file-name-absolute-p filename)
                 (setq filename (concat tag-file-path filename))))
             (save-excursion
               ;; (set-buffer etags-select-buffer-name)
               (when (not (string= filename current-filename))
                 ;; (insert "\nIn: " filename "\n")
                 (princ (format "\nIn: %s\n" filename))
                 
                 (setq current-filename filename))
               ;; (insert (int-to-string tag-count) " [" full-tagname "] " tag-line "\n")
               (princ (format "%d [%s] %s\n" tag-count full-tagname tag-line))

               )))
         (modify-syntax-entry ?_ "_")))
     tag-count)))

(defun aced-etags-select-find (tagname)
  "Returns the tags."
  (let ((tag-files (etags-select-get-tag-files))
        (tag-count 0))
    (with-output-to-string
      (mapcar (lambda (tag-file)
                (let ((result (aced-etags-select-insert-matches tagname tag-file tag-count)))
                  (princ (car result))
                  (setq tag-count (cdr result))))
              tag-files))))

;; avoid ask the user while completing
(when (eq 'ask-user tags-add-tables)
  (setq tags-add-tables t))


(defun aced-etags-complete-doc (item)
  (aced-etags-select-find (substring-no-properties item)))

;; TODO this might override the original settings
;; (setq ac-source-etags
;;       '((candidates . (lambda ()
;;                         (all-completions ac-target (tags-completion-table))))
;;         (candidate-face . ac-etags-candidate-face)
;;         (selection-face . ac-etags-selection-face)
;;         (document . aced-etags-complete-doc)
;;         (requires . 2)))

(defun aced-update-ac-source-etags ()
  (if (assoc 'document ac-source-etags)
      (setf (cdr (assoc 'document ac-source-etags)) 1)
    (add-to-list 'ac-source-etags '(document . aced-etags-complete-doc))))

(provide 'auto-complete-etags-docs)
