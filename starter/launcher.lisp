;; (ql:quickload "ltk")

(in-package :ltk-user)

(defvar *application-executable* nil)
(defvar *starter-config-directory* "d:\\mefcl-0.2\\")
(defvar *starter-config-file* "d:\\mefcl-0.2\\starter.exe.conf")

(defun w32native-translated-namestring (path)
  (map 'string #'(lambda (x)
                   (if (char= x #\/)
                       #\\
                       x))
       (ccl:native-translated-namestring path)))

(defun init-starter ()
  (setq *application-executable*
        (merge-pathnames (first ccl:*command-line-argument-list*) (ccl:current-directory)))
  (setq *starter-config-directory*
        (make-pathname :name nil :type nil :defaults *application-executable*))
  (setq *starter-config-file*
        (concatenate 'string (w32native-translated-namestring *application-executable*) ".conf"))
  (setq *wish-pathname*
        (w32native-translated-namestring (merge-pathnames "tcl/bin/wish.exe" *application-executable*))))

(defun make-callback (path &optional (show-cmd #$SW_HIDE))
  (lambda ()
    (ccl:with-filename-cstrs ((spath (ccl:native-translated-namestring path)))
      (ccl:with-encoded-cstrs :utf-16le ((sopen "open"))
        (#_ShellExecuteW (ccl:%null-ptr)
                         sopen
                         spath
                         (ccl:%null-ptr)
                         (ccl:%null-ptr)
                         show-cmd)))))
                         

(defun load-config-file (frame path)
  (let ((buttons nil))
    (with-open-file (f path
                       :direction :input
                       :external-format (ccl:make-external-format :line-termination :windows))
      (read-line f)                     ; ignore the first line
      (loop
         (let ((s (read-line f))
               app-name
               launcher
               marker
               show-cmd)
           (when (string= s "(END)")
             (return))
           (setq app-name s)
           (setq launcher (concatenate 'string
                                       (w32native-translated-namestring *starter-config-directory*)
                                       (read-line f)))
           (setq marker (concatenate 'string
                                     (w32native-translated-namestring *starter-config-directory*)
                                     (read-line f)))
           (setq s (read-line f))
           (if (string= s "SW_SHOWNORMAL")
               (setq show-cmd #$SW_SHOWNORMAL)
               (setq show-cmd #$SW_HIDE))
           (unless (string= s "")
             (read-line f))
           (when (probe-file marker)
             (push (make-instance 'button
                                  :master frame
                                  :text app-name
                                  :command (make-callback launcher show-cmd))
                   buttons)))))
    (push (make-instance 'button
                         :master frame
                         :text "Close"
                         :command (lambda ()
                                    (exit-wish)))
          buttons)
    (reverse buttons)))

(defun mainform ()
  (let ((*wish-args* '("-name" "\"My Emacs For Common Lisp\"")))
    (with-ltk ()
      (let* ((f (make-instance 'frame))
             (buttons (load-config-file f *starter-config-file*)))
        (mapc #'(lambda (x)
                  (pack x :fill :both :ipady 5 :ipadx 20))
              buttons)
        (pack f)
        ;; disable the maximize button
        (resizable *tk* "false" "false") ; use *tk* to access the root object
        (font-configure "TkDefaultFont" :size 15)))))


(defun appmain ()
  (init-starter)
  (mainform))






