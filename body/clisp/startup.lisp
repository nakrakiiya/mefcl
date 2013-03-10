
;; set the PATH of the hyperSpec
(setf *CLHS-ROOT-DEFAULT* (format nil "~a" (truename "../emacs/home/.emacs.d/clhs7/HyperSpec/")))

;; update source locations for all symbols
(defpackage "TEMP-PACKAGE"
  (:use :cl :ext))

(in-package "TEMP-PACKAGE")

;; CL-USER> (symbol-plist 'disassemble)
;; (SYSTEM::DOC
;;  (SYSTEM::IMPNOTES "debugger.html#disassemble" CLHS "Body/f_disass.htm" SYSTEM::FILE
;;   ((DEFGENERIC #P"G:\\gnu\\home\\src\\clisp\\clisp-2.49\\build-full\\disassem.fas" 15 71))))
;; CL-USER> (symbol-plist 'system-info)
;; (CLOS::CLOSCLASS #<STRUCTURE-CLASS SYSTEM-INFO> SYSTEM::DOC
;;  (SYSTEM::FILE (((SETF FIND-CLASS) #P"G:\\gnu\\home\\src\\clisp\\clisp-2.49\\build-full\\syscalls\\posix.fas" 192 268))))
;; CL-USER> (symbol-plist 'symbol-plist)
;; (SYSTEM::SETF-EXPANDER SYSTEM::%PUTPLIST SYSTEM::DOC
;;  (CLHS "Body/f_symb_4.htm" SYSTEM::FILE
;;   ((DEFSETF #P"G:\\gnu\\home\\src\\clisp\\clisp-2.49\\build-full\\places.fas" 925 926))))
;; (symbol-plist 'defun)          
;; (SYSTEM::DOC (SYSTEM::IMPNOTES "evaluation.html#defun" CLHS "Body/m_defun.htm" SYSTEM::FILE ((DEFMACRO "init" NIL NIL))))

(defparameter *source-location-dir-mappings*
  `((nil . ,(absolute-pathname ".\\src\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full") . ,(absolute-pathname  ".\\src\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "syscalls") . ,(absolute-pathname ".\\syscalls\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "regexp") . ,(absolute-pathname ".\\regexp\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "dirkey") . ,(absolute-pathname ".\\dirkey\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "i18n") . ,(absolute-pathname ".\\i18n\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "libsvm") . ,(absolute-pathname ".\\libsvm\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "linkkit") . ,(absolute-pathname ".\\linkkit\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "pcre") . ,(absolute-pathname ".\\pcre\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "rawsock") . ,(absolute-pathname ".\\rawsock\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "readline") . ,(absolute-pathname ".\\readline\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "wildcard") . ,(absolute-pathname ".\\wildcard\\"))
    ((:ABSOLUTE "gnu" "home" "src" "clisp" "clisp-2.49" "build-full" "zlib") . ,(absolute-pathname ".\\zlib\\"))
    ))

(defparameter *new-pathbase* (absolute-pathname ".\\"))

(defun new-pathbase-p (path)
  (and (string= (pathname-device *new-pathbase*)
                (pathname-device path))
       (let ((all-the-same t)
             (dirs (pathname-directory path))
             (base-dirs (pathname-directory *new-pathbase*)))
         (loop
            while base-dirs
            do
              (unless dirs
                (setq all-the-same nil)
                (return))
              (unless (equal (car dirs)
                             (car base-dirs))
                (setq all-the-same nil)
                (return))
              (setq dirs (cdr dirs))
              (setq base-dirs (cdr base-dirs)))
         all-the-same)))

;; (setf (second (first (getf (getf (symbol-plist 'disassemble) 'system::doc) 'system::file)))
;;       #P"D:\\mefcl-0.2\\clisp\\src\\disassem.lisp")

(defun replace-symbol-source-location (symbol)
  (when (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file))
    (let (processed)
      (if (new-pathbase-p (second (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file))))
          (setq processed t)
          (dolist (pair *source-location-dir-mappings*)
            (when (equal (pathname-directory (second (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file))))
                         (car pair))
              (let* ((new-dir (cdr pair))
                     (new-pathname (make-pathname :defaults new-dir
                                                  :name (pathname-name (second (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file))))
                                                  :type "lisp")))
                (setf (second (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file)))
                      new-pathname)
                (setf processed t)
                (return)))))
      ;; (unless processed
      ;;   (format t "unprocessed: ~A, path = ~A~%" symbol (second (first (getf (getf (symbol-plist symbol) 'system::doc) 'system::file)))))
      )))

(defun replace-all-symbols ()
  (let ((processed-symbols (make-hash-table :test 'eq)))
    (do-all-symbols (sym)
      (unless (gethash sym processed-symbols)
        (replace-symbol-source-location sym)
        (setf (gethash sym processed-symbols) t)))))


(replace-all-symbols)

(delete-package "TEMP-PACKAGE")






  
                                            
                                            

