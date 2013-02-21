;; requires cl-w32api from http://sourceforge.net/projects/cl-w32api/
;; put cl-w32api into the local-projects directory of quicklisp first
(ql:quickload "cl-w32api")
(ql:quickload "ltk")

(in-package :cl-user)

(load "launcher.lisp")

(ccl:save-application "Launcher.exe"
                      :toplevel-function 'ltk-user::appmain                 
                      :application-type :gui
                      :prepend-kernel t)

