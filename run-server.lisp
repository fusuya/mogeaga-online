(ql:quickload '(:jonathan :cl-ppcre :usocket :verbose :ftw))

(defpackage moge
  (:use #:cl #:ftw #:cffi #:sb-bsd-sockets))

(in-package moge)

(load "item.lisp" :external-format :utf-8)
(load "define.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)
(load "mogeaga-server.lisp" :external-format :utf-8)

(defun main ()
  (moge::server-main))
