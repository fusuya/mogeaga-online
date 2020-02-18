(ql:quickload '(:jonathan :cl-ppcre :usocket :verbose))

(defpackage moge
  (:use #:cl #:cffi #:sb-bsd-sockets))

(in-package moge)

(load "item.lisp" :external-format :utf-8)
(load "define.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)
(load "mogeaga-server.lisp" :external-format :utf-8)

(in-package :cl-user)

(defun main ()
  (moge::server-main))
