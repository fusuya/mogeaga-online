(ql:quickload '(:jonathan :cl-ppcre :usocket :verbose :ftw))

(defpackage moge
  (:use #:cl #:ftw #:cffi #:sb-bsd-sockets))

(in-package moge)

(load "item.lisp" :external-format :utf-8)
(load "define.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)
;;(load "make-monster.lisp" :external-format :utf-8)
(load "mogeaga-server.lisp" :external-format :utf-8)
(load "mogeaga-client.lisp")

#|
(sb-ext:save-lisp-and-die "mogerpg"
        :toplevel #'main
        :save-runtime-options t
        :executable t)
|#
