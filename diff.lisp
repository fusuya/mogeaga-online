(defpackage :diff
  (:use :cl)
  (:export :diff :patch))

(in-package :diff)

(defun diff (from to)
  (cond
   ((equal from to) ",,")
   ((and (consp from) (consp to))
    (diff-lists from to))
   (t
    to)))

(defun diff-lists (from to)
  (cond
   ((/= (length from) (length to))
    to)
   ((and (plistp from) (plistp to))
    (diff-plists from to))
   (t
    (diff-plain-lists from to))))

(defun plistp (list)
  (cond
   ((null list)
    t)
   ((and (atom (car list)) (cdr list))
    (plistp (cddr list)))
   (t
    nil)))

(defun diff-plain-lists (from to)
  (loop for x in from
	for y in to
	collect (diff x y)))

(defun property-names (plist)
  (cond
   ((null plist) nil)
   (t
    (cons (car plist) (property-names (cddr plist))))))

(defun diff-plists (from to)
  (if (and (equal (property-names from) (property-names to)))
      (%diff-plists from to)
    to))

(defun %diff-plists (from to)
  (cond
   ((null from) '())
   (t
    (append
     (list (car from)
	   (diff (cadr from) (cadr to)))
     (%diff-plists (cddr from) (cddr to))))))

(defun patch (from diff)
  (cond
   ((equal ",," diff) from)
   ((and (listp from) (listp diff))
    (patch-list from diff))
   (t
    diff)))

(defun patch-list (from diff)
  (cond
   ((/= (length from) (length diff))
    diff)
   ((plistp from)
    (patch-plist from diff))
   (t
    (patch-plain-list from diff))))

(defun patch-plist (from diff)
  (cond
   ((null from)
    nil)
   ((equal (second diff) ",,")
    (append (list (first from) (second from))
	    (patch-plist (cddr from) (cddr diff))))
   (t
    (append (list (first from) (patch (second from) (second diff)))
	    (patch-plist (cddr from) (cddr diff))))))

(defun patch-plain-list (from diff)
  (cond
   ((null from) nil)
   (t
    (cons
     (patch (first from) (first diff))
     (patch-plain-list (cdr from) (cdr diff))))))
