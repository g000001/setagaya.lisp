(defpackage :shibuya.lisp
  (:use :cl)
  (:nicknames :sl)
  (:export 
   :listq))

(in-package :sl)

;; TAOより拝借
;; (list 'a 'b 'c 'd)などと書くのが面倒だと思ったときに
;; 
;; (listq foo bar baz)
;; ;⇒ (FOO BAR BAZ)

(defmacro listq (&rest args)
  `(list ,@(mapcar (lambda (x) `',x)
                   args)))






