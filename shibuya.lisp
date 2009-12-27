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

;; なんかの実験に
(defun ycomb (f)
  ((lambda (proc)
     (funcall f (lambda (arg) (funcall (funcall proc proc) arg))))
   (lambda (proc)
     (funcall f (lambda (arg) (funcall (funcall proc proc) arg))))))

;; (funcall (ycomb ...) ..)が面倒なので
(defun ycall (fctn &rest args)
  (apply (ycomb fctn)
         args))

;; 例. 遅い
(flet ((fibf (f)
         (lambda (n)
           (if (< n 2)
               1
               (+ (funcall f (1- n))
                  (funcall f (- n 2)))))))
  (ycall #'fibf 39))
;⇒ 102334155
;----------
;(FLET ((FIBF (F) (LAMBDA (N) (IF (< N 2) 1 (+ (FUNCALL F (1- N)) (FUNCALL F (- N 2))))))) (DECLARE (FTYPE (FUNCTION (FUNCTION) FUNCTION) FIBF)) (YCALL #'FIBF 39)) took 66,643,181 microseconds (66.643180 seconds) to run 
;                    with 2 available CPU cores.
;During that period, 61,650,000 microseconds (61.650000 seconds) were spent in user mode
;                    2,090,000 microseconds (2.090000 seconds) were spent in system mode
;16,853,890 microseconds (16.853890 seconds) was spent in GC.
; 26,197,543,632 bytes of memory allocated.
;Intel(R) Core(TM)2 Duo CPU     P8600  @ 2.40GHz
