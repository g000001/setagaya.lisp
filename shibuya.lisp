(defpackage :shibuya.lisp
  (:use :cl)
  (:nicknames :sl)
  (:export :listq
           :ycomb
           :ycall
           :cut
           :cute
           :<>
           :<...>
           :adefun
           :self
           :with-keyword-function
           :bind
           :fvlet
           :nalist-to-plist
           :nplist-to-alist
           :do#
           :lambda#
           :bind#
           :let#
           :defun#
           :defmacro#
           :define-layered-package
           ))


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


;; SRFI-26
;; テストしてない
(defmacro cut (&body form)
  (let ((form (if (member '<...> form)
		  (if (eq '<...> (car (last form)))
		      `(apply (function ,(car form)) ,@(cdr form))
		      (error "CUT:found garbage in lambda list when expecting a `<...>': ~S" (car (last form))))
		  form))
	(result)
	(gss))
    (dolist (item form `(lambda ,(nreverse gss) ,(nreverse result)))
      (case item
	(<> 
	 (let ((gs (gensym)))
	   (push gs result)
	   (push gs gss)))
	(<...> 
	 (let ((gs (gensym)))
	   (push gs result)
	   (push '&rest gss)
	   (push gs gss)))
	(cut)
	(otherwise
	 (push item result))))))

(defmacro cute (&body form)
  (let ((form (if (member '<...> form)
		  (if (eq '<...> (car (last form)))
		      `(apply (function ,(car form)) ,@(cdr form))
		      (error "CUT:found garbage in lambda list when expecting a `<...>': ~S" (car (last form))))
		  form))
	(result)
	(gss)    
	(binds))
    (dolist (item form `(let ,binds (lambda ,(nreverse gss) ,(nreverse result))))
      (case item
	(<> 
	 (let ((gs (gensym)))
	   (push gs result)
	   (push gs gss)))
	(<...>
	 (let ((gs (gensym)))
	   (push gs result)
	   (push '&rest gss)
	   (push gs gss)))
	(cut)
	(otherwise
	 (if (symbolp item)
	     (push item result)
	     (let ((gs (gensym)))
	       (push `(,gs ,item) binds)
	       (push gs result))))))))

;; Anaphoric DEFUN
;; http://cadr.g.hatena.ne.jp/g000001/20091214/1260799762
(DEFMACRO ADEFUN (NAME ARGS &BODY BODY)
  `(DEFUN ,NAME (,@ARGS)
     (MACROLET ((SELF (,@ARGS) `(,',NAME ,,@ARGS)))
       ,@BODY)))

;; Example
;;
;; (ADEFUN FIB (N)
;;  (IF (< N 2)
;;      1
;;      (+ (SELF (1- N))
;;         (SELF (- N 2)))))


(defun flatten (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
         (append (flatten (car lis)) (flatten (cdr lis))))
        (t (append (list (car lis)) (flatten (cdr lis))))))

;; キーワードなのに関数
;; http://cadr.g.hatena.ne.jp/g000001/20090929/1254234012
(DEFMACRO WITH-KEYWORD-FUNCTION (&BODY BODY)
  (LET ((KEYS (COLLECT-KEYWORD-SYMBOL BODY)))
    `(FLET (,@(MAPCAN (LAMBDA (K)
                        (COPY-LIST
                         `((,K (HASH-TABLE &OPTIONAL DEFAULT)
                               (GETHASH ,K HASH-TABLE DEFAULT))
                           ((SETF ,K) (NEW-VALUE HASH-TABLE)
                            (SETF (GETHASH ,K HASH-TABLE) NEW-VALUE)))))
                      KEYS))
       ,@BODY)))

(DEFUN COLLECT-KEYWORD-SYMBOL (LIST)
  (REMOVE-DUPLICATES
   (REMOVE-IF-NOT #'KEYWORDP (FLATTEN LIST))))


;; 変なlet(destructuring-bind)
;; http://cadr.g.hatena.ne.jp/g000001/20090928/1254067317
(DEFMACRO BIND (&BODY BODY)
  (DO ((BODY BODY (CDDR BODY))
       (BINDS () (DESTRUCTURING-BIND (VAR VAL &REST IGNORE) BODY
                   (DECLARE (IGNORE IGNORE))
                   (PUSH `(,(INTERN (SYMBOL-NAME VAR)) ,VAL) 
                         BINDS))))
      ((NOT (KEYWORDP (CAR BODY)))
       `(LET (,@(NREVERSE BINDS))
          ,@BODY))))

;; map-accum(gaucheより)
;; http://cadr.g.hatena.ne.jp/g000001/20090927/1254049969
(DEFGENERIC MAP-ACCUM (F SEED SEQUENCE &REST REST))
(DEFMETHOD MAP-ACCUM ((F FUNCTION) SEED (SEQUENCE SEQUENCE) &REST REST)
  (LET ((MIN-LEN (APPLY #'MIN (LENGTH SEQUENCE) (MAPCAR #'LENGTH REST))))
    (DO ((ACC SEED)
         TEM
         (IDX 0 (1+ IDX))
         (SEQS (CONS SEQUENCE REST))
         (RES (MAKE-SEQUENCE (CLASS-OF SEQUENCE) MIN-LEN)))
        ((= IDX MIN-LEN) (VALUES RES ACC))
      (SETF (VALUES TEM ACC)
            (APPLY F (NCONC (MAPCAR (LAMBDA (A) (ELT A IDX))
                                    SEQS)
                            (LIST ACC))))
      (SETF (ELT RES IDX) TEM))))

;; Example
;; (MAP-ACCUM (LAMBDA (X Y Z ACC) 
;;              (VALUES (LIST ACC X Y Z) (1+ ACC)))
;;            0
;;            '(A B C E E)
;;            '(F G H I)
;;            '(J K L))
;;⇒ ((0 A F J) (1 B G K) (2 C H L)),
;;   3
;; 
;; (MAP-ACCUM (LAMBDA (X Y ACC) 
;;              (VALUES (IF (CHAR< X Y) X Y)
;;                      (1+ ACC)))
;;            0
;;            "abCd"
;;            "ABcD")
;; ;⇒ "ABCD",
;; ;   4


;; FvLet
;; http://cadr.g.hatena.ne.jp/g000001/20090925/1253889176
(DEFMACRO FVLET ((&REST SPECS) &BODY BODY)
  (LET ((SYMS (MAPCAR #'CAR SPECS)))
    `(FLET (,@SPECS)
       (LET (,@(MAPCAR (LAMBDA (X) `(,X (FUNCTION ,X)))
                       SYMS))
         ,@BODY))))

;; Alist <-> Plist
(defun nalist-to-plist (alist)
  (do ((a alist (cddr a)))
      ((endp a) alist)
    (rotatef (cdr a) (caar a))
    (rotatef (caar a) (cdar a))
    (rotatef (cdr a) (car a))))

(defun nplist-to-alist (plist)
  (do ((p plist (cdr p)))
      ((endp p) plist)
    (rotatef (cdr p) (car p))
    (rotatef (caar p) (cdar p))
    (rotatef (cdr p) (caar p))))

;; do#
;; びっくりするほど使えないマクロ
;; http://cadr.g.hatena.ne.jp/g000001/20090228/1235760220
(defun car-safe (form)
  (if (consp form)
      (car form)
      form))

(defun reduce-unintern-sym (sym expr wo)
  (subst sym sym
         expr
         :test (lambda (x y)
                 (and (symbolp y)
                      (not (symbol-package y))
                      (not (member y wo))
                      (string= x y)))))

(defmacro do# (varlist test &body body)
  (let ((syms (remove-duplicates
               (remove-if #'symbol-package
                          (mapcar #'car-safe varlist))
               :test #'string=)))
    (reduce (lambda (res x)
              (reduce-unintern-sym x res nil))
            syms
            :initial-value
            `(do ,varlist
                 ,test
               ,@(when syms `((declare (dynamic-extent ,@syms))))
               ,@body))))

;; lambda# bind# let# defun#
;; 名前の統一感がいまいち
;; http://cadr.g.hatena.ne.jp/g000001/20090222/1235279666
(defmacro lambda# ((&rest bvl-spec) &body body)
  (let ((ignores (remove-if #'symbol-package bvl-spec)))
    `(lambda ,bvl-spec 
       ,@(when ignores `((declare (ignore ,@ignores))))
       ,@body)))

(defmacro bind# (bvl-spec values &body body)
  (let ((ignores (remove-if #'symbol-package bvl-spec)))
    `(multiple-value-bind ,bvl-spec ,values
       ,@(when ignores `((declare (ignore ,@ignores))))
       ,@body)))

(defmacro let# (bvl-spec values &body body)
  (let ((ignores (remove-if #'symbol-package bvl-spec)))
    `(destructuring-bind ,bvl-spec ,values
       ,@(when ignores `((declare (ignore ,@ignores))))
       ,@body)))

(defmacro defun# (name lambda-list &body body)
  (flet ((&rest#-p (x) (string-equal '&rest# x)))
    (let ((dynamic (second (member-if #'&rest#-p lambda-list))))
      `(defun ,name ,(substitute-if '&rest #'&rest#-p lambda-list) 
         ,@(when dynamic `((declare (dynamic-extent ,dynamic))))
         ,@body))))


;; defmacro#
;; Let Over Lambdaのdefmacro!に対抗
;; http://cadr.g.hatena.ne.jp/g000001/20090219/1235031781
(defmacro *defmacro/# (wo name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if #'symbol-package
                          (flatten body))
               :test #'string=)))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(symbol-name s))))
              syms)
         ,@(reduce (lambda (res x)
                     (reduce-unintern-sym x res wo))
                   syms
                   :initial-value body)))))

(defmacro defmacro# (name args &rest body)
  (let* ((os (remove-if #'symbol-package args))
         (gs (mapcar #'copy-symbol os)))
    `(*defmacro/# ,os ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; Example
;; (defmacro# square (#:x)
;;   `(* ,#:x ,#:x))

;; ;; 展開
;; (let ((x 3))
;;   (square (incf x)))
;; ;=>
;; (LET ((X 3))
;;   (LET ((#:X2531 (INCF X)))
;;     (* #:X2531 #:X2531)))

;; なんの役にも立たなそうなのに無駄に野心的
;; http://cadr.g.hatena.ne.jp/g000001/20090116/1232070370
(defmacro define-layered-package (name &rest args)
  (let ((base (find :base args :key #'car-safe)))
    (if base
        `(PROG1
           (defpackage ,name ,@(remove :base args :key #'zl:car-safe))
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (do-symbols (x (find-package ,(second base)))
               (when (eq (find-package ,(second base))
                         (symbol-package x))
                 (let ((sym (intern (format nil ">~A" x) ,name)))
                   (when (fboundp x)
                     (if (macro-function x)
                         (setf (macro-function sym)
                               (macro-function x))
                         (setf (symbol-function sym) 
                               (symbol-function x))))
                   (when (boundp x)
                     (setf (symbol-value sym) (symbol-value x)))
                   (setf (symbol-plist sym) (symbol-plist x)))))))
        `(DEFPACKAGE ,name ,@args))))
