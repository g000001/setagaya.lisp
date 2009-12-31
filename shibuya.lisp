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
           :zap))


(in-package :sl)

;; Compile時定義でも効くdefun
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defun-compile-time (function-name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defun ,function-name ,lambda-list ,@body)
       #-(or akcl harlequin-common-lisp)
       (eval-when (:compile-toplevel) (compile ',function-name)))))

;; Arcより
(defmacro zap (op place &rest args)
  `(setf ,place (apply (function ,op) ,place (LIST ,@args))))

;(LET ((X 1))
;  (ZAP 1+ X)
;  X)
;=> 2

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
#|(flet ((fibf (f)
         (lambda (n)
           (if (< n 2)
               1
               (+ (funcall f (1- n))
                  (funcall f (- n 2)))))))
  (ycall #'fibf 39))|#
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


(defun-compile-time flatten (lis)
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

(DEFUN-COMPILE-TIME COLLECT-KEYWORD-SYMBOL (LIST)
  (REMOVE-DUPLICATES
   (REMOVE-IF-NOT #'KEYWORDP (FLATTEN LIST))))


;; 変なlet(destructuring-bind)
;; http://cadr.g.hatena.ne.jp/g000001/20090928/1254067317
(DEFMACRO BINDK (&BODY BODY)
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
(defun-compile-time car-safe (form)
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

;; MDLのmapf
;; http://cadr.g.hatena.ne.jp/g000001/20081020/1224431259
(PROGN
  (defun mapleave (&optional vals)
    (throw '#0=(gensym "MAPFLEAVE-") vals))

  (defun mapstop (&rest vals)
    (throw '#1=(gensym "MAPSTOP-") (copy-list vals)))

  (defun mapret (&rest vals)
    (throw '#2=(gensym "MAPRET-") (copy-list vals)))

  (defun mapf (finalf loopf &rest lists)
    ;; mapleave
    (catch '#0#
      (prog* ((lists (copy-tree lists)) 
              (len (length lists))
              (ans (list :ans))
              (tem ans))
       :top  (when (some #'endp lists) (go :fin))
             (progn
               ;; mapstop
               (setf (cdr tem)
                 (catch '#1#
                   ;; mapret
                   (setf (cdr tem)
                         (catch '#2#
                           ;; nomal
                           (setf (cdr tem)
                                 (list
                                  (apply loopf (and lists 
                                                    (mapcar #'car lists)))))
                           (or finalf (go :esc)) ;finalf?
                           (setf tem (cdr tem))
                           (go :esc)))
                   (setf tem (last tem))
                   (go :esc)))
               (setf tem (last tem))
               (go :fin))
       :esc  (dotimes (i len) (pop (nth i lists)))
         (go :top)
       :fin  (return (and finalf (apply finalf (cdr ans))))))))

;; Example 
;; (defmacro once-only ((&rest vars) &body body)
;;   (mapf (lambda (&rest arg)
;;           `(let ,(mapcar #'first arg)
;;              (let ,(mapcar (lambda (x) `(,(second x) (gensym))) arg)
;;                `(let (,,@(mapcar #'third arg))
;;                   ,,@body))))
;;         (lambda (v &aux (g (gensym)))
;;           `((,g ,v) ,v `(,,v ,,g)))
;;         vars))

;; (defun mappend (fn &rest lists)
;;   (apply #'mapf #'append fn lists))

;; (defun first-nonzero (list)
;;   (mapf ()
;;         (lambda (x)
;;           (when (not (zerop x)) (mapleave x)))
;;         list))

;; (first-nonzero '(0 0 0 0 9 0 0))
;; ;=> 9

;; (defun odd-list (list)
;;   (mapf #'list
;;         (lambda (x) (if (oddp x)
;;                         x
;;                         (mapret)))
;;         list))

;; (odd-list '(1 2 3 4 5))
;; ;=> (1 3 5)

;; (defun odd-list2 (list)
;;   (mapf #'list
;;         (lambda (x) (if (oddp x)
;;                         x
;;                         (mapret 'e 'ven)))
;;         list))

;; (odd-list2 '(1 2 3 4 5))
;; ;=> (1 E VEN 3 E VEN 5)

;; (defun first-ten (list)
;;   (let ((cnt 10))
;;     (mapf #'list
;;           (lambda (x)
;;             (when (zerop (decf cnt)) (mapstop 10))
;;             x)
;;           list)))

;; (first-ten '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ;=> (1 2 3 4 5 6 7 8 9 10)

;; (defun lnum (n &aux (cnt 0))
;;   (mapf #'list
;;         (lambda ()
;;           (if (<= n (incf cnt))
;;               (mapstop n)
;;               cnt))))
;; ;=> (lnum 10)
;; (1 2 3 4 5 6 7 8 9 10)

;; L4u的
;; http://cadr.g.hatena.ne.jp/g000001/20081018/1224341021
(defmacro with-l4u (&body body)
  `(let (it)
     (macrolet ((-> (fn &rest args)
                  `(apply #',fn it ',args)))
       ,@(mapcar (lambda (x)
                   `(setq it ,x))
                 body))))
;(with-l4u
;  4
;  (-> print)
;  (-> list :foo :bar :baz)
;  (print it))
; 
;>>> 4 
;>>> (4 :FOO :BAR :BAZ) 

;; lisp1的
;; http://cadr.g.hatena.ne.jp/g000001/20081015/1224023297
(defmacro with-lisp1 (&body body)
  (let ((syms (remove-if-not (lambda (x) 
                               (and (symbolp x) 
                                    (fboundp x)
                                    (not (eq 'quote x))))
                             (flatten body))))
    `(let ,(mapcar (lambda (x) `(,x (symbol-function ',x))) syms)
       (declare (ignorable ,@syms))
       ,@body)))

;; ;; 動作
;; (with-lisp1
;;   (mapcar 1+ '(1 2 3 4)))
;; ;=> (2 3 4 5)

;; (with-lisp1 
;;   (sort (list 38 29 3 1) <))

;; ;=> (1 3 29 38)


;; TAOの!(論理関数のOR) 勘違い実装編
;; http://cadr.g.hatena.ne.jp/g000001/20081013/1223860187
(defmacro ! (&body forms)
  (let ((aux-vars (and (consp (car forms))
                       (string-equal '&aux (string (caar forms)))
                       (prog1 (cdar forms) (pop forms))))
        (exit (gensym "EXIT-")))
    (cl:loop 
       :with cuts 
       :and tags := (list exit)
       :and body 
       :and ans := (gensym "ANS-")

       :for x :in forms
       :if (and (symbolp x) (string-equal '! x))
       :do (progn
             (push (gensym "CUT-") cuts)
             (push `(if ,(car cuts) (go ,exit) (setq ,(car cuts) t))
                   (cdr body)))
       :else 
       :do (progn
             (push (gensym "TAG-") tags)
             (push (car tags) body)
             (push `(and (setq ,ans ,x) (go ,(cadr tags))) body))
       :finally (return `(prog* (,ans ,@aux-vars ,@cuts)
                            ,@(nreverse body)
                            ,exit
                            (return ,ans))))))

;; (! (&aux (foo 0) result)
;;    result
;;    (= foo 100)
;;    (progn (incf foo) 
;;           (zap append result (list foo))))
;; ;=> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
;;  30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
;;  56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
;;  82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)

;; symbol-macroletで添字
;; http://cadr.g.hatena.ne.jp/g000001/20081001/1222849105
;; ;; 添字
;; (let ((foo '(1 2 3 4)))
;;   (symbol-macrolet ((foo[1] (nth 0 foo))
;;                     (foo[2] (nth 1 foo))
;;                     (foo[3] (nth 2 foo))
;;                     (foo[4] (nth 3 foo))
;;                     (foo[5] (nth 4 foo))
;;                     (foo[6] (nth 5 foo))
;;                     (foo[7] (nth 6 foo)))
;;     (list foo[2] foo[3] foo[1])))
;; ;=> (2 3 1)

;; ;; ハッシュ
;; (let ((ht (make-hash-table)))
;;   (setf (gethash :foo ht) 30)
;;   (symbol-macrolet ((ht[foo] (gethash :foo ht)))
;;     (setf ht[foo] 40)
;;     ht[foo]))
;; ;=> 40 , T

(defun-compile-time mappend (func seq)
  (apply #'append (mapcar func seq)))

;; with-l/ists
;; 無茶苦茶なアイデア
;; http://cadr.g.hatena.ne.jp/g000001/20081001/1222847841
(defun-compile-time symbol-car (sym)
  (intern (subseq (string sym) 0 1)))
(defun-compile-time symbol-cdr (sym)
  (intern (subseq (string sym) 1)))

(defmacro with-l/ists ((&rest lists) &body body)
  (let ((xx (mappend (lambda (x)
                         `((,(symbol-car x) (car ,x))
                           (,(symbol-cdr x) (cdr ,x))))
                       lists)))
    `(symbol-macrolet ,xx
       ,@body)))

;; (LET ((FOO '(1 2 3 4)))
;;   (WITH-L/ISTS (FOO)
;;     (LIST F OO)))
;=> (1 (2 3 4))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fn*
    '(car cdr rest first second third forth fifth sixth seventh eighth ninth tenth 
      reverse length null gensym 1+ 1-)))

(defmacro with-dot-concat ((&rest args) &body body)
  (let ((xx (mappend (lambda (y)
                         (mapcar (lambda (x)
                                   `(,(symb x "." y) (,x ,y))) *fn*))
                       args)))
    `(symbol-macrolet ,xx
       ,@body)))

(defmacro with-dot-concat-reverse ((&rest args) &body body)
  (let ((xx (mappend (lambda (y)
                         (mapcar (lambda (x)
                                   `(,(symb y "." x) (,x ,y))) *fn*))
                       args)))
    `(symbol-macrolet ,xx
       ,@body)))

;; (defun encode-direct (coll &aux (g "G"))
;;   (with-dot-concat (coll tem g acc cnt reverse.acc)
;;     (if null.coll
;;         ()
;;         (labels ((recur (coll tem acc)
;;                    (let ((cnt first.tem) (item second.tem))
;;                      (cond (null.coll cdr.reverse.acc)
;;                            ((eql car.coll item)
;;                             (recur cdr.coll (list 1+.cnt car.coll) acc))
;;                            (:else
;;                             (recur cdr.coll 
;;                                    `(1 ,car.coll)
;;                                    (cons (if (= 1 cnt)
;;                                              item
;;                                              tem)
;;                                          acc)))))))
;;           (recur `(,@coll ,gensym.g)
;;                  `(1 ,gensym.g)
;;                  () )))))

;; (ENCODE-DIRECT '(1 2 3 4 5 8 8))
;; ;=> (1 2 3 4 5 (2 8))

;; anaphoric destructuring-bind
;; http://cadr.g.hatena.ne.jp/g000001/20080930/1222765327
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *anaphoras*
    '(car cdr rest 
      first second third forth fifth sixth seventh eighth ninth tenth)))

(defmacro adestructuring-bind (list &body body)
  (let ((anaphoras (mapcar (lambda (x) `(,x (,x ,list))) *anaphoras*)))
    `(symbol-macrolet ,anaphoras
       ,@body)))

;; (let ((foo (list 1 2 3 4)))
;;   (adestructuring-bind foo
;;     (list (car foo)
;;           (funcall #'car cdr)
;;           cdr
;;           first
;;           second
;;           (setq car 'alt)
;;           foo)))
;=> (1 2 (2 3 4) 1 2 ALT (ALT 2 3 4))

;; tconc
;; http://cadr.g.hatena.ne.jp/g000001/20080611/1213160047
(defun TCONC (ptr x)
  (declare (list ptr))
  (let ((x (list x)))
    (if (null ptr)
        (cons x x)
        (progn (psetf (cddr ptr) x             
                      (cdr ptr) x)
               ptr))))

(defun LCONC (ptr x)
  (declare (cons ptr x))
  (let ((last (last x)))
    (rplaca ptr (nconc (car ptr) x))
    (rplacd ptr last)))

(defun ATTACH (x y)
  (declare (cons y))
  (let ((ptr y)
        (tail (cons (car y) (cdr y))))
    (setf (car ptr) x
          (cdr ptr) tail)
    ptr))

;; ;; tconcの動作
;; (loop :with start := 1 :and end := 10
;;       :with tc := (tconc () start)
;;       :for i :from (1+ start) :to end :do (tconc tc i) 
;;       :finally (return (car tc)))

;; ;==> (1 2 3 4 5 6 7 8 9 10)

;; ;; lconcの動作
;; (loop :with start := 1 :and end := 10
;;       :with lc := (lconc (list ()) (list start))
;;       :for i :from (1+ start) :to end :do (lconc lc (list i)) 
;;       :finally (return (car lc)))

;; ;==> (1 2 3 4 5 6 7 8 9 10)

;; ;; attachの動作
;; (setq foo (list 100))

;; (eq foo (attach 0 foo))
;; ;==> T

;; foo
;; ;==> (0 100)

;; generic-function
;;http://cadr.g.hatena.ne.jp/g000001/20080521/1211341393


;; (Gauche)isomorphic?
;; http://cadr.g.hatena.ne.jp/g000001/20080512/1210584057

;;(let ((p (cons 1 2)))
;;  (isomorphic? (list p p) (list p '(1 2))))
;;;=> nil
;;
;;(let ((p (cons 1 2)))
;;  (isomorphic? (list p p) (list p p)))
;;;=> t
;;
;;(let ((p (make-array 100 :fill-pointer 1 :adjustable 'T)))
;;  (vector-push 1 p)
;;  (isomorphic? p (vector 0 (+ 0 1))))
;;;=> t

(defun ISOMORPHIC? (a b &rest args)
  (let ((ctx (if (consp args)
                 (if (hash-table-p (car args))
                     (car args)
                     (error "hash table required, but got ~S." (car args)))
                 (make-hash-table))))
    (ISO? a b ctx)))

(defun ISO? (a b ctx)
  (let (win tem)
    (cond ((or (characterp a) (numberp a))
           (eql a b))
          ((null a) (null b))
          ((progn (setf (values tem win) (gethash a ctx))
                  win) ;node has been appeared
           (eq tem b))
          (:else
           (setf (gethash a ctx) b)
           (typecase a
             (cons (and (consp b)
                        (iso? (car a) (car b) ctx)
                        (iso? (cdr a) (cdr b) ctx)))
             (string (and (stringp b) (string= a b)))
             (keyword (eql a b))
             (symbol (eq a b))
             (vector (VECTOR-ISO? a b ctx))
             (otherwise (OBJECT-ISOMORPHIC? a b ctx)))))))

(progn
  (declaim (inline vector->list))
  (defun VECTOR->LIST (vec) (coerce vec 'list)))

(defun VECTOR-ISO? (a b ctx)
  (and (vectorp b)
       (do ((la (vector->list a) (cdr la))
            (lb (vector->list b) (cdr lb)))
           ((endp la) (endp lb))
         (cond ((endp lb) (return nil))
               ((ISO? (car la) (car lb) ctx))
               (:else (return nil))))))

(defmethod OBJECT-ISOMORPHIC? (a b context)
  (equal a b))


;; multiple-value-do
;; http://cadr.g.hatena.ne.jp/g000001/20080322/1206160599
(defmacro MULTIPLE-VALUE-DO ((&rest varlist) (test &rest finally) &body body)
  (let ((vars (mappend #'car varlist))
        (inits (mappend #'cadr varlist))
        (tag (gensym)))
    `(BLOCK NIL
       (MULTIPLE-VALUE-BIND ,vars ,inits
         (TAGBODY
            (MULTIPLE-VALUE-PSETQ ,@(mappend (fn ((x y z)) `(,x ,y))
                                             varlist))
       ,tag (WHEN ,test
              (RETURN-FROM NIL (PROGN ,@finally)))
            ,@body
            (MULTIPLE-VALUE-PSETQ ,@(mappend (fn ((x y z)) `(,x ,z))
                                             varlist))
            (GO ,tag))))))

(defmacro FN ((&rest args) &body body) ;; Arcから拝借
  (let ((g (gensym)))
    `(LAMBDA (&rest ,g)
       (DESTRUCTURING-BIND ,args ,g
         (DECLARE (IGNORABLE ,@(flatten args)))
         ,@body))))

(defmacro MULTIPLE-VALUE-PSETQ (&rest pairs)
  (cond ((cddr pairs) `(SETF (VALUES ,@(car pairs))
                             (MULTIPLE-VALUE-PROG1 ,(cadr pairs)
                               (MULTIPLE-VALUE-PSETQ ,@(cddr pairs)))))
        ((cdr pairs) `(SETF (VALUES ,@(car pairs)) ,@(cdr pairs)))
        ('T (error "Odd number of args."))))

;; onep
;; http://cadr.g.hatena.ne.jp/g000001/20080301/1204336099
(DEFUN ONEP (X)
  (= 1 X))

;; wget
;; http://cadr.g.hatena.ne.jp/g000001/20080224/1203797398
(defun wget (uri &optional (dir "./"))
  (let* ((file-name (aref (nth-value 1 (ppcre:scan-to-strings ".*/([^/]*)$" uri)) 0))
	 (out-file (concatenate 'string dir file-name)))
    (format t "~A ==> ~A~%" uri out-file)
    (with-open-file (out out-file
			 :direction :output 
			 :if-exists :supersede
			 :element-type 'unsigned-byte)
      (with-open-stream (str (drakma:http-request uri :want-stream 'T))
	(do ((s (read-byte str nil -1) (read-byte str nil -1))
	     (cnt 0 (1+ cnt)))
	    ((= -1 s) (format t "end ~A.~%" cnt))
	  (write-byte s out)
	  (when (and (zerop (rem cnt 1024)) (not (zerop cnt)))
	    (princ ".")
	    (when (zerop (rem cnt (* 100 1024)))
	      (format t "~A~%" cnt))))))))

;; 2008-02-03