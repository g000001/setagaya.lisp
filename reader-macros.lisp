;; パイプ記法
;; http://cadr.g.hatena.ne.jp/g000001/20081125/1227612201
(defun pipeop (q)
  (labels ((pipeop-n (expr rest)
             (let ((expr (read-from-string expr)))
               `(,@(if (numberp expr)
                       `(nth ,(1- expr))
                       (list expr))
                   ,@(if rest (list rest) rest))))
           (recur (q acc)
             (let* ((cmds (string q))
                    (pos (position #\/ cmds)))
               (if pos
                   (recur (subseq cmds (1+ pos))
                          (pipeop-n (subseq cmds 0 pos) acc))
                   (pipeop-n cmds acc)))))
    (recur q () )))

(set-dispatch-macro-character #\# #\/
                              (lambda (str char arg)
                                (declare (ignore char arg))
                                (pipeop (read str nil nil nil))))

;; 動作
;; (progn
;;   #/person/car/father/name/last/1)
;; 展開 => (PROGN (NTH 0 (LAST (NAME (FATHER (CAR (PERSON)))))))

;; R6RS風 [foo bar]
(defun open-bracket-macro-char (stream macro-char)
  (declare (ignore macro-char))
  (read-delimited-list #\] stream t))
(set-macro-character #\[ #'open-bracket-macro-char)
(set-macro-character #\] (get-macro-character #\)))

;; 使用例
;; (defun my-find-if (pred lst)
;;   (cond [(null lst) nil]
;;         [(funcall pred (car lst)) (car lst)]
;;         [:else (my-find-if pred (cdr lst))]))


;; Arc風 [* 3 _]
;; http://cadr.g.hatena.ne.jp/g000001/20080203/1201971701
(set-macro-character #\[ 
		     (lambda (stream char)
		       (declare (ignore char))
		       (let ((body (read-delimited-list #\] stream t)))
			 `#'(lambda (,(intern "_")) ,body))))

(set-macro-character #\] (get-macro-character #\)))

;; 動作
;; (mapcar [* 3 _] '(1 2 3 4))
;=> (3 6 9 12)


;; hashのアクセスを {}で書く
;; http://cadr.g.hatena.ne.jp/g000001/20080203/1201971701
(set-macro-character #\} (get-macro-character #\)))

(set-macro-character #\{
		     (lambda (str char)
		       (declare (ignore char))
		       (let ((body (read-delimited-list #\} str t)))
			 (cond ((eq '=> (cadr body))                 ; (hash => key)
				`(obcall ,(car body) ',(caddr body)))
			       ((some #'sym.bol-p body)              ; hash.key
				(mapcar (lambda (x)
					  (if (sym.bol-p x)
					      (hash.key->gethash x)
					      x))
					body))
			       ('T `(obcall ,@body))))))

(defun sym.bol-p (sym)
  (and (symbolp sym)
       (position #\. (string sym)) t))
      
(defun obcall (obj arg)
  (etypecase obj
    (hash-table (gethash arg obj))
    (sequence (elt obj arg))))

(defun hash.key->gethash (sym)
  (let* ((str (string sym))
	 (sep (position #\. str)))
    (flet ((intern-upcase (str) (intern (string-upcase str))))
      `(gethash ',(intern-upcase (subseq str (1+ sep)))
		,(intern-upcase (subseq str 0 sep))))))
