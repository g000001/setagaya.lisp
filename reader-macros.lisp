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
