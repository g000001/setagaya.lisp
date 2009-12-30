;; elisp -> cl -> elispの中途半端な連携
;; http://cadr.g.hatena.ne.jp/g000001/20081031/1225438532
(require 'cl)

(defun has-package-name-p (symbol)
  (and (position ?: (symbol-name symbol)) 'T))

(defun put-package-name (tree &optional pkg)
  (let ((pkg (symbol-name (or pkg 'cl-user))))
    (*put-package-name tree pkg)))

(defun *put-package-name (tree pkg)
  (cond ((null tree) () )
        ((consp (car tree))
         (cons (*put-package-name (car tree) pkg)
               (*put-package-name (cdr tree) pkg)))
        ((and (symbolp (car tree))
              (not (has-package-name-p (car tree))))
         (cons (intern (concat pkg "::" (symbol-name (car tree))))
               (*put-package-name (cdr tree) pkg)))
        ('T (cons (car tree)
                  (*put-package-name (cdr tree) pkg)))))

(defmacro cl-funcall (fn &rest expr)
  `(slime-eval 
    ',(put-package-name `(funcall ,fn ,@expr))))

;; 動作
;; (+ (cl-funcall #'+ 30 40) 30)
;; 100
;;
;; (cl-funcall #'ppcre:scan "良し" "これで良し")
;; => 3


;; elisp -> cl
;; http://cadr.g.hatena.ne.jp/g000001/20081027/1225052899

(defun slime-eval-mesg (string)
  "Eval STRING in Lisp; Echo any output."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lambda (result)
                      (destructuring-bind (output value) result
                        (message (format "%s" value))))))

;; (slime-eval-mesg "(print 'hello)") =>HELLO

