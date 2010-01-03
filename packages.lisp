(in-package :cl-user)

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
           :mapleave
           :mapstop
           :mapret
           :mapf
           :with-l4u
           :with-lisp1
           :!
           :with-l/ists
           :with-dot-concat
           :with-dot-concat-reverse
           :adestructuring-bind
           :tconc
           :lconc
           :attach
           :isomorphic?
           :iso?
           :vector->list
           :vector-iso?
           :object-isomorphic?
           :multiple-value-do
           :fn
           :multiple-value-psetq
           :onep
           :wget
           :zap
           :update-alist
           :plist-alist
           :update-plist
           :tail-recursive-defun
           :let-nreverse
           :file-extract-defs
           :group
           :subst*
           :do-template
           :$
           :$*))