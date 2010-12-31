;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package :cl-user)

(asdf:defsystem :shibuya.lisp
  :name "shibuya.lisp"
  :depends-on (:cl-ppcre :drakma :trivial-utf-8)
  :components ((:file "packages")
               (:file "shibuya" :depends-on ("packages"))))
