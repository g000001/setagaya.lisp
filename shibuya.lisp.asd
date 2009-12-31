;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package :cl-user)

(asdf:defsystem :shibuya.lisp
  :name "shibuya.lisp"
  :depends-on (:cl-ppcre :drakma)
  :components ((:file "shibuya")))
