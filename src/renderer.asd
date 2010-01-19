;;;; renderer.asd
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem renderer
  :version "0"
  :description "Minilight port to Lisp"
  :licence "BSD-style"
  :serial t ; Serial dependency
  :components ((:file "package")
               (:file "vector3f")
               (:file "polygon" :depends-on ("vector3f"))
	       (:file "triangle" :depends-on ("vector3f" "polygon"))
	       (:file "image" :depends-on ("vector3f"))
	       (:file "surfacepoint" :depends-on ("triangle"))))
