;;;; package.lisp
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:common-lisp-user)

(defpackage #:vector3f
    (:use #:cl)
    (:export #:vector3f
	     #:make-vector3f
	     #:dot
	     #:unitize
	     #:cross
	     #:plus
	     #:mul
	     #:sub
	     #:getv
	     #:div
	     #:neg
	     #:eq-vector3f))

(defpackage #:polygon
    (:use #:cl
	  #:vector3f)
    (:export #:polygon))

(defpackage #:triangle
    (:use #:cl
	  #:vector3f)
    (:export #:triangle
	     #:make-triangle
	     #:eq-triangle
	     #:tangent
	     #:normal
	     #:area
	     #:intersects
	     #:samplePoint
	     #:bound))

(defpackage #:surfacepoint
  (:use #:cl
	#:vector3f
	#:polygon
	#:triangle)
  (:export #:surfacepoint
	   #:make-surfacepoint
	   #:emission
	   #:reflection
	   #:nextDirection))

(defpackage #:scene
  (:use #:cl
	#:vector3f
	#:polygon
	#:triangle))

(defpackage #:image
  (:use #:cl
	#:vector3f))

(defpackage #:raytracer
    (:use #:cl))
