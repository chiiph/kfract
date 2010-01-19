;;;; surfacepoint.lisp
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:surfacepoint)

(defclass SurfacePoint ()
  ((triangle
     :initarg :triangle
     :initform (make-triangle 
		 '(0.0 0.0 0.0)
		 '(0.0 0.0 0.0)
		 '(0.0 0.0 0.0))
     :accessor triangle)
   (pos
     :initarg :pos
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor pos)))

(defgeneric make-surfacepoint (tri pos)
	    (:documentation "Returns an instance of SurfacePoint"))

(defgeneric emission (surf toPosition outDirection isSolidAngle)
	    (:documentation "..."))

(defgeneric reflection (surf inDirection inRadiance outDirection)
	    (:documentation "..."))

(defgeneric nextDirection (surf inDirection)
	    (:documentation "..."))

;:::: IMPLEMENTATION ::::;

;:::: CONSTRUCTOR    ::::;

(defmethod make-surfacepoint ((tri Triangle) (pos Vector3f))
  (make-instance 'SurfacePoint :triangle tri :pos pos))

;:::: THE REST ::::;

(defmethod emission ((surf         SurfacePoint)
		     (toPosition   Vector3f)
		     (outDirection Vector3f)
		     isSolidAngle)
  (let* ((ray        (sub toPosition (pos surf)))
	 (distance2  (dot ray ray))
	 (tri        (triangle surf))
	 (cosArea    (* (dot outDirection (normal tri)) (area tri)))
	 (solidAngle (if isSolidAngle
		       (/ cosArea (if (>= distance2 1e-6)
				    distance2
				    1.0d0))
		       1.0d0)))
    (if (> cosArea 0.0)
      (mul (emitivity tri) solidAngle)
      (make-vector3f '(0.0 0.0 0.0)))))

(defmethod reflection ((surf         SurfacePoint)
		       (inDirection  Vector3f)
		       (inRadiance   Vector3f)
		       (outDirection Vector3f))
  (let* ((inDot  (dot inDirection (normal (triangle surf))))
	 (outDot (dot outDirection (normal (triangle surf)))))
    (if (xor (< inDot 0.0) (< outDot 0.0))
      (make-vector3f '(0.0 0.0 0.0))
      (mul (mul inRadiance (reflectivity (triangle surf)))
	   (/ (abs inDot) pi)))))

; Returns outDirection color
; Call this with (multiple-value-setq (outDirection color) (nextDirection))
; Check for isZero when calling
(defmethod nextDirection ((surf         SurfacePoint)
			  (inDirection  Vector3f))
  (let* ((reflect          (reflectivity (triangle surf)))
	 (vec1             (make-vector3f '(1.0 1.0 1.0)))
	 (reflectivityMean (/ (dot reflect vec1) 3.0d0)))
    (if (< (random 1.0d0) reflectivityMean)
      (let* ((color        (div reflect reflectivityMean))
	     (outDirection (make-vector3f '(0.0 0.0 0.0)))
	     (_2pr1        (float (* pi 2.0d0 (random 1.0d0)) 0.0d0))
	     (sr2          (float (sqrt (random 1.0d0)) 0.0d0))
	     (x            (float (* (cos _2pr1) sr2) 0.0d0))
	     (y            (float (* (sin _2pr1) sr2) 0.0d0))
	     (z            (float (sqrt (- 1.0d0 (* sr2 sr2))) 0.0d0))
	     (norm         (normal (triangle surf)))
	     (normal       (if (>= (dot norm inDirection) 0.0)
			     norm
			     (neg norm)))
	     (tangent      (tangent (triangle surf)))
	     (outDirection (plus (mul tangent x)
				 (plus (mul (cross normal tangent) y)
				       (mul normal z)))))
	(values outDirection color))
      (values (make-vector3f '(0.0 0.0 0.0)) (make-vector3f '(0.0 0.0 0.0))))))
