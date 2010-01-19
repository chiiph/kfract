;;;; triangle.lisp
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:triangle)

(defconstant EPSILON 0.000001d0)

(defclass triangle (polygon)
  ((verts
     :initarg :verts
     :initform (make-array 3 :initial-element (make-instance 'vector3f))
     :accessor verts)))

(defgeneric make-triangle (vec1 vec2 vec3)
	    (:documentation "Creates an instance of triangle class with vecN as its verts"))

(defgeneric eq-triangle (tri1 tri2)
	    (:documentation "Returns T if tri1 and tri2 are the same"))

;:::: IMPLEMENTATION ::::;

;:::: CONSTRUCTORS   ::::;

(defmethod make-triangle (vec1 vec2 vec3)
  (make-instance 'triangle :verts 
			 (make-array 3 :initial-contents 
				     (list (make-vector3f vec1)
					   (make-vector3f vec2)
					   (make-vector3f vec3)))))

;:::: GET/SET FUNCS  ::::;

(defmethod getv ((tri triangle) i)
  (elt (verts tri) i))

(defmethod setv ((tri triangle) i val)
  (setf (elt (verts tri) i) val))

;:::: THE REST ::::;

(defmethod tangent ((tri triangle))
  (unitize (sub (getv tri 1) (getv tri 0))))

(defmethod normal ((tri triangle))
  (unitize (cross (tangent tri) (sub (getv tri 2) (getv tri 1)))))

(defmethod area ((tri triangle))
  (let* ((res1 (sub (getv tri 1) (getv tri 0)))
	 (res2 (sub (getv tri 2) (getv tri 1)))
	 (pa2  (cross res1 res2)))
    (* (sqrt (dot pa2 pa2)) 0.5d0)))

(defmethod intersects ((tri triangle) (rayOrigin vector3f) (rayDirection vector3f))
  (let* ((edge1 (sub (getv tri 1) (getv tri 0)))
	 (edge2 (sub (getv tri 2) (getv tri 0)))
	 (pvec  (cross rayDirection edge2))
	 (det   (dot edge1 pvec)))
    (if (and (> det (- EPSILON)) (< det EPSILON))
      (return-from intersects -1.0))
    (let* ((inv_det (/ 1.0d0 det))
	   (tvec    (sub rayOrigin (getv tri 0)))
	   (u       (* (dot tvec pvec) inv_det)))
      (if (or (< u 0.0) (> u 1.0))
	(return-from intersects -1.0))
      (let* ((qvec (cross tvec edge1))
	     (v    (* (dot rayDirection qvec) inv_det)))
	(if (or (< v 0.0) (> (+ u v) 1.0))
	  (return-from intersects -1.0)) ; Returns -1.0 if it doesn't intersect
	(* (dot edge2 qvec) inv_det))))) ; Returns hitDistance

(defmethod samplePoint ((tri triangle))
  (let* ((sqr1 (sqrt (random 1.0d0)))
	 (r2   (random 1.0d0))
	 (a    (- 1.0d0 sqr1))
	 (b    (* (- 1.0d0 r2) sqr1)))
    (plus (mul (sub (getv tri 1) (getv tri 0)) a)
	  (plus (mul (sub (getv tri 2) (getv tri 0)) b)
		(getv tri 0)))))

(defmethod bound ((bound list))
  )

(defmethod eq-triangle ((tri1 triangle) (tri2 triangle))
  (and (eq-vector3f (getv tri1 0) (getv tri2 0))
       (eq-vector3f (getv tri1 1) (getv tri2 1))
       (eq-vector3f (getv tri1 2) (getv tri2 2))))
