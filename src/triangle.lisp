(load "polygon.lisp")

(defconstant EPSILON 0.000001)

(defclass Triangle (Polygon)
  ((verts
     :initarg :verts
     :initform (make-array 3 :initial-element (make-instance 'Vector3f))
     :accessor verts)))

(defgeneric make-triangle (vec1 vec2 vec3)
	    (:documentation "Creates an instance of Triangle class with vecN as its verts"))

;:::: IMPLEMENTATION ::::;

;:::: CONSTRUCTORS   ::::;

(defmethod make-triangle (vec1 vec2 vec3)
  (make-instance 'Triangle :verts 
			 (make-array 3 :initial-contents 
				     (list (make-vector3f vec1)
					   (make-vector3f vec2)
					   (make-vector3f vec3)))))

;:::: GET/SET FUNCS  ::::;

(defmethod getv ((tri Triangle) i)
  (elt (verts tri) i))

(defmethod setv ((tri Triangle) i val)
  (setf (elt (verts tri) i) val))

;:::: THE REST ::::;

(defmethod tangent ((tri Triangle))
  (unitize (sub (getv tri 2) (getv tri 1))))

(defmethod normal ((tri Triangle))
  (unitize (cross (tangent tri (sub (getv tri 3) (getv tri 2))))))

(defmethod area ((tri Triangle))
  (let* ((res1 (sub (getv tri 2) (getv tri 1)))
	 (res2 (sub (getv tri 3) (getv tri 2)))
	 (pa2  (cross res1 res2)))
    (* (sqrt (dot pa2 pa2)) 0.5)))

(defmethod intersects ((tri Triangle) (rayOrigin Vector3f) (rayDirection Vector3f))
  (let* ((edge1 (sub (getv tri 2) (getv tri 1)))
	 (edge2 (sub (getv tri 3) (getv tri 1)))
	 (pvec  (cross rayDirection edge2))
	 (det   (dot edge1 pvec)))
    (if (and (> det (- EPSILON)) (< det EPSILON))
      (return-from intersects -1.0))
    (let* ((inv_det (/ 1.0 det))
	   (tvec    (sub rayOrigin (getv tri 1)))
	   (u       (* (dot tvec pvec) inv_det)))
      (if (or (< u 0.0) (> u 1.0))
	(return-from intersects -1.0))
      (let* ((qvec (cross tvec edge1))
	     (v    (* (dot rayDirection qvec) inv_det)))
	(if (or (< v 0.0) (> (+ u v) 1.0))
	  (return-from intersects -1.0))
	(* (dot edge2 qvec) inv_det))))) ; Returns hitDistance

(defmethod samplePoint ((tri Triangle))
  (let* ((sqr1 (sqrt (random 1.0)))
	 (r2   (random 1.0))
	 (a    (- 1.0 sqr1))
	 (b    (* (- 1.0 r2) sqr1))
	 (point (plus (mul (sub (getv tri 2) (getv tri 1)) a)
		      (mul (sub (getv tri 3) (getv tri 1)) b)
		      (getv tri 1))))
    (make-instance 'Vector3f 
		   :verts (make-array 3 :initial-element point))))

(setf tri (make-triangle (list 1.0 2.0 3.0) (list 4.0 5.0 6.0) (list 7.0 8.0 9.0)))

;(setf vec1 (make-array 3 :initial-contents (list 1.0 2.0 3.0)))
;(setf vec2 (make-array 3 :initial-contents (list 4.0 5.0 6.0)))
;(setf vec3 (make-array 3 :initial-contents (list 7.0 8.0 9.0)))
;(setf tri (make-instance 'Triangle :verts 
;                         (make-array 3 :initial-contents 
;                                     (list (make-instance 'Vector3f :verts vec1)
;                                           (make-instance 'Vector3f :verts vec2)
;                                           (make-instance 'Vector3f :verts vec3)))))

(describe (getv tri 2))
;(describe vec1)
;(describe (samplePoint t))
