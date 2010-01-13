(load "polygon.lisp")

(defconstant EPSILON 0.000001d0)

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
  (unitize (sub (getv tri 1) (getv tri 0))))

(defmethod normal ((tri Triangle))
  (unitize (cross (tangent tri) (sub (getv tri 2) (getv tri 1)))))

(defmethod area ((tri Triangle))
  (let* ((res1 (sub (getv tri 1) (getv tri 0)))
	 (res2 (sub (getv tri 2) (getv tri 1)))
	 (pa2  (cross res1 res2)))
    (* (sqrt (dot pa2 pa2)) 0.5d0)))

(defmethod intersects ((tri Triangle) (rayOrigin Vector3f) (rayDirection Vector3f))
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

(defmethod samplePoint ((tri Triangle))
  (let* ((sqr1 (sqrt (random 1.0d0)))
	 (r2   (random 1.0d0))
	 (a    (- 1.0d0 sqr1))
	 (b    (* (- 1.0d0 r2) sqr1)))
    (plus (mul (sub (getv tri 1) (getv tri 0)) a)
	  (plus (mul (sub (getv tri 2) (getv tri 0)) b)
		(getv tri 0)))))

(defmethod bound ((bound list))
  )
