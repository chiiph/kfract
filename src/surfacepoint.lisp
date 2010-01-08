(load "triangle.lisp")

(defclass SurfacePoint ()
  ((triangle
     :initarg triangle
     :initform (make-triangle 
		 '(0.0 0.0 0.0)
		 '(0.0 0.0 0.0)
		 '(0.0 0.0 0.0))
     :accessor triangle)
   (position
     :initarg pos
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor pos)))

(defgeneric make-surfacepoint (tri pos)
	    (:documentation "Returns an instance of SurfacePoint"))

(defgeneric emission (surf toPosition outDirection isSolidAngle)
	    (:documentation "..."))

(defgeneric reflection (surf inDirection inRadiance outDirection)
	    (:documentation "..."))

(defgeneric nextDirection (surf inDirection outDirection color)
	    (:documentation "..."))

;:::: IMPLEMENTATION ::::;

;:::: CONSTRUCTOR    ::::;

(defmethod make-surfacepoint ((tri Triangle) (pos Vector3f))
  (make-instance 'SurfacePoint :triangle tri :pos pos))

;:::: THE REST ::::;

(defmethod emission ((surf SurfacePoint)
		     (toPosition   Vector3f)
		     (outDirection Vector3f)
		     isSolidAngle)
  (let* ((ray (sub toPosition (pos surf)))
	 (distance2 (dot ray ray))
	 (tri (triangle surf))
	 (cosArea (* (dor outDirection (normal tri)) (area tri)))
	 (solidAngle (if isSolidAngle
		       (/ cosArea (if (>= distance2 1e-6)
				    distance2
				    1.0))
		       1.0))
	 (res (mul (emitivity tri) solidAngle)))
    (if (> cosArea 0.0)
      (make-vector3f '(res res res))
      (make-vector3f '(0.0 0.0 0.0)))))

(defmethod reflection ((surf         SurfacePoint)
		       (inDirection  Vector3f)
		       (inRadiance   Vector3f)
		       (outDirection Vector3f))
  (let* ((inDot  (dot inDirection (normal (triangle surf))))
	 (outDot (dot outDirection (normal (triangle surf))))
	 (res    (* (mul inRadiance (reflectivity (triangle surf)))
		    (/ (abs inDot) pi))))
    (if (xor (< inDot 0.0) (< outDot 0.0))
      (make-vector3f '(0.0 0.0 0.0))
      (make-vector3f '(res res res)))))

; Returns outDirection color
; Call this with (multiple-value-setq (outDirection color) (nextDirection))
; Check for isZero when calling
(defmethod nextDirection ((surf         SurfacePoint)
			  (inDirection  Vector3f))
  (let* ((reflect (reflectivity (triangle surf)))
	 (vec1 (make-vector3f '(1.0 1.0 1.0)))
	 (reflectivityMean (/ (dot reflect vec1) 3.0)))
    (if (< (random 1.0) reflectivityMean)
      (let* ((color (div reflect reflectivityMean))
	     (outDirection (make-vector3f '(0.0 0.0 0.0)))
	     (_2pr1 (* pi 2.0 (random 1.0)))
	     (sr2 (sqrt (random 1.0)))
	     (x (* (cos _2pr1) sr2))
	     (y (* (sin _2pr1) sr2))
	     (z (sqrt (- 1.0 (* sr2 sr2))))
	     (norm (normal (triangle surf)))
	     (normal (if (>= (dot norm inDirection) 0.0)
		       norm
		       (neg norm)))
	     (tangent (tangent (triangle surf)))
	     (outDirection (plus (mul tangent x)
				 (plus (mul (cross normal tangent) y)
				       (mul normal z)))))
	(values outDirection color)))))
