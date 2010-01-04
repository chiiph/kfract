(defclass Vector3f ()
  ((verts
     :initarg :verts
     :initform #(0.0 0.0 0.0)
     :accessor verts)
   ))

(defgeneric make-vector3f (vec)
	    (:documentation "Creates an instance of Vector3f class with vec as its verts"))

(defgeneric getv (vec i)
	    (:documentation "Returns the vertex i from vec"))

(defgeneric setv (vec i val)
	    (:documentation "Sets val to the vertex i from vec"))

(defgeneric dot (vec1 vec2)
	    (:documentation "Returns the dot product between vec1 and vec2"))

(defgeneric cross (vec1 vec2)
	    (:documentation "Returns the cross product between vec1 and vec2"))

(defgeneric sub (vec1 vec2)
	    (:documentation "Returns the substraction of vec2 to vec1"))

(defgeneric plus (vec1 vec2)
	    (:documentation "Returns the addition of vec2 to vec1"))

; mul is implemented for scalars too
(defgeneric mul (vec1 vec2)
	    (:documentation "Returns the multiplication of vec2 times vec1"))

(defgeneric div (vec1 f)
	    (:documentation "Returns the division of vec2 to vec1"))

(defgeneric unitize (vec)
	    (:documentation "Returns ... I know what it returns, but I don't know how to interpret it"))

(defgeneric isZero (vec)
	    (:documentation "True if vec is zero"))

(defgeneric clamp (vec min max)
	    (:documentation "Clamps vec between min and max"))

(defgeneric neg (vec)
	    (:documentation "Returns the vector with all its components in the negative form"))

;:::: IMPLEMENTATION ::::;

;:::: CONSTRUCTOR    ::::;

(defmethod make-vector3f (vec)
  (make-instance 'Vector3f :verts (make-array 3 :initial-contents vec)))

;:::: GET/SET FUNCS  ::::;

(defmethod getv ((vec Vector3f) i)
  (elt (verts vec) i))

(defmethod setv ((vec Vector3f) i val)
  (setf (elt (verts vec) i) val))

;:::: THE REST ::::;

(defmethod dot ((vec1 Vector3f) (vec2 Vector3f))
  (let ((sum 0.0))
    (dotimes (i 3)
      (incf sum (* (getv vec1 i) (getv vec2 i))))
    (return-from dot sum)))

(defmethod cross ((vec1 Vector3f) (vec2 Vector3f))
  (let ((p1 (- (* (getv vec1 1) (getv vec2 2)) (* (getv vec1 2) (getv vec2 1))))
	(p2 (- (* (getv vec1 2) (getv vec2 0)) (* (getv vec1 0) (getv vec2 2))))
	(p3 (- (* (getv vec1 0) (getv vec2 1)) (* (getv vec1 1) (getv vec2 0)))))
    (make-instance 'Vector3f :verts (make-array 3 :initial-contents (list p1 p2 p3)))))

(defmethod sub ((vec1 Vector3f) (vec2 Vector3f))
  (let ((ar (make-array 3)))
    (dotimes (i 3)
      (setf (elt ar i) (- (getv vec1 i) (getv vec2 i))))
    (make-instance 'Vector3f :verts ar)))

(defmethod plus ((vec1 vector3f) (vec2 vector3f))
  (let ((ar (make-array 3)))
    (dotimes (i 3)
      (setf (elt ar i) (+ (getv vec1 i) (getv vec2 i))))
    (make-instance 'Vector3f :verts ar)))

(defmethod mul ((vec1 vector3f) (vec2 vector3f))
  (let ((ar (make-array 3)))
    (dotimes (i 3)
      (setf (elt ar i) (* (getv vec1 i) (getv vec2 i))))
    (make-instance 'Vector3f :verts ar)))

(defmethod mul ((vec1 vector3f) f)
  (let ((ar (make-array 3)))
    (dotimes (i 3)
      (setf (elt ar i) (* (getv vec1 i) f)))
    (make-instance 'Vector3f :verts ar)))

(defmethod div ((vec1 vector3f) f)
  (let ((ar (make-array 3)))
    (dotimes (i 3)
      (setf (elt ar i) (/ (getv vec1 i) f)))
    (make-instance 'Vector3f :verts ar)))

(defmethod unitize ((vec Vector3f))
  (let* ((l    (sqrt (+ (expt (getv vec 0) 2) (expt (getv vec 1) 2) (expt (getv vec 2) 2))))
	 (oneL (if (zerop l) 0.0 (/ 1.0 l))))
    (make-instance 'Vector3f :verts (make-array 3 :initial-contents (list (* (getv vec 0) oneL) (* (getv vec 1) oneL) (* (getv vec 2) oneL))))))

(defmethod isZero ((vec Vector3f))
  (and (zerop (getv vec 0)) (zerop (getv vec 1)) (zerop (getv vec 2))))

(defmethod clamp ((vec Vector3f) (min Vector3f) (max Vector3f))
  (loop
    for v  across (verts vec)
    for mi across (verts min)
    for ma across (verts max)
    for i from 0 to 2 collecting i
    if (< v mi) do (setv vec i mi)
    if (> v ma) do (setv vec i ma)))

(defmethod neg ((vec Vector3f))
  (make-instance 'Vector3f :verts 
		 (make-array 3 :initial-contents (list
						   (- (getv vec 0))
						   (- (getv vec 1))
						   (- (getv vec 2))))))

;:::: TESTS ::::;

;(setf test1 (make-instance 'Vector3f :verts #(1.0 2.0 3.0)))
;(setf test2 (make-instance 'Vector3f :verts #(0.5 5.0 6.0)))
;(setf max (make-instance 'Vector3f :verts #(3.2 5.0 4.9)))

;(format t "The dot product is ~S" (dot test1 test2))
;(describe (cross test1 test2))
;(print (elt (verts (cross test1 test2)))
;(describe (unitize test1))
;(clamp test2 test1 max)
;(describe test2)
;(describe (div test1 "a"))
;(describe (mul test1 2))
;(describe (neg test2))
