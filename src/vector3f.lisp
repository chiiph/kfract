(defpackage Vector3f)

(in-package :Vector3f)

(defclass Vector3f ()
  ((v1
     :initarg :v1
     :initform 0.0
     :accessor v1)
   (v2
     :initarg :v2
     :initform 0.0
     :accessor v2)
   (v3
     :initarg :v3
     :initform 0.0
     :accessor v3)))

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

(defmethod dot ((vec1 Vector3f) (vec2 Vector3f))
  (+ (* (v1 vec1) (v1 vec2)) (* (v2 vec1) (v2 vec2)) (* (v3 vec1) (v3 vec2))))

(defmethod cross ((vec1 Vector3f) (vec2 Vector3f))
  (let ((p1 (- (* (v2 vec1) (v3 vec2)) (* (v3 vec1) (v2 vec2))))
	(p2 (- (* (v3 vec1) (v1 vec2)) (* (v1 vec1) (v3 vec2))))
	(p3 (- (* (v1 vec1) (v2 vec2)) (* (v2 vec1) (v1 vec2)))))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod sub ((vec1 Vector3f) (vec2 Vector3f))
  (let ((p1 (- (v1 vec1) (v1 vec2)))
	(p2 (- (v2 vec1) (v2 vec2)))
	(p3 (- (v3 vec1) (v3 vec2))))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod plus ((vec1 Vector3f) (vec2 Vector3f))
  (let ((p1 (+ (v1 vec1) (v1 vec2)))
	(p2 (+ (v2 vec1) (v2 vec2)))
	(p3 (+ (v3 vec1) (v3 vec2))))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod mul ((vec1 Vector3f) (vec2 Vector3f))
  (let ((p1 (* (v1 vec1) (v1 vec2)))
	(p2 (* (v2 vec1) (v2 vec2)))
	(p3 (* (v3 vec1) (v3 vec2))))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod mul ((vec1 Vector3f) f)
  (let ((p1 (* (v1 vec1) f))
	(p2 (* (v2 vec1) f))
	(p3 (* (v3 vec1) f)))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod div ((vec1 Vector3f) f)
  (if (or (not (numberp f)) (= f 0.0))
    (error "Dividing by ZERO or by not a number"))
  (let ((p1 (/ (v1 vec1) f))
	(p2 (/ (v2 vec1) f))
	(p3 (/ (v3 vec1) f)))
    (make-instance 'Vector3f :v1 p1 :v2 p2 :v3 p3)))

(defmethod unitize ((vec Vector3f))
  (let* ((l (sqrt (+ (expt (v1 vec) 2) (expt (v2 vec)) (expt (v3 vec)))))
	 (oneL (if (zerop l) 0.0 (/ 1.0 l))))
    (make-instance 'Vector3f 
		   :v1 (* (v1 vec) oneL) 
		   :v2 (* (v2 vec) oneL)
		   :v3 (* (v3 vec) oneL))))

(defmethod isZero ((vec Vector3f))
  (and (zerop (v1 vec)) (zerop (v2 vec)) (zerop (v3 vec))))

;:::: TODO: encontrar alguna forma razonable de implementar esto ::::;
;(defmethod clamp ((vec Vector3f) (min Vector3f) (max Vector3f))
;  (do ((v '((v1 vec) (v2 vec) (v3 vec)) (rest v))
;       (mi '((v1 min) (v2 min) (v3 min)) (rest mi))
;       (ma '((v1 max) (v2 max) (v3 max)) (rest ma)))
;      ((or (null v) (null mi) (null ma)) nil)
;    (if (< v mi) (setf v mi))
;    (if (> v ma) (setf v ma))))

(defmethod neg ((vec Vector3f))
  (make-instance 'Vector3f :v1 (- (v1 vec)) :v2 (- (v2 vec)) :v3 (- (v3 vec))))

;:::: TESTS ::::;

;(setf test1 (make-instance 'Vector3f :v1 2 :v2 3 :v3 4))
;(setf test2 (make-instance 'Vector3f :v1 1 :v2 2 :v3 3))

;(format t "The dot product is ~S" (dot test1 test2))
;(describe (cross test1 test2))
;(describe (div test1 "a"))
;(describe (mul test1 2))
