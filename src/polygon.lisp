(load "vector3f.lisp")

(defconstant tolerance (/ 1.0 1024.0))

(defclass Polygon ()
  ((reflectivity
     :initarg :ref
     :initform (make-instance 'Vector3f)
     :accessor reflectivity)
   (emitivity
     :initarg :emit
     :initform (make-instance 'Vector3f)
     :accessor emitivity)))

;:::: TODO: descifrar ::::;
(defgeneric bound ()
	    (:documentation "..."))

(defgeneric intersects (poly rayOrigin rayDirectiion)
	    (:documentation "Returns the hit distance in case there's any intersection"))

(defgeneric samplePoint (poly)
	    (:documentation "Returns a random point inside poly"))

(defgeneric normal (poly)
	    (:documentation "Returns poly's normal"))

(defgeneric tangent (poly)
	    (:documentation "Returns poly's tangent"))

(defgeneric area (poly)
	    (:documentation "Returns poly's area"))
