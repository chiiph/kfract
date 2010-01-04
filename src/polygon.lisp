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

;:::: Como getv/setv generics estan definidos en Vector3f, no hay que redefinirlos
;:::: para usarlos en Polygon. TODO: Ver desde el pto de vista de la OOP como queda
;:::: mejor definir estos generics, tal vez hacer un package que sea 3DObjs y ahi
;:::: definir getv/setv generics, y que despues vector3f y los polys esten dentro 
;:::: de ese package

;(defgeneric getv (poly i)
;            (:documentation "Returns the vertex i from vec"))

;(defgeneric setv (poly i val)
;            (:documentation "Sets val to the vertex i from vec"))

;:::: TODO: descifrar e implementar ::::;
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
