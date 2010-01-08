(load "triangle.lisp")

(defconstant MAX_TRIANGLES #16r100000)
(defconstant MAX_EMITTERS_P 16)

(defclass Scene ()
  ((triangles
     :initarg :triangles
     :initform '()
     :accessor triangles)
   (emitters
     :initarg :emitters
     :iniform '()
     :accessor emitters)
   ; FALTA SPATIALINDEX
   (sky
     :initarg :sky
     :initform (make-vector3f '(0.0 0.0 0.0)))
   (ground
     :initarg :ground
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor ground)))

(defgeneric make-scene (triangles emitters sky ground)
	    (:documentation "..."))

(defgeneric intersects (scene rayOrigin rayDirection lastHit pHitObject hitPosition)
	    (:documentation "..."))

(defgeneric emitter (scene position id)
	    (:documentation "..."))

(defgeneric defaultEmission (backDirection)
	    (:documentation "..."))
