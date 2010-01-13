(load "triangle.lisp")

(defconstant MAX_TRIANGLES #16r100000)
(defconstant MAX_EMITTERS_P 16)

(defclass Scene ()
  ((items
     :initarg :items
     :initform '()
     :accessor items)
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

(defgeneric defaultEmission (scene backDirection)
	    (:documentation "..."))

;#### Definicion provisoria de intersects ####;
(defmethod intersects ((scene        Scene)
		       (rayOrigin    Vector3f)
		       (rayDirection Vector3f)
		       (lastHit      Triangle)
		       (pHitObject   Triangle) ; <--- Aca devuelve el obj con el cual intersecta!
		       (hitPosition  Vector3f))
  (loop for item in (items scene)
	with hitd = (intersects item rayOrigin rayDirection)
	when (>= hitd 0)
	do (return-from intersects item)))

(defmethod emitter ((scene Scene))
  (let* ((emits (emitters scene))
	 (l (length emits))
	 (index (random l)))
    (if (not (= l 0))
      (samplePoint (elt emits index))
      (make-vector3f '(0.0 0.0 0.0)))))

(defmethod defaultEmission ((scene Scene) (backDirection Vector3f))
  (if (< (getv 1 backDirection) 0.0d0)
    (sky scene)
    (ground scene)))
