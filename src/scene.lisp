;;;; scene.lisp
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:scene)

(defconstant MAX_TRIANGLES #16r100000)
(defconstant MAX_EMITTERS_P 16)

(defclass scene ()
  ((items
     :initarg :items
     :initform '()
     :accessor items)
   (emitters
     :initarg :emitters
     :initform '()
     :accessor emitters)
   ; FALTA SPATIALINDEX
   (sky
     :initarg :sky
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor sky)
   (ground
     :initarg :ground
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor ground)))

(defgeneric make-scene (triangles emitters sky ground)
	    (:documentation "..."))

(defgeneric scene-intersects (scene rayOrigin rayDirection lastHit)
	    (:documentation "..."))

(defgeneric emitter (scene)
	    (:documentation "..."))

(defgeneric defaultEmission (scene backDirection)
	    (:documentation "..."))

;#### Definicion provisoria de intersects ####;
(defmethod scene-intersects ((scene        scene)
			     (rayOrigin    vector3f)
			     (rayDirection vector3f)
			     (lastHit      Triangle)) ; Se usa en SpatialIndex
  (loop for item in (items scene)
	with hitd = (intersects item rayOrigin rayDirection)
	when (>= hitd 0)
	do (return-from scene-intersects 
			(values item (plus rayOrigin (mul rayDirection hitd))))) ; Devuelve el elemento intersectado y la posicion
  (values 0 0)) ; Devuelve (0 0) cuando no intersecta

(defmethod emitter ((scene scene))
  (let* ((emits (emitters scene))
	 (l (length emits))
	 (index (random l)))
    (if (not (= l 0))
      (values (samplePoint (elt emits index)) (elt emits index))
      (values (make-vector3f '(0.0 0.0 0.0)) 0))))

(defmethod defaultEmission ((scene scene) (backDirection vector3f))
  (if (< (getv 1 backDirection) 0.0d0)
    (sky scene)
    (ground scene)))
