;;;; raytracer.lisp
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:raytracer)

(defclass raytracer ()
  ((scene
    :initarg :scene
    :initform (make-instance 'scene)
    :accessor scene)))

(defgeneric radiance (rayt ray-origin ray-direction last-hit)
  (:documentation "..."))

(defgeneric sample-emitters (rayt ray-direction surface-point)
  (:documentation "..."))

(defmethod radiance ((rayt raytracer)
		     (ray-origin vector3f)
		     (ray-direction vector3f)
		     last-hit)
  (multiple-value-bind (object-hit-p hit-position)
      (scene-intersects ray-origin ray-direction last-hit)
    (if (not (numberp object-hit-p))
	(let* ((surfp (make-surfacepoint object-hit-p hit-position))
	       (rad (if (= 0 last-hit)
			(make-vector3f '(0.0 0.0 0.0))
			(emission surfp ray-origin (neg ray-direction) NIL)))
	       (rad (+ rad (sample-emitters rayt ray-direction surfp))))
	  (multiple-value-bind (direction-out color)
	      (direction-next surfp (neg ray-direction))
	    (if (not (zero-p direction-out))
		(return-from radiance (+ rad
					 (mul color
					      (radiance rayt (pos surfp) direction-out
							(triangle surfp)))))
		(return-from radiance rad))))
	(return-from radiance (default-emission (scene rayt) (neg ray-direction))))))

(defmethod sample-emitters ((rayt raytracer)
			   (ray-direction vector3f)
			   (surfp surface-point))
  (multiple-value-bind (emitter-position emitter) (emitter (scene rayt))
    (if (not (numberp emitter)) ; Si es numero, entonces es 0, sino es un Triangle
	(let ((emit-direction (unitize (sub emitter-position (pos surfp)))))
	  (multiple-value-bind (object-hit-p hit-position) 
	      (scene-intersect (pos surfp) emit-direction (triangle surfp))
	    (let* ((emission-in (if (or (numberp object-hit-p)
				       (eq-triangle emitter object-hit-p)) ; Generalizar, no todo va a ser Triangle
				   (emission (make-surfacepoint emitter emitter-position)
					     (pos surfp)
					     (neg emit-direction)
					     T)
				   (make-vector3f '(0.0 0.0 0.0)))))
	      (return-from sample-emitters (reflection surfp 
						      emit-direction
						      (mul emission-in (length (emitters (scene rayt))))
						      (neg ray-direction))))))
	(make-vector3f '(0.0 0.0 0.0)))))

;(defclass RayTracer ()
;  ((scene
;     :initarg :scene
;     :initform (make-instance 'Scene)
;     :accessor scene)))

;(defgeneric radiance (rayt rayOrigin rayDirection lastHit)
;            (:documentation "..."))

;(defgeneric sampleEmitters (rayt rayDirection surfacePoint)
;            (:documentation "..."))

;(defmethod radiance ((rayt RayTracer)
;                     (rayOrigin Vector3f)
;                     (rayDirection Vector3f)
;                     lastHit)
;  (multiple-value-bind (pHitObject hitPosition)
;    (scene-intersects rayOrigin rayDirection lastHit)
;    (if (not (numberp pHitObject))
;      (let* ((surfp (make-surfacepoint pHitObject hitPosition))
;             (rad (if (= 0 lastHit)
;                    (make-vector3f '(0.0 0.0 0.0))
;                    (emission surfp rayOrigin (neg rayDirection) NIL)))
;             (rad (+ rad (sampleEmitters rayt rayDirection surfp))))
;        (multiple-value-bind (outDirection color)
;          (nextDirection surfp (neg rayDirection))
;          (if (not (isZero outDirection))
;            (return-from radiance (+ rad
;                                     (mul color
;                                          (radiance rayt (pos surfp) outDirection
;                                                    (triangle surfp)))))
;            (return-from radiance rad))))
;    (return-from radiance (defaultEmission (scene rayt) (neg rayDirection))))))

;(defmethod sampleEmitters ((rayt RayTracer)
;                           (rayDirection Vector3f)
;                           (surfp SurfacePoint))
;  (multiple-value-bind (emitterPosition emitter) (emitter (scene rayt))
;    (if (not (numberp emitter)) ; Si es numero, entonces es 0, sino es un Triangle
;      (let ((emitDirection (unitize (sub emitterPosition (pos surfp)))))
;        (multiple-value-bind (pHitObject hitPosition) 
;          (scene-intersect (pos surfp) emitDirection (triangle surfp))
;          (let* ((emissionIn (if (or (numberp pHitObject)
;                                     (eq-triangle emitter pHitObject)) ; Generalizar, no todo va a ser Triangle
;                               (emission (make-surfacepoint emitter emitterPosition)
;                                         (pos surfp)
;                                         (neg emitDirection)
;                                         T)
;                               (make-vector3f '(0.0 0.0 0.0)))))
;            (return-from sampleEmitters (reflection surfp 
;                                                    emitDirection
;                                                    (mul emissionIn (length (emitters (scene rayt))))
;                                                    (neg rayDirection))))))
;      (make-vector3f '(0.0 0.0 0.0)))))
