(load "scene.lisp")
(load "surfacepoint.lisp")

(defclass RayTracer ()
  ((scene
     :initarg :scene
     :initform (make-instance 'Scene)
     :accessor scene)))

(defgeneric radiance (rayt rayOrigin rayDirection lastHit)
	    (:documentation "..."))

(defgeneric sampleEmitters (rayt rayDirection surfacePoint)
	    (:documentation "..."))

(defmethod radiance ((rayt RayTracer)
		     (rayOrigin Vector3f)
		     (rayDirection Vector3f)
		     lastHit)
  (multiple-value-setq (pHitObject hitPosition)
    (scene-intersects rayOrigin rayDirection lastHit))
  (if (not (numberp pHitObject))
    (let* ((surfp (make-surfacepoint pHitObject hitPosition))
	   (rad (if (= 0 lastHit)
		       (make-vector3f '(0.0 0.0 0.0))
		       (emission surfp rayOrigin (neg rayDirection) NIL)))
	   (rad (+ rad (sampleEmitters rayt rayDirection surfp))))
      (multiple-value-setq (outDirection color)
	(nextDirection surfp (neg rayDirection)))
      (if (not (isZero outDirection))
	(return-from radiance (+ rad
				 (mul color
				      (radiance rayt (pos surfp) nextDirection
						(triangle surfp)))))
	(return-from radiance rad)))
    (return-from radiance (defaultEmission (scene rayt) (neg rayDirection)))))

(defmethod sampleEmitters ((rayt RayTracer)
			   (rayDirection Vector3f)
			   (surfp SurfacePoint))
  (multiple-value-setq (emitterPosition emitter) (emitter (scene rayt)))
  (if (not (numberp emitter)) ; Si es numero, entonces es 0, sino es un Triangle
    (let ((emitDirection (unitize (sub emitterPosition (pos surfp)))))
      (multiple-value-bind (pHitObject hitPosition) 
	(scene-intersect (pos surfp) emitDirection (triangle surfp))
	(if (or (numberp pHitObject) ()))))

;;;;;;;;;;;;;;;; CONTINUAR, LINEA 128, RAYTRACER.CPP

    (multiple-value-setq (pHitObject hitPosition)
      (scene-intersects rayOrigin rayDirection (triangle surfp)))
    ))
