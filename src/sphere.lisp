; ESTO ESTA EN VERSION ALPHA VER CUANDO ESTE EL RESTO FUNCIONAL

(load "polygon.lisp")

(defclass Sphere (Polygon)
  ((center
     :initarg :center
     :initform (make-vector3f '(0.0 0.0 0.0))
     :accessor center)
   (radius
     :initarg :radius
     :initform 1.0d0
     :accessor radius)))

(defgeneric make-sphere (center radius)
	    (:documentation "Creates an instance of Sphere class"))

(defmethod make-sphere ((center Vector3f) (radius Vector3f)))

; La normal es (sub (plus rayOrigin (mul rayDirection minDistance)) (center sph))
; Pero no se puede calcular en general, porque tiene normales para todos lados

(defmethod intersects ((sph Sphere) (rayOrigin Vector3f) (rayDirection Vector3f))
  (let* ((oc (sub (center sph) rayOrigin))
	 (oc2 (dot oc oc))
	 (tca (dot oc rayDirection))
	 (outside (> oc2 (expt (radius sph) 2))))
    (if (and (< tca 0.0) outside)
      (return-from intersects -1.0))
    (let* ((d2 (- oc2 (expt tca 2)))
	   (thc (- (expt (radius sph) 2) d2))
	   (tout (- tca (sqrt thc)))
	   (tin (+ tca (sqrt thc))))
      (if (< thc 0)
	(return-from intersects -1.0))
      (if (and outside (< tout 0))
	(return-from intersects -1.0)
	(return-from intersects tout))
      (if (and (not outside) (< tin 0))
	(return-from intersects -1.0)
	(return-from intersects tin)))))
