(load "surfacepoint.lisp")

;(0.278000 0.275000 -0.789000)
;(0.045177 -0.239136 -0.969935)
;(1000.000000 1000.000000 1000.000000)
;TRIANGLE:
;(0.213000 0.545000 0.227000)
;(0.343000 0.545000 0.332000)
;(0.213000 0.545000 0.332000)
;(0.700000 0.700000 0.700000)
;(1000.000000 1000.000000 1000.000000)
;(0.226992 0.545000 0.306121)

;(setf tri (make-triangle '(0.213000 0.545000 0.227000)
;                         '(0.343000 0.545000 0.332000)
;                         '(0.213000 0.545000 0.332000)))

;(setf (reflectivity tri) (make-vector3f '(0.700000 0.700000 0.700000)))
;(setf (emitivity tri) (make-vector3f '(1000.000000 1000.000000 1000.000000)))

;(setf pos (make-vector3f '(0.226992 0.545000 0.306121)))

;(setf surfpos (make-surfacepoint tri pos))

;(setf inDirection (make-vector3f '(0.278000 0.275000 -0.789000)))
;(setf outDirection (make-vector3f '(0.045177 -0.239136 -0.969935)))

;(print "***** EMISSION:")

;(describe (emission surfpos inDirection outDirection NIL))

;(0.006 0.0 0.559) v0
;(0.556 0.0 0.0) v1
;(0.006 0.0 0.559) v2
;(0.7 0.7 0.7) reflec
;(0.0 0.0 0.0) emit
;(0.349778707208 0.0 0.000250677650593) pos
;(0.15339004845 0.0204784248673 -0.987953504549) in_dir
;(155.231030977 41.0896754857 0.512290680565) in_rad
;(-0.08556649943 0.327824061739 -0.940855865008) out_dir
;(0.708309812515 0.187489705868 0.00233755141366) get_reflection

(setf inDirection (make-vector3f (list 0.15339004845 0.0204784248673 -0.987953504549)))
(setf inRadiance (make-vector3f (list 155.231030977 41.0896754857 0.512290680565)))
(setf outDirection (make-vector3f (list -0.08556649943 0.327824061739 -0.940855865008)))

(setf tri (make-triangle '(0.006 0.0 0.559)
			 '(0.556 0.0 0.0)
			 '(0.006 0.0 0.559)))

(setf (reflectivity tri) (make-vector3f '(0.7 0.7 0.7)))
(setf (emitivity tri) (make-vector3f '(0.0 0.0 0.0)))

(setf pos (make-vector3f '(0.349778707208 0.0 0.000250677650593)))

(setf surfpos (make-surfacepoint tri pos))

;(describe inDirection)
;(describe (tangent tri))
;(describe (cross (tangent tri) (sub (getv tri 2) (getv tri 1))))
;(describe (dot inDirection (normal tri)))
;(print "***** REFLECTION:")

;(print (verts inDirection))
;(print (verts (normal tri)))
;(print (verts (tangent tri)))
;(print (verts (cross (tangent tri) (sub (getv tri 2) (getv tri 1)))))
;(print (dot inDirection (normal tri)))
;(describe (reflection surfpos inDirection inRadiance outDirection))

;(describe pos)

(multiple-value-setq (outDirection color) (nextDirection surfpos inDirection))
(print (verts outDirection))
(print (verts color))
