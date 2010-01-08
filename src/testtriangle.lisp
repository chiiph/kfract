(load "triangle.lisp")

(setf vec1 (make-vector3f '(0.556 0.401054805321 0.40692763018)))
(setf vec2 (make-vector3f '(-0.745565615449 0.432850996024 -0.506726680076)))

(setf tri (make-triangle '(0.343 0.545 0.332)
			 '(0.213 0.545 0.227)
			 '(0.343 0.545 0.227)))

(print (intersects tri vec1 vec2))
(describe (samplePoint tri))
(print (area tri))
(describe (tangent tri))
(describe (normal tri))
