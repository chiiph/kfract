(load "vector3f.lisp")

(setf test1 (make-instance 'Vector3f :v1 2 :v2 3 :v3 4))
(setf test2 (make-instance 'Vector3f :v1 1 :v2 2 :v3 3))

(setf min (make-instance 'Vector3f :v1 1 :v2 1 :v3 1))
(setf max (make-instance 'Vector3f :v1 3 :v2 3 :v3 3))

;(format t "The dot product is ~S" (dot test1 test2))
;(describe (cross test1 test2))
;(describe (div test1 "a"))
;(describe (mul test1 2))
(clamp test1 min max)
(describe test1)
;(describe (neg test2))
