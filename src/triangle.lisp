(load "polygon.lisp")

(defclass Triangle (Polygon)
  ((vertex1
     :initarg :v1
     :initform (make-instance 'Vector3f)
     :accessor vertex1)
   (vertex2
     :initarg :v2
     :initform (make-instance 'Vector3f)
     :accessor vertex2)
   (vertex3
     :initarg :v3
     :initform (make-instance 'Vector3f)
     :accessor vertex3)))

(make-instance 'Triangle)
