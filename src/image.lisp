(load "vector3f.lisp")

(defconstant RGB_LUMINANCE (make-vector3f (list 0.2126 0.7152 0.0722)))
(defconstant DISPLAY_LUMINANCE_MAX 200.0)

(defclass Image ()
  ((width
     :initarg :width
     :initform 0
     :accessor width)
   (height
     :initarg :height
     :initform 0
     :accessor height)
   (pixels
     :initarg :pixels
     :initform (make-array 0 :element-type 'Vector3f)
     :accessor pixels)))

(defgeneric make-image (width height)
	    (:documentation "Creates an Image of width x height"))

(defgeneric index (img x y)
	    (:documentation "Returns the array index for coords (x,y)"))

(defgeneric sum (img x y radiance)
	    (:documentation "Adds radiance to the pixel (x,y)"))

(defgeneric write-image (img file)
	    (:documentation "Writes the final image in PPM format"))

(defgeneric toneMapping (img divider)
	    (:documentation "..."))

;:::: IMPLEMENTATION ::::;

(defmethod index (img x y)
  (+ (* (width img) y) x))

;:::: CONSTRUCTOR    ::::;

(defmethod make-image (w h)
  (make-instance 'Image :width w :height h :pixels 
		 (make-array (* w h) :element-type 'Vector3f)))

;:::: GET/SET ::::;

(defmethod getv ((img Image) i)
  (elt (pixels img) i))

(defmethod setv ((img Image) i val)
  (setf (elt (pixels img) i) val))

;:::: THE REST ::::;

(defmethod sum ((img Image) x y (radiance Vector3f))
  (when (or (<= x 0) (>= x (width img))
	    (<= y 0) (>= y (height img)))
    (error "Coordinates are wrong!"))
  (let ((ind (index img x y)))
    (setv img ind (plus (getv img ind) radiance))))

(defmethod toneMapping ((img Image) divider)
  (let ((sumOfLogs        0.0))
    (setf sumOfLogs (loop
		      for p across (pixels img)
		      for i from 0 to (- (length (pixels img)) 1)
		      with Y = (* (dot (getv img i) RGB_LUMINANCE) divider)
		      summing (if (> Y 1e-4) Y 1e-4)))
    (let* ((logMeanLuminance (expt 10.0 (/ sumOfLogs (length (pixels img)))))
	   (a (+ 1.219 (expt (* DISPLAY_LUMINANCE_MAX 0.25) 0.4)))
	   (b (+ 1.219 (expt logMeanLuminance 0.4))))
      (/ (expt (/ a b) 2.5) DISPLAY_LUMINANCE_MAX))))
