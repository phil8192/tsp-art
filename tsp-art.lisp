;; (ql:quickload "png-read")
;; (ql:quickload "zpng")

(defpackage :tsp-art
  (:use :cl))
(in-package :tsp-art)

;; constants for dithering error diffusion filter
(defconstant +diffusion-e+  7/16 "east pixel")
(defconstant +diffusion-sw+ 3/16 "south west pixel") 
(defconstant +diffusion-s+  5/16 "south pixel")
(defconstant +diffusion-se+ 1/16 "south east pixel")

(defun load-png (file-location)
  "load a png into an array. note that the array will be either
2 or 3d depending on if the image is greyscale or not and furthermore,
each r g b value may be 8 or 16 bits wide depending on if the image
has an alpha channel or not."
  (png-read:image-data (png-read:read-png-file file-location)))

(defun save-png (image-array &key file-location)
  "save an image matrix to a png."
  (with-open-file (stream file-location
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (zpng:write-png-stream 
     (make-instance 'zpng:png
		    :color-type :grayscale
		    :height (first (array-dimensions image-array))
		    :width (second (array-dimensions image-array))
		    :bpp 8
		    :image-data (flat-array image-array))
     stream)))
  
(defun component-fun (image-array)
  "this function evaluates to a lambda expression which takes as an argument
an r g b or greyscale value (components). if there is an alpha component 
for an r g b or greyscale value, the component will be 2 bytes: rA gA bA 
or greyscaleA. in this case, ignore the alpha by shifting 8 bits to the 
right."
  (if (equal (array-element-type image-array) '(unsigned-byte 16))
      (lambda (component) (ash component -8))
      (lambda (component) component)))

(defun greyscale-pixel-fun (image-array)
  "this function evaluates to a lambda expression which takes as an argument
a position (i j) of the image-array and evaluates to a greyscale/luminosity
value. the lambda expression, which closes over image-array, depends on the 
type of image represented in the matrix. (1) in the first case if the matrix 
is 3d and the image has rgb components, expression will convert (i j 0:2)
to greyscale (ignoring possible alpha byte at 3). (2) in the second case, if
the matrix is 3d, but the image is greyscale, the value at i j 0 is evaluted.
in the final case (3) the matrix is 2d (the image is greyscale) - component
i j is evaluated."
  (let ((component (component-fun image-array))
	(dimension (array-dimensions image-array)))
    (if (= (length dimension) 3)
	(if (>= (third dimension) 3)
	    (lambda (i j) 
	      (let ((r (funcall component (aref image-array i j 0)))
		    (g (funcall component (aref image-array i j 1)))
		    (b (funcall component (aref image-array i j 2))))
		(round (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))))
	    (lambda (i j)
	      (funcall component (aref image-array i j 0))))
	(lambda (i j)
	  (funcall component (aref image-array i j))))))

(defun greyscale (image-array)
  "converting an image to greyscale is not as straight forward as
r+g+b /3. this is because the human eye has varying degrees of 
sensitivity to different wavelengths of light according to densities
of photoreceptor cells (cone cells) in the retina. for example,
the eye perceives green as brighter compared to red or blue when
illuminated with the same intensity. as such, a weighted combination
is used to determine the luminosity (y) of a pixel:

y = (0.2126*r) + (0.7152*g) + (0.0722*b)"
  (let* ((dimension (array-dimensions image-array))
	 (rows (first  dimension))
	 (cols (second dimension))
	 (greyscale-pixel (greyscale-pixel-fun image-array))
	 (greyscale-matrix (make-array 
			    (list rows cols) 
			    :element-type '(unsigned-byte 8))))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref greyscale-matrix i j) (funcall greyscale-pixel i j))))
    greyscale-matrix))

(defun floyd-steinberg-dithering (image-array)
  "perform non-boustrophedon floyd steinberg dithering."
  (let* ((dimension (array-dimensions image-array))
	 (rows (first  dimension))
	 (cols (second dimension))
	 (quant-errors (make-array (list 2 cols) 
				   :element-type 'double-float 
				   :initial-element 0.0d0)))
    (flet ((find-closest-palette-colour (old-pixel)
	     (if (< old-pixel 128) #x00 #xFF))
	   (cond-incf (i j delta)
	     (and (<= 0 j) (< j cols)
		 (incf (aref quant-errors i j) delta))))
      (do ((i 0 (1+ i))
	   (current-errors 0 (mod (1+ i) 2))
	   (next-errors 1 (mod i 2)))
	  ((= i rows) image-array)
	(dotimes (j cols)
	  (let* ((old-pixel (+ (aref image-array i j) 
			       (aref quant-errors current-errors j)))
		 (new-pixel (find-closest-palette-colour old-pixel))
		 (quant-error (- old-pixel new-pixel))
		 (e-delta  (* quant-error +diffusion-e+))
		 (sw-delta (* quant-error +diffusion-sw+))
		 (s-delta  (* quant-error +diffusion-s+))
		 (se-delta (* quant-error +diffusion-se+)))
	    (cond-incf current-errors (1+ j) e-delta)
	    (cond-incf next-errors (1- j) sw-delta)
	    (cond-incf next-errors j s-delta)
	    (cond-incf next-errors (1+ j) se-delta)
	    (setf (aref image-array i j) new-pixel)
	    (setf (aref quant-errors current-errors j) 0.0d0)))))))

;;_________________auxiliary functions____________________________

(defun flat-array (array)
  "flatten an array to a vector."
  (let* ((len (array-total-size array))
	 (flat (make-array (list len) 
			   :element-type (array-element-type array))))
    (dotimes (i len)
      (setf (aref flat i) (row-major-aref array i)))
    flat))

(defun transpose (array)
  "transpose a matrix."
  (let* ((dimension (array-dimensions array))
	 (rows (second dimension))
	 (cols (first  dimension))
	 (transposed-matrix (make-array
			     (list rows cols)
			     :element-type (array-element-type array))))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref transposed-matrix i j) (aref array j i))))
    transposed-matrix))

;;_________________test___________________________________________
(defun test-greyscale ()
  (let* ((raw-img (load-png "~/lenna.png"))
	 (b&w-img (transpose (greyscale raw-img))))
    (save-png b&w-img :file-location "~/lenna-out.png")))

(defun test-dithering ()
  (let* ((raw-img (load-png "~/lenna.png"))
	 (b&w-img (transpose (greyscale raw-img)))
	 (dithered-img (floyd-steinberg-dithering b&w-img)))
    (save-png dithered-img :file-location "~/lenna-out.png")))
  
;;________________ tsp.___________________________________________

(deftype vec3 () '(simple-array double-float (3)))

(defun make-point (x y active)
  (let ((vec3 (make-array 3 :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref vec3 0) x
          (aref vec3 1) y
          (aref vec3 2) active)
    vec3))

(defmacro point-x (point)
  `(aref ,point 0))

(defmacro point-y (point)
  `(aref ,point 1))

(defmacro point-active (point)
  `(aref ,point 2))

(declaim (inline distance-squared))
(defun distance-squared (p1 p2)
  "for comparing 2 edges."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vec3 p1 p2))
  (let ((dx (- (point-x p1) (point-x p2)))
	(dy (- (point-y p1) (point-y p2))))
    (+ (* dx dx) (* dy dy))))

(defun distance (p1 p2)
  "euclidean distance."
  (declare (type vec3 p1 p2))
  (declare (optimize (speed 3) (debug 0)))
  (sqrt (distance-squared p1 p2)))

(defun tour-distance (points)
  (declare (optimize (speed 3) (debug 0)))
  (let ((tour-length (length points)))
    (do ((i 1 (1+ i))
	 (d 0 (+ d (distance (aref points (1- i)) (aref points i)))))
	((= i tour-length)
	 (+ d (distance (aref points (1- tour-length)) (aref points 0)))))))

(defun line-to-point (line)
  "given a string of the form X Y create an instance of city." 
  (let ((coordinates 
	 (with-input-from-string (s line)
	   (loop
	      :for num := (read s nil nil)
	      :while num
	      :collect num))))
    (make-point (first coordinates)
		(second coordinates)
                1.0d0)))

(defun load-points (file-location)
  "load points from a file into a vector. this vector, when
iterated in order, represents a tour."
  (let ((*read-default-float-format* 'double-float) 
       (in (open file-location))
	(result nil))
    (loop for line = (read-line in nil)
       while line do 
	 (push (line-to-point line) result))
    (close in)
    (make-array (length result)
		:initial-contents (reverse result))))

(defun reverse-subseq (array from to)
  (do ((i from (1+ i))
       (j to (1- j)))
      ((>= i j))
    (let ((tmp (aref array i)))
      (setf (aref array i) (aref array j))
      (setf (aref array j) tmp))))

(declaim (inline wrap))
(defun wrap (i max)
  (declare (optimize (speed 3) (debug 0)))
  (declare (type fixnum i max))
  (mod (+ i max) max))

(declaim (inline move-cost))
(defun move-cost (a b c d)
  (declare (type vec3 a b c d))
  (declare (optimize (speed 3) (debug 0)))
  (let ((ab (distance-squared a b)) (cd (distance-squared c d))
	(ac (distance-squared a c)) (bd (distance-squared b d)))
    (declare (type double-float ab cd ac bd))
    (if (and (< ab ac) (< cd bd))
	1
	(- (+ (sqrt ac) (sqrt bd))
           (+ (sqrt ab) (sqrt cd))))))

(defun activate (a b c d)
  (declare (type vec3 a b c d))
  (setf (point-active a) 1.0d0) (setf (point-active b) 1.0d0)
  (setf (point-active c) 1.0d0) (setf (point-active d) 1.0d0))

(declaim (inline try-move))
(defun try-move (points from to a b c d)
  (declare (type vec3 a b c d))                 
  (let ((delta (move-cost a b c d)))
    (if (< delta 0)
	(progn 
	  (activate a b c d)
	  (reverse-subseq points (1+ (min from to)) (max from to))
	  delta)
	nil)))

(defun find-move (current-idx current-point
		  points num-cities)
  (let* ((prev (wrap (1- current-idx) num-cities))
	 (next (wrap (1+ current-idx) num-cities))
	 (prev-point (aref points prev))
	 (next-point (aref points next)))
    (do ((i (wrap (+ current-idx 2) num-cities) j)
	 (j (wrap (+ current-idx 3) num-cities) (wrap (1+ j) num-cities)))
	((= j current-idx) 0)
      (let* ((c (aref points i))
	     (d (aref points j))
	     (delta (or 
		     (try-move points prev i prev-point current-point c d)
		     (try-move points current-idx i current-point next-point c d))))
	(if delta
	    (return-from find-move delta))))))

;; nasty.	
(defun optimise (points)
  (let ((best (tour-distance points))
	(num-cities (length points))
	(visited 0)
	(current 0))
    (do ()
	((= visited num-cities) best)
      (let ((current-point (aref points current)))
	(if (= (point-active current-point) 1.0d0)
	    (let ((modified (find-move current current-point points num-cities)))
	      (if (< modified 0)
		  (progn
		    (setf current (wrap (1- current) num-cities))
		    (setf visited 0)
		    (incf best modified))
		  (progn
		    (setf (point-active current-point) 0.0d0)
		    (setf current (wrap (1+ current) num-cities))
		    (incf visited))))
	    (progn
	      (setf current (wrap (1+ current) num-cities))
	      (incf visited)))))))

(defun test-optimise ()
  (let ((tour-array (load-points "greece.points")))
    (format t "starting distance = ~4$~%" (tour-distance tour-array))
    (format t "optimised distance = ~4$~%" (optimise tour-array))))

