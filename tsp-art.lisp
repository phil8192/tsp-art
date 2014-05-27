;; (ql:quickload "png-read")
;; (ql:quickload "zpng")

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

(defclass point ()
  ((x
    :initarg :x
    :initform (error "need x.")
    :reader x)
   (y
    :initarg :y
    :initform (error "need y.")
    :reader y)))

(defclass city(point)
  ((active
    :initarg :active
    :initform t
    :accessor active-p
    :documentation "dont-look-bit: if set, the city's adjacent edges
before and after, will not be considered when searching for a 2-opt move.")))

(defun distance-squared (p1 p2)
  "for comparing 2 edges."
  (let ((dx (- (x p1) (x p2)))
	(dy (- (y p1) (y p2))))
    (+ (* dx dx) (* dy dy)))) 

(defun distance (p1 p2)
  "euclidean distance."
  (sqrt (distance-squared p1 p2)))

(defun line-to-point (line)
  "given a string of the form X Y create an instance of city." 
  (let ((coordinates 
	 (with-input-from-string (s line)
	   (loop
	      :for num := (read s nil nil)
	      :while num
	      :collect num))))
    (make-instance 'city
		   :x (first coordinates)
		   :y (second coordinates))))

(defun load-points (file-location)
  "load points from a file into a vector. this vector, when
iterated in order, represents a tour."
  (let ((in (open file-location))
	(result nil))
    (loop for line = (read-line in nil)
       while line do 
	 (push (line-to-point line) result))
    (close in)
    (make-array (length result)
		:initial-contents (reverse result)
		:element-type 'city)))

(defun reverse-subseq (array from to)
  (do ((i from (1+ i))
       (j to (1- j)))
      ((>= i j))
    (let ((tmp (aref array i)))
      (setf (aref array i) (aref array j))
      (setf (aref array j) tmp))))

(defun wrap (i max)
  (mod (+ i max) max))

(defun move-cost (a b c d)
  (let ((ab (distance-squared a b)) (cd (distance-squared c d))
	(ac (distance-squared a c)) (bd (distance-squared b d)))
    (if (and (< ab ac) (< cd bd))
	1
	(+ (sqrt ab) (sqrt cd)
	   (sqrt ac) (sqrt bd)))))

(defun activate (a b c d)
  (setf (active-p a) t) (setf (active-p b) t)
  (setf (active-p c) t) (setf (active-p d) t))

(defun try-move (points from to a b c d)
  (let ((delta (move-cost a b c d)))
    (if (< delta 0)
	(progn 
	  (activate a b c d)
	  (reverse-subseq points (1+ (min from to)) (max from to)))
	nil)))

(defun find-move (current-idx current-point
		  points num-cities)
  (let* ((prev (wrap (1- current-idx) num-cities))
	 (next (wrap (1+ current-idx) num-cities))
	 (prev-point (aref points prev))
	 (next-point (aref points next)))
    (do ((i (wrap (+ current-idx 2) num-cities) j)
	 (j (wrap (+ current-idx 3) num-cities) (wrap (1+ j) num-cities)))
	((= j current-idx))
      (let ((c (aref points i))
	    (d (aref points j)))
	(or 
	 (try-move points prev i prev-point current-point c d)
	 (try-move points current-idx i current-point next-point c d)
	 0)))))
	
	
