(defpackage :net.parasec.tsp-art
  (:use :cl 
	:net.parasec.dithering
	:net.parasec.two-opt)
  (:export transform))

(in-package :net.parasec.tsp-art)

(defun image-to-tour (file-location)
  "convert a png to a tsp tour."
  (let* ((raw-img (load-png file-location))
	 (b&w-img (greyscale raw-img))
	 (dithered-img (transpose (floyd-steinberg-dithering b&w-img)))
	 (dimension (array-dimensions dithered-img))
	 (rows (first dimension))
	 (cols (second dimension))
	 (point-list nil))
    (random-filter dithered-img 0.1d0) ; prevent contiguous black regions.
    (dotimes (i rows)
      (dotimes (j cols)
	(when (= (aref dithered-img i j) #x00)
	  (push (make-point (coerce i 'double-float) 
			    (coerce j 'double-float) 
			    1d0) 
		point-list))))
    (make-array (length point-list) 
		:element-type 'vec3 
		:initial-contents (reverse point-list))))

(defun transform (from-image-file-location to-points-file-location)
  (let* ((tour (image-to-tour from-image-file-location))
         (before-distance (tour-distance tour))
         (after-distance 0d0))
    (format t "image dithering of ~a resulted in ~a points...~%" 
	    from-image-file-location (length tour))
    (format t "hold tight... this can take a long time...~%")
    (setq after-distance (optimise tour))
    (dump-points tour to-points-file-location)
    (format t "done. original distance = ~4$ 2-opt distance = ~4$~%" 
	    before-distance after-distance)
    (format t "file in ~a. :)~%" to-points-file-location)))
