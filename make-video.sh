#!/bin/bash
# hack.
# to generate points file every 500 2-opt invocations,
# replace reverse-subseq in two-opt.lisp with this:
#
# (defparameter *x* 0)
# (defun reverse-subseq (array from to)
#   (declare (type fixnum from to))
#   (do ((i from (1+ i))
#        (j to (1- j)))
#       ((>= i j))
#    (let ((tmp (svref array i)))
#      (setf (svref array i) (svref array j))
#      (setf (svref array j) tmp)))
#  (when (= (mod *x* 500) 0)
#    (format t "~a ~a~%" "writing points..." (/ *x* 500))
#    (dump-points array (concatenate 'string "/tmp/rev" 
#				    (write-to-string (/ *x* 500)) ".points")))
#  (incf *x*))
#
# note/todo: sbcl can invoke external programs as so:
# (sb-ext:run-program "/bin/bash" '("/tmp/x.sh"))
# ... perhaps have less hacky way to generate video.

# convert all point files to png
for((i=1,j=0;;i++,j++)) ;do
  filename="/tmp/rev"$i".points"
  if [ ! -e $filename ]; then
    echo "done. ("$i")" 
    break 
  fi
  outname="/tmp/img"$(printf %03d $j)".png"
  echo $filename "->" $outname "..."
  ./generate-image.sh $filename $outname 800 1120 0.5 "#000000" "#ff0000"
done

# make video
/home/phil/ffmpeg/ffmpeg/ffmpeg -r 25 -i /tmp/img%03d.png -vcodec libx264 -bf 0 -crf 16 -threads 0 -b 5000 tsp.mp4

