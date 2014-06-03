#!/bin/bash
# ./generate-image.sh <in.points> <out.png> <width> <height> 
#                     <line-width> "<background-colour>" "<line-colour>"
# e.g.,
# ./generate-image.sh lenna-med-negate.points lenna-med-negate-tsp.png \
#     1024 1024 1 "#000000" "#ff0000"
gnuplot -e "unset border; unset xtics; unset ytics;
            set term pngcairo size $4,$3 enhanced; set output '$2'; 
            set label ''; set key off; 
            set obj 1 rectangle behind from screen 0,0 to screen 1,1; 
            set obj 1 fillstyle solid 1 fillcolor rgbcolor '$6'; 
            plot '$1' using 1:2 with lines lw $5 lc rgb '$7'"

convert $2 -rotate 90 $2

