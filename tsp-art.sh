#!/bin/bash
# ./tsp-art.sh <input.png> <output.points>
#
# experiment: invert for Chiaroscuro style.
# http://en.wikipedia.org/wiki/Chiaroscuro
# convert lenna-med.png -negate lenna-med-negate.png

REP1="https://raw.githubusercontent.com/phil8192/dithering-algorithms/master/dithering.lisp"
REP2="https://raw.githubusercontent.com/phil8192/tsp-lisp/master/two-opt.lisp"

RUN=$(cat <<EOF
(progn 
  (net.parasec.tsp-art:transform "$1" "$2")
  (quit))
EOF
)

if [ ! -e "dithering.lisp" ]; then 
  echo "snatching from" $REP1 "..."
  curl -s $REP1 >dithering.lisp
fi

if [ ! -e "two-opt.lisp" ]; then 
  echo "snatching from" $REP2 "..."
  curl -s $REP2 >two-opt.lisp
fi

if [ ! -e "dithering.fasl" ]; then
  echo "compiling dithering.fasl ..."
  sbcl --noinform --load dithering.lisp --eval \
      '(progn (compile-file "dithering.lisp") (quit))' >/dev/null 2>&1
fi

if [ ! -e "two-opt.fasl" ]; then
  echo "compiling two-opt.fasl ..."
  sbcl --noinform --load two-opt.lisp --eval \
      '(progn (compile-file "two-opt.lisp") (quit))' >/dev/null 2>&1
fi

if [ ! -e "tsp-art.fasl" ]; then
  echo "compiling tsp-art.fasl ..."
  sbcl --noinform --load dithering.fasl \
		  --load two-opt.fasl \
		  --eval \
      '(progn (compile-file "tsp-art.lisp") (quit))' >/dev/null 2>&1
fi

echo "running two-opt ..."
sbcl --noinform --load dithering.fasl \
	        --load two-opt.fasl \
                --load tsp-art.fasl \
	        --eval "$(echo $RUN)" 2>/dev/null

echo "done."

