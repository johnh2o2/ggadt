#!/bin/bash

bash reconf-custom.sh || exit
./configure --enable-openmp || exit
make || exit
<<COMMENT
echo "Sequential orientations"
src/ggadt --grain-axis-x=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --spin-axis=xaxis --euler-angle-mode=sequential > output.dat || exit
python scripts/plot.py output.dat
COMMENT
echo "Random orientations"
echo "X-axis:"
src/ggadt --grain-axis-y=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --rotation-axis=xaxis --euler-angle-mode=random > outputx.dat || exit
python scripts/plot.py outputx.dat &
echo "y-axis:"
src/ggadt --grain-axis-y=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --rotation-axis=yaxis --euler-angle-mode=random > outputy.dat || exit
python scripts/plot.py outputy.dat &
echo "z-axis:"
src/ggadt --grain-axis-y=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --rotation-axis=zaxis --euler-angle-mode=random > outputz.dat || exit
python scripts/plot.py outputz.dat
