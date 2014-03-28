#!/bin/bash

bash reconf-custom.sh || exit
./configure --enable-openmp || exit
make || exit

echo "Sequential orientations"
src/ggadt --grain-axis-x=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --spin-axis=xaxis --euler-angle-mode=sequential > output.dat || exit
python scripts/plot.py output.dat
echo "Random orientations"
src/ggadt --grain-axis-x=0.5 --grain-axis-z=0.5 --grain-geometry=ellipsoid --spin-axis=xaxis --euler-angle-mode=random > output.dat || exit
python scripts/plot.py output.dat
