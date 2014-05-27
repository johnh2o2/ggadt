#!/bin/bash

. reconf-custom.sh
make

src/ggadt --nscatter=512 > analytical_sphere.dat
src/ggadt --force-numerical --nscatter=512 --ngrain=1024 > numerical_sphere.dat

#python scripts/plot.py numerical_sphere.dat
python scripts/plot.py analytical_sphere.dat
