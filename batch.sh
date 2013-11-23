#!/bin/bash

make clean
make || exit
echo "Compiled..."
./spheres > testdat.dat || exit
echo "Ran code..."
ipython plot${1}.py || exit
echo "Plotted..."
