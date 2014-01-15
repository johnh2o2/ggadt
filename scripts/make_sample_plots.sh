#!/bin/bash


cd ../src/serial

#Test sphere
./ggadt-serial --parameter-file-name=../../data/paramfiles/parameterfile-sphere.ini > ../../data/test_sphere.dat
./ggadt-serial --parameter-file-name=../../data/paramfiles/parameterfile-spheres.ini > ../../data/test_spheres.dat
./ggadt-serial --parameter-file-name=../../data/paramfiles/parameterfile-ellipsoid.ini > ../../data/test_ellipsoid.dat

cd ../../

ipython scripts/plot.py data/test_sphere.dat
mv dQscat_dOmega_1d.png test_sphere_1d.png
mv dQscat_dOmega_2d.png test_sphere_2d.png

ipython scripts/plot.py data/test_spheres.dat
mv dQscat_dOmega_1d.png test_spheres_1d.png
mv dQscat_dOmega_2d.png test_spheres_2d.png

ipython scripts/plot.py data/test_ellipsoid.dat
mv dQscat_dOmega_1d.png test_ellipsoid_1d.png
mv dQscat_dOmega_2d.png test_ellipsoid_2d.png

