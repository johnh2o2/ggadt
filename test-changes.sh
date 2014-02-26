#!/bin/bash

bash cleandist-custom.sh
git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a


# test new options (ngrain, nscatter)

rm test_output.dat
bash configure --enable-fftw3 
make || exit
#options="--max-angle=3000 --ngrain=256 --nscatter=16"
options="--ngrain=512 --nscatter=36 --max-angle=5000 --norientations=100"
sphere_options="--grain-geometry=spheres --cluster-file-name=$HOME/Desktop/Draine_temp/GGADT_JohnsMac/GGADT/data/clusters/BAM1.256.1.targ"

rm test_*exp\.dat
rm test_*reg\.dat

echo "Experimental fft"
echo "================"
echo "|---->Testing sphere module"
time src/ggadt $options --use-experimental-fft > test_sphere_exp.dat || exit
python scripts/plot.py test_sphere_exp.dat || exit 

echo "|---->Testing ellipsoid module"
time src/ggadt $options --grain-geometry=ellipsoid --grain-axis-x=1 --grain-axis-y=0.7 --grain-axis-z=1 --use-experimental-fft > test_ellipsoid_exp.dat || exit
python scripts/plot.py test_ellipsoid_exp.dat || exit
<<COMMENT
echo "|---->Testing spheres module"
time src/ggadt $options $sphere_options --use-experimental-fft > test_spheres_exp.dat || exit
python scripts/plot.py test_spheres_exp.dat || exit
COMMENT

echo "Padded fft"
echo "================"
echo "   Testing sphere module"
time src/ggadt $options  > test_sphere_reg.dat || exit 
python scripts/plot.py test_sphere_reg.dat || exit 

echo "   Testing ellipsoid module"
time src/ggadt $options --grain-geometry=ellipsoid --grain-axis-x=1 --grain-axis-y=0.7 --grain-axis-z=1 > test_ellipsoid_reg.dat || exit 
python scripts/plot.py test_ellipsoid_reg.dat || exit

echo "   Testing spheres module"
time src/ggadt $options $sphere_options  > test_spheres_reg.dat || exit
python scripts/plot.py test_spheres_reg.dat || exit 

