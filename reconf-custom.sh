#!/bin/bash

set -x

bash cleandist-custom.sh
git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

std="f2003"

bash configure  #FCFLAGS="-O2 -fimplicit-none -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wimplicit-interface -Wunused-parameter -fwhole-file -fcheck=all -std=${std} -pedantic -fbacktrace" || exit
#make || exit
#cp doc/*png doc/ggadt.html
#make html || exit
#src/ggadt > test_output_fftw3.dat || exit
#python scripts/plot.py test_output_fftw3.dat || exit

#bash configure --enable-fftw3 
make || exit
make html || exit
cp doc/*png doc/ggadt.html || exit

src/ggadt --sed --material=custom --material-file=data/materials/index_silD03 --ephot-min=0.7 --ephot-max=1.8 --dephot=0.2 || exit

echo "SUCCESS!!!"




<<COMMENT
cluster_dir="$HOME/Desktop/Draine_temp/GGADT_JohnsMac/GGADT/data/clusters/"

rm -f test_changes_exp.dat
rm -f test_changes_reg.dat
args="--ngrain=128" 
args="${args} --nscatter=128" 
args="${args} --max-angle=3000."
args="${args} --norientations=100"
args="${args} --grain-geometry=spheres"
args="${args} --cluster-file-name=${cluster_dir}/BAM1.256.1.targ"
args="${args} --ephot=2.0"
args="${args} --aeff=0.2"
args="${args} --ior-re=-1.0E-4"
args="${args} --ior-im=1.0E-4"
#args="${args} --grain-geometry=sphere"
echo "======EXPERIMENTAL MODE======="
time src/ggadt $args > test_changes_exp.dat || exit
#echo "======NON EXP. MODE    ======="
#time src/ggadt $args > test_changes_reg.dat || exit


#python scripts/plot.py test_changes_exp.dat
#python scripts/plot.py test_changes_reg.dat
#time src/ggadt --use-experimental-fft --ngrid=512 --grid-width=2.0 > test_output_expfft.dat
#time src/ggadt --ngrid=2048 --grid-width=8.0 > test_output_expfft_reg.dat
#make html || exit
#python scripts/make_sample_plots.py
#python scripts/HD09.py
#src/ggadt --grain-geometry=spheres --cluster-file-name=data/clusters/BA.256.1.targ --euler-angle-mode=file --euler-angle-file=eul_angle_file.dat > test_output_gpfa.dat || exit
#python scripts/plot.py test_output_gpfa.dat || exit
COMMENT
