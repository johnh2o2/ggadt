#!/bin/bash

bash cleandist-custom.sh
git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

#bash configure --enable-fftw3 #FCFLAGS="-O2 -fimplicit-none -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wimplicit-interface -Wunused-parameter -fwhole-file -fcheck=all -std=f2003 -pedantic -fbacktrace" || exit
#make || exit
#cp doc/*png doc/ggadt.html
#make html || exit
#src/ggadt > test_output_fftw3.dat || exit
#python scripts/plot.py test_output_fftw3.dat || exit

rm test_output_expfft.dat
bash configure --enable-fftw3 
make || exit
src/ggadt --use-experimental-fft --ngrid=1024 --grid-width=2 > test_output_expfft.dat
#cp doc/*png doc/ggadt.html
#make html || exit
#python scripts/make_sample_plots.py
#python scripts/HD09.py
#src/ggadt --grain-geometry=spheres --cluster-file-name=data/clusters/BA.256.1.targ --euler-angle-mode=file --euler-angle-file=eul_angle_file.dat > test_output_gpfa.dat || exit
#python scripts/plot.py test_output_gpfa.dat || exit

