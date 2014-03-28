#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a
#bash configure --enable-openmp
#std="f95"
# add -fimplicit-none
#bash configure FCFLAGS="-O2 -fall-intrinsics -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wimplicit-interface -Wunused-parameter -fwhole-file -fcheck=all -std=${std} -pedantic -fbacktrace" || exit
#bash configure FCFLAGS="-O2 -std=$std -fall-intrinsics"
#bash configure FC="/opt/local/bin/g95" # FCFLAGS="-O2 -std=$std -fall-intrinsics"
#cp doc/*png doc/ggadt.html
#make html || exit
#src/ggadt > test_output_fftw3.dat || exit
#python scripts/plot.py test_output_fftw3.dat || exit

#bash configure --enable-fftw3 
#make || exit
#make html || exit
#cp doc/ggadt.html/* doc/htmldoc/

#python scripts/testing_against_mie_theory.py
