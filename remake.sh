#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

#./configure --enable-openmp || exit
./configure --enable-fftw3 --enable-openmp || exit 1
#./configure || exit 1
make || exit 1
cp src/* ../temp_bins/temp-serial
