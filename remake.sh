#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

./configure --enable-openmp || exit
#./configure --enable-fftw3 || exit
#./configure || exit
make
