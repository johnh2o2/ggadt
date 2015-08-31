#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

#./configure --enable-openmp || exit 1
./configure --enable-openmp --enable-maintainer-mode FCFLAGS="-O3" || exit 1
#./configure || exit
sudo make install || exit 1
#cp src/* ../temp_bins/temp-openmp
