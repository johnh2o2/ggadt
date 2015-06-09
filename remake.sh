#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake --add-missing --force-missing

#./configure --enable-openmp || exit
./configure --enable-openmp FCFLAGS="-O3" || exit 1
#./configure || exit 1
make || exit 1
