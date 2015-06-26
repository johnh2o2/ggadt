#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
automake --add-missing --force-missing -c
autoconf
./configure --enable-openmp || exit
#./configure || exit
make html || exit
cp doc/ggadt.html/* doc/htmldoc

make clean

#make dist
