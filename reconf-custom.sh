#!/bin/bash

set -x

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a
./configure || exit
make html || exit
cp doc/ggadt.html/* doc/htmldoc

make clean

#make dist
