#!/bin/bash

set -x

#make html || exit

bash cleandist-custom.sh
rm configure

git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a
./configure FCFLAGS='-O3' || exit
make || exit
make html || exit
cp doc/ggadt.html/* doc/htmldoc
