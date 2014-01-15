#!/bin/bash
bash cleandist-custom.sh

aclocal -I./m4
autoconf
automake -a

bash configure --enable-openmp --enable-profiling || exit
make || exit
make html || exit
cp doc/*png doc/ggadt.html/
