#!/bin/bash
bash cleandist-custom.sh

aclocal -I./m4
autoconf
automake -a

bash configure --enable-openmp || exit
make || exit
make html || exit
cd src
./ggadt > ../test_output.dat || exit
