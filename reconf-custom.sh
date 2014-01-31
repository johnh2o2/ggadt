#!/bin/bash

bash cleandist-custom.sh
git log --pretty --graph > ChangeLog

aclocal -I./m4
autoconf
automake -a

bash configure || exit
make || exit
make html || exit
src/ggadt > test_output.dat || exit
python scripts/plot.py test_output.dat || exit
