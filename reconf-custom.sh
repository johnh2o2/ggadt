#!/bin/bash
bash cleandist-custom.sh

aclocal -I./m4
autoconf
automake -a
