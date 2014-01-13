#!/bin/bash
bash cleandist-custom.sh

aclocal
autoconf
automake -a
