#!/bin/bash
rm -f aclocal.m4
rm -r -f autom4te.cache/
rm  -f *o
rm  -f *mod
rm -f config.status
rm  -f config.log
rm  -f configure
rm  -f Makefile.in
rm -f Makefile
rm -f src/Makefile
rm -f src/Makefile.in
rm -f src/common.f03
rm -f data/test_*
rm -f *png

rm -f doc/Makefile.in
rm -f doc/Makefile
rm -f doc/stamp-vti
rm -f doc/version.texi
rm -f doc/ggadt.info
rm -f -r doc/ggadt.html

rm -f src/serial/ggadt-serial
rm -f src/serial/*o
rm -f src/serial/*mod
rm -f src/serial/Makefile.in
rm -f src/serial/Makefile
rm -f src/serial/options.f03
rm -f src/serial/common.f03
rm -f src/serial/fftwmod-serial.f03


rm -f src/mpi/*mod
rm -f src/mpi/*o
rm -f src/mpi/Makefile
rm -f src/mpi/Makefile.in
rm -f src/mpi/ggadt-mpi
rm -f src/mpi/fftwmod-mpi.f03

rm -f src/omp/*mod
rm -f src/omp/options.f03
rm -f src/omp/common.f03
rm -f src/omp/*o
rm -f src/omp/Makefile
rm -f src/omp/Makefile.in
rm -f src/omp/ggadt-omp
rm -f src/omp/ggadt/fftwmod-omp.f03

rm -f plans/*
