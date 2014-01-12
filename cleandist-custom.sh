#!/bin/bash
rm -f aclocal.m4
rm -r -f autom4te.cache/
rm  -f *o
rm  -f *mod
rm -f config.status
rm  -f config.log
rm  -f configure
rm  -f Makefile
rm -f src/ggadt
rm -f src/*o
rm -f src/*mod
rm -f src/Makefile.in
rm -f src/Makefile
rm -f Makefile.in


rm -f src/ggadt-mpi
rm -f src/ggadt-omp

rm -f src/mpi/*mod
rm -f src/mpi/*o
rm -f src/mpi/Makefile
rm -f src/mpi/Makefile.in
rm -f src/mpi/ggadt-mpi

rm -f src/omp/*mod
rm -f src/omp/*o
rm -f src/omp/Makefile
rm -f src/omp/Makefile.in
rm -f src/omp/ggadt-omp

rm -f plans/*
