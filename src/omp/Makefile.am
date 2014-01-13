bin_PROGRAMS = ggadt-omp

ggadt_omp_SOURCES = common-omp.f03 fftwmod-omp.f03 sphere-omp.f03 spheres-omp.f03 ellipsoid-omp.f03 ggadt-omp.f03 

ggadt_omp_FCFLAGS = -I@fftw3_inc_dir@ @OPENMP_FCFLAGS@ 
ggadt_omp_LDADD = -lfftw3_omp
