bin_PROGRAMS = ggadt @mpi_prog@ @omp_prog@
EXTRA_PROGRAMS = ggadt-mpi ggadt-omp
#SUBDIRS= mpi omp

ggadt_SOURCES = common.f03 fftwmod.f03 sphere.f03 spheres.f03 ellipsoid.f03 ggadt.f03 
ggadt_FCFLAGS = -I@fftw3_inc_dir@

ggadt_mpi_SOURCES = mpi/common-mpi.f03 mpi/fftwmod-mpi.f03 mpi/sphere-mpi.f03 mpi/spheres-mpi.f03 mpi/ellipsoid-mpi.f03 mpi/ggadt-mpi.f03 
ggadt_mpi_FCFLAGS = -I@fftw3_inc_dir@

ggadt_omp_SOURCES = omp/common-omp.f03 omp/fftwmod-omp.f03 omp/sphere-omp.f03 omp/spheres-omp.f03 omp/ellipsoid-omp.f03 omp/ggadt-omp.f03 
ggadt_omp_FCFLAGS = -I@fftw3_inc_dir@
