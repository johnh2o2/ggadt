FC = $(MPIFC)
AM_CPPFLAGS = -DWITH_MPI

bin_PROGRAMS = ggadt-mpi

ggadt_mpi_SOURCES = common-mpi.f03 fftwmod-mpi.f03 sphere-mpi.f03 spheres-mpi.f03 ellipsoid-mpi.f03 ggadt-mpi.f03 

ggadt_mpi_FCFLAGS = -I@fftw3_inc_dir@