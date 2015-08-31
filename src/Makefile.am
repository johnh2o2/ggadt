bin_PROGRAMS = ggadt



ggadt_SOURCES = constants.f90 options.f90 interp.f90

if USING_FFTW
ggadt_SOURCES += fftwmod.f90 
AM_FCFLAGS = -I$(fftw3_inc_dir)
else
ggadt_SOURCES += gpfa.f90 gpfamod.f90
endif

ggadt_SOURCES += common.f90 sphere.f90 spheres.f90 ellipsoid.f90 custom.f90 ggadt.f90 

#install-data-local:
#	mkdir -p @data_dir@
#uninstall-local:
#	rm -rf @data_dir@
#clean-local:
#	rm -rf @data_dir@
#	rm -rf *mod
