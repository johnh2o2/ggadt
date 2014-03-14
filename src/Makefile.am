bin_PROGRAMS = ggadt

ggadt_SOURCES = constants.f95 options.f95

if USING_FFTW
ggadt_SOURCES += fftwmod.f95
else
ggadt_SOURCES += gpfa.f90 gpfamod.f95
endif

ggadt_SOURCES += common.f95 sphere.f95 spheres.f95 ellipsoid.f95 ggadt.f95 

install-data-local:
	mkdir -p @data_dir@
uninstall-local:
	rm -rf @data_dir@
clean-local:
	rm -rf @data_dir@
