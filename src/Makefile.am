bin_PROGRAMS = ggadt

ggadt_SOURCES = constants.f03 options.f03 common.f03

if USING_FFTW
ggadt_SOURCES += fftwmod.f03
else
ggadt_SOURCES += gpfa.f90 gpfamod.f03
endif

ggadt_SOURCES += sphere.f03 spheres.f03 ellipsoid.f03 ggadt.f03 

install-data-local:
	mkdir -p @data_dir@
uninstall-local:
	rm -rf @data_dir@
clean-local:
	rm -rf @data_dir@
