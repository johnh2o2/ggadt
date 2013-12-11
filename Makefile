VERSION = 0.15
#FF = gfortran-mp-4.8
FF = gfortran
FFLAGS = -O3
#LIBS = -L./lib -lfftw3
#INC = -I./inc
LIBS=-lfftw3
INC= -I./inc
EXECUTABLE=ggadt_v$(VERSION)
SRCDIR=./src
#SOURCES=$(wildcard $(SRCDIR)/*.f)
DRIVERSRC=ggadt.f03
DRIVEROBJ=ggadt.o

MODULESRC=common.f03 sphere.f03 spheres.f03 ellipsoid.f03 fftwmod.f03 
OBJECTS=$(patsubst %.f03,%.o,$(MODULESRC))

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) $(DRIVEROBJ)

	$(FF) $(LIBS) $(INC) $(FFLAGS) $(DRIVEROBJ) $(OBJECTS) -o $@

%.o: $(SRCDIR)/%.f03
	$(FF) -c $(LIBS) $(INC) $(FFLAGS) $<

run: clean all
	./$(EXECUTABLE) parameterfile.ini

test: clean all
	./$(EXECUTABLE) parameterfile.ini  > data/testdat_thetaphi.dat
	ipython scripts/plot_thetaphi.py

clean:
	rm -f $(OBJECTS) $(EXECUTABLE) $(DRIVEROBJ)
	rm -f *o *mod 
	rm -f $(SRCDIR)/*mod

cleanplans:
	rm -f plans/*
