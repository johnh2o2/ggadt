VERSION = 0.12

FF = gfortran
FFLAGS = -O3
LIBS = -L./lib -lfftw3
INC = -I./inc
EXECUTABLE=ggadt_v$(VERSION)
SRCDIR=./src
#SOURCES=$(wildcard $(SRCDIR)/*.f)
DRIVERSRC=ggadt.f03
DRIVEROBJ=ggadt.o

MODULESRC=params.f03 sphere.f03 ellipsoid.f03 fftwmod.f03
OBJECTS=$(patsubst %.f03,%.o,$(MODULESRC))

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) $(DRIVEROBJ)

	$(FF) $(LIBS) $(INC) $(FFLAGS) $(DRIVEROBJ) $(OBJECTS) -o $@

%.o: $(SRCDIR)/%.f03
	$(FF) -c $(LIBS) $(INC) $(FFLAGS) $<

test: clean all
	./$(EXECUTABLE)  > testdat.dat
	ipython scripts/plot.py

test2d: clean all
	./$(EXECUTABLE)  > testdat2d.dat
	ipython scripts/plot2d.py

clean:
	rm -f $(OBJECTS) $(EXECUTABLE) $(DRIVEROBJ)
	rm -f *o *mod 
	rm -f $(SRCDIR)/*mod
