FORT=gfortran
INCLUDES=-I./inc
LIBS=-L./lib


all:
	$(FORT) $(LIBS) -lm -lfftw3 $(INCLUDES) -c spheres.f03
	$(FORT) -o spheres $(LIBS) -lm -lfftw3 spheres.o