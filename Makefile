FORT=gfortran
INCLUDES=-I./inc
LIBS=-L./lib


all: test spheres

test:
	$(FORT) $(LIBS) -lm -lfftw3 $(INCLUDES) -c test.f03
	$(FORT) -o test $(LIBS) -lm -lfftw3 test.o

spheres:
	$(FORT) $(LIBS) -lm -lfftw3 $(INCLUDES) -c spheres.f03
	$(FORT) -o spheres $(LIBS) -lm -lfftw3 spheres.o

clean:
	rm -f test spheres *o