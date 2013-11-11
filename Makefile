FORT=gfortran


all:
	$(FORT) -c ggadt.f95
	$(FORT) -o ggadt ggadt.o