FC = gfortran
FFLAGS = -O3 -lm
OBJS =	gpfft3.o gpfa.o setgpfa.o gpfa2f.o gpfa3f.o gpfa5f.o

gpfatest:	libgpfa.a gpfatest.o  fft3d.o timer.o
	$(FC) -o gpfatest $(FFLAGS) gpfatest.o fft3d.o timer.o libgpfa.a

libgpfa.a: $(OBJS)
	ar rv libgpfa.a $(OBJS)
	ranlib libgpfa.a

timer.o:	timer.f
	$(FC) -c $(FFLAGS) timer.f

.f.o:	
	$(FC) -c $(FFLAGS) $<

