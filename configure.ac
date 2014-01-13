AC_INIT([ggadt], [0.17], [jah5@princeton.edu])
AC_PREREQ([2.69]) 
AC_CONFIG_MACRO_DIRS([m4])

AC_CONFIG_SRCDIR([src/serial/ggadt-serial.f03])
AM_INIT_AUTOMAKE([subdir-objects])
AC_ARG_ENABLE([mpi],
[  --enable-mpi    Compile mpi version],
[case "${enableval}" in
  yes) mpi=true ;;
  no)  mpi=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-mpi]) ;;
esac],[mpi=false])
AM_CONDITIONAL([MPI], [test x$mpi = xtrue])

AC_ARG_ENABLE([openmp],
[  --enable-openmp    Use OpenMP ],
[case "${enableval}" in
  yes) openmp=true ;;
  no)  openmp=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-openmp]) ;;
esac],[openmp=false])
AM_CONDITIONAL([OMP], [test x$openmp = xtrue])
AC_PROG_FC
AC_CHECK_LIB([m],[cos])



mpi_dir=
omp_dir=

plan_dir=$PWD/plans
mkdir -p $plan_dir

AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library]))


#check for FFTW3


AC_FIND_FILE("fftw3.f03", [/usr/local/include /usr/include ${ac_srcdir}], fftw3_fortran_header)
AC_FIND_FILE("fftw3-mpi.f03", [/usr/local/include /usr/include ${ac_srcdir}], fftw3_mpi_fortran_header)

fftw3_inc_dir=${ac_srcdir}

if test -f "${fftw3_inc_dir}/fftw3.f03"
then 
	AC_MSG_NOTICE([Found fftw3.f03 file in ${fftw3_inc_dir}. Will use this directory for fortran header files.])
elif test -f "/usr/local/include/fftw3.f03"
then 
	AC_MSG_NOTICE([Found fftw3.f03 file in /usr/local/include. Will use this directory for fortran header files.])
	fftw3_inc_dir="/usr/local/include"
elif test -f "/usr/include/fftw3.f03"
then 
	AC_MSG_NOTICE([Found fftw3.f03 file in /usr/include. Will use this directory for fortran header files.])
	fftw3_inc_dir="/usr/include"
else
	AC_MSG_ERROR([Cannot find fftw3 fortran headers!])
fi


if test $mpi = true
then
	mpi_dir=[mpi]
	#AC_CHECK_LIB(fftw3_mpi,fftw_mpi_init,,AC_MSG_ERROR([You do not have the MPI version of the FFTW3 library installed.]))
	#AX_MPI(AC_MSG_NOTICE([ Found MPI compiler (${MPIFC}). ]), AC_MSG_ERROR([You do not seem to have any usable MPI compilers...]))
	AC_CONFIG_FILES([ src/mpi/Makefile ])
fi

if test $openmp = true
then
	omp_dir=[omp]
	#AC_LANG([C])
	#AC_OPENMP
	#AC_CHECK_LIB(fftw3_threads,fftw_init_threads, , AC_MSG_ERROR([You do not have the OpenMP version of the FFTW3 library installed.]))
	AC_LANG([Fortran])
	AC_OPENMP
	AC_MSG_NOTICE([Fortran OpenMP compiler: ${MPIFC}])

	AC_CONFIG_FILES([ src/omp/Makefile ])
fi

AC_PROG_INSTALL
AC_CONFIG_FILES([ Makefile src/Makefile src/serial/Makefile src/serial/fftwmod-serial.f03 src/mpi/fftwmod-mpi.f03 src/omp/fftwmod-omp.f03 ])
AC_SUBST(fftw3_inc_dir)
AC_SUBST(plan_dir)
AC_SUBST(OPENMP_FCFLAGS)
AC_SUBST(mpi_dir)
AC_SUBST(omp_dir)
AC_OUTPUT
