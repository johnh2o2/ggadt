AC_INIT([ggadt], [0.17], [jah5@princeton.edu])
AC_PREREQ([2.63]) 
#AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([src/serial/ggadt-serial.f03])
AM_INIT_AUTOMAKE([subdir-objects])

#define default prefix
AC_PREFIX_DEFAULT(/usr/local)
ac_prefix=$prefix
if test "x$ac_prefix" = "xNONE"; then
    ac_prefix=$ac_default_prefix
fi
AC_SUBST(ac_prefix)
AC_DEFINE_UNQUOTED(CONFIG_PREFIX, ["$ac_prefix"], [Default installation prefix.])


# Move shared files to serial and open-mp code versions
[cp src/common.f03 src/serial/]
[cp src/options.f03 src/serial/]
[cp src/common.f03 src/omp/]
[cp src/options.f03 src/omp/]


#MPI is not currently operational (I can't find a Fortran 2003 MPI compiler...)
AC_ARG_ENABLE([mpi],
[  --enable-mpi    Compile mpi version],
[case "${enableval}" in
  yes) mpi=true ;;
  no)  mpi=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-mpi]) ;;
esac],[mpi=false])
AM_CONDITIONAL([MPI], [test x$mpi = xtrue])

# allows for --enable-openmp
AC_ARG_ENABLE([openmp],
[  --enable-openmp    Use OpenMP ],
[case "${enableval}" in
  yes) openmp=true ;;
  no)  openmp=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-openmp]) ;;
esac],[openmp=false])
AM_CONDITIONAL([OMP], [test x$openmp = xtrue])


AC_PROG_FC

AC_CHECK_LIB([m],[cos]) #check for the math library
AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library])) #check for fftw3 library
AC_CHECK_HEADERS([ fftw3.h stdlib.h stdio.h ]) #Check for necessary headers

#Try to find the fftw3.f03 headers

fftw3_inc_dir=${ac_srcdir}
AC_FIND_FILE("fftw3.f03", [/usr/local/include /usr/include ${ac_srcdir} ${ac_prefix}/include], fftw3_fortran_header)
AC_FIND_FILE("fftw3-mpi.f03", [/usr/local/include /usr/include ${ac_srcdir} ${ac_prefix}/include], fftw3_mpi_fortran_header)

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
elif test -f "${ac_prefix}/include/fftw3.f03"
then 
	AC_MSG_NOTICE([Found fftw3.f03 file in ${ac_prefix}. Will use this directory for fortran header files.])
	fftw3_inc_dir="${ac_prefix}/include"
else
	AC_MSG_ERROR([Cannot find fftw3 fortran headers!])
fi

AC_LANG([Fortran])

AC_MSG_CHECKING([to see if Fortran supports iso_c_binding])
AC_RUN_IFELSE([
  		AC_LANG_PROGRAM([],	[[ 
  								use, intrinsic :: iso_c_binding
  							]]
  			)],
  		AC_MSG_RESULT(yes), 
  		AC_MSG_RESULT(no)
  	)


AC_MSG_CHECKING([to see if Fortran supports iso_fortran_env])
AC_RUN_IFELSE([
  		AC_LANG_PROGRAM([],	[[ 
  								use, intrinsic :: iso_fortran_env
  							]]
  			)],
  		AC_MSG_RESULT(yes), 
  		AC_MSG_RESULT(no)
  	)

AC_MSG_CHECKING([to see if Fortran supports getpid ])
AC_RUN_IFELSE([
  		AC_LANG_PROGRAM([],	[[ 
  								use, intrinsic :: iso_c_binding
  								use, intrinsic :: iso_fortran_env
  								integer :: i
  								i = getpid() 
  							]]
  			)],
  		AC_MSG_RESULT(yes), 
  		AC_MSG_RESULT(no)
  	)
mpi_dir=
omp_dir=


plan_dir="${HOME}/.ggadt/plans"
mkdir -p $plan_dir

AC_MSG_NOTICE([using $plan_dir as the directory to store FFTW plans])

if test $mpi = true
then
	mpi_dir=[mpi]
	AC_MSG_ERROR([MPI is not available at this time.])
	AC_CONFIG_FILES([ src/mpi/Makefile ])
fi

omp_lib=true
if test $openmp = true
then
	omp_dir=[omp]
	
	
	AC_LANG(C)
	AC_OPENMP
	AC_CHECK_HEADERS([omp.h])
	AC_MSG_CHECKING([if we can use the lfftw3_omp library (in C)])
	CFLAGSOLD=$CFLAGS
	LDFLAGSOLD=$LDFLAGS 
	LDFLAGS="${LDFLAGSOLD} -lfftw3_omp -lfftw3"
	CFLAGS="${CFLAGSOLD} $OPENMP_CFLAGS"
	AC_RUN_IFELSE([
  		AC_LANG_PROGRAM([[
  							#include<omp.h>
  							#include<fftw3.h>
  						]], 
  						[[int main() 
  							{ fftw3_init_threads(); return 0; }
  						]]
  			)],
  		[ AC_MSG_RESULT(yes) ; omp_lib=" -lfftw3_omp"], 
  		[ AC_MSG_RESULT(no) ; AC_MSG_ERROR([Cannot seem to use the lfftw3_omp library (in C)! Check config.log for more details.])]
  	)
	LDFLAGS=$LDFLAGSOLD
	CFLAGS=$CFLAGSOLD
	AC_LANG([Fortran])
	AC_OPENMP
	AC_MSG_CHECKING([that we can utilize openmp in fortran])
	FCFLAGSOLD=$FCFLAGS
	FCFLAGS="${FCFLAGSOLD} $OPENMP_FCFLAGS"
	AC_RUN_IFELSE([	
		AC_LANG_PROGRAM([], 
  						[[  
  							use, intrinsic :: iso_c_binding
  							use, intrinsic :: iso_fortran_env
  							use omp_lib
  						]]
  			)],
  		AC_MSG_RESULT(yes), 
  		[ AC_MSG_RESULT(no) ; AC_MSG_ERROR([Cannot seem to use openmp in fortran! Check config.log for more details.])]
  	)
	FCFLAGS=$FCFLAGSOLD
  	
	
	
	AC_CONFIG_FILES([ src/omp/Makefile ])
fi

AC_PROG_INSTALL
AC_CONFIG_FILES([ Makefile src/Makefile src/serial/Makefile src/serial/fftwmod-serial.f03 src/mpi/fftwmod-mpi.f03 src/omp/fftwmod-omp.f03 ])
AC_SUBST(fftw3_inc_dir)
AC_SUBST(plan_dir)
AC_SUBST(OPENMP_FCFLAGS)
AC_SUBST(mpi_dir)
AC_SUBST(omp_lib)
AC_SUBST(omp_dir)
AC_OUTPUT
