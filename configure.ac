AC_INIT([ggadt], [0.9.4], [jah5@princeton.edu])
AC_PREREQ([2.63]) 
#AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([src/serial/ggadt-serial.f03])

AC_CANONICAL_HOST
# Check for which host we are on and setup a few things
# specifically based on the host
macports_prefix=""
macports_include="."
case $host_os in
  darwin* )
        #test for macports
        AC_MSG_NOTICE([MAC OSX specific checks:])
        AC_MSG_NOTICE([========================])


        AC_MSG_CHECKING([if macports is installed])
        MACPORTS_TEST=`port version`
        if [[ "X${MACPORTS_TEST}" == "X" ]]
        then
          AC_MSG_RESULT([no])
          USING_MACPORTS="no"
        else
          AC_MSG_RESULT([yes])
          USING_MACPORTS="yes"
          AC_MSG_CHECKING([for macports install of fftw-3])
          PORT_TEST_FFTW3=`port installed fftw-3 | awk "/fftw-3/"`

          if [[ "X${PORT_TEST_FFTW3}" == "X" ]]
          then
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([Macports is installed, but fftw-3 is not.])
            AC_MSG_ERROR([fix this by doing: sudo port install fftw-3])
          else
            AC_MSG_RESULT([yes])
          fi

          AC_MSG_CHECKING([guessing macports parent directory])
          fftw3_header_file=`port contents fftw-3 | grep "fftw3.f03"`
          macport_inc_dir=`dirname $fftw3_header_file`
          PWD_OLD=$PWD
          cd $macport_inc_dir
          cd ../..
          macports_prefix=$PWD
          cd $PWD_OLD
          AC_MSG_RESULT([$macports_prefix])
          macports_include=$macports_prefix/local/include
          LDFLAGS="$LDFLAGS -L$macports_prefix/local/lib"
          

        fi
        AC_MSG_NOTICE([========================])
        ;;
  linux*)
        # Do something specific for linux
        ;;
   *BSD*)
        # Do something specific for BSD
        ;;
    *)
        #Default Case
        AC_MSG_ERROR([Your platform is not currently supported])
        ;;
esac

AM_INIT_AUTOMAKE([subdir-objects])
AC_PROG_CC
AC_PROG_FC 

#define default prefix
AC_PREFIX_DEFAULT(/usr/local)
ac_prefix=$prefix
if test "x$ac_prefix" = "xNONE"; then
    ac_prefix=$ac_default_prefix
fi
AC_SUBST(ac_prefix)
AC_DEFINE_UNQUOTED(CONFIG_PREFIX, ["$ac_prefix"], [Default installation prefix.])


# allows for --enable-openmp
AC_ARG_ENABLE([openmp],
[  --enable-openmp    Use OpenMP ],
[case "${enableval}" in
  yes) openmp=true ;;
  no)  openmp=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-openmp]) ;;
esac],[openmp=false])
AM_CONDITIONAL([OMP], [test x$openmp = xtrue])


AC_CHECK_LIB([m],[cos]) #check for the math library
AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library])) #check for fftw3 library
AC_CHECK_HEADERS([ fftw3.h stdlib.h stdio.h ]) #Check for necessary headers

#Try to find the fftw3.f03 headers

fftw3_inc_dir=${ac_srcdir}
AC_FIND_FILE("fftw3.f03", [ /usr/local/include /usr/include ${ac_srcdir} ${ac_prefix}/include $macports_include ], fftw3_fortran_header)

if test -f "${macports_include}/fftw3.f03"
then 
  AC_MSG_NOTICE([Found fftw3.f03 file in ${macports_include}. Will use this directory for fortran header files.])
  fftw3_inc_dir="${macports_include}"
elif test -f "${fftw3_inc_dir}/fftw3.f03"
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

INC="$INC -I$fftw3_inc_dir"

# Now make sure the Fortran compiler is sane.

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

#getpid is needed for the random number generator

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

# adds -pg compiler flag for using gprof
AC_ARG_ENABLE([profiling],
[  --enable-profiling    Add -pg compiler flag ],
[case "${enableval}" in
  yes) 
		FCFLAGSOLD=$FCFLAGS
		FCFLAGS="${FCFLAGS} -pg" 
		AC_MSG_CHECKING([ if compiler recognizes -pg ])
		AC_COMPILE_IFELSE([
  			AC_LANG_PROGRAM([],	[[ 
  									integer :: i 
  								]]
  				)],
  		AC_MSG_RESULT(yes), 
  		[	AC_MSG_RESULT(no) ; 
  			FCFLAGS=$FCFLAGSOLD ; 
  			AC_MSG_WARN([Your fortran compiler does not recognize the '-pg' tag. Ignoring --enable-profiling.]) 
  		]
  	)
	;;
  no)  ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-profiling]) ;;
esac],[])


# Where to put the FFTW plans 
plan_dir="${HOME}/.ggadt/plans"


AC_MSG_NOTICE([using $plan_dir as the directory to store FFTW plans])

omp_dir=
USE_OMP_LIB=


omp_lib=true
if test $openmp = true
then
	omp_dir=[omp]
	USE_OMP_LIB="use omp_lib"
	
	AC_LANG(C)
	AC_OPENMP
	AC_CHECK_HEADERS([omp.h])
	AC_MSG_CHECKING([if we can use the lfftw3_threads library (in C)])
	CFLAGSOLD=$CFLAGS
	LDFLAGSOLD=$LDFLAGS 
	LDFLAGS="${LDFLAGSOLD} -lfftw3_threads -lfftw3"
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
  		[ AC_MSG_RESULT(yes) ; omp_lib=" -lfftw3_threads"], 
  		[ AC_MSG_RESULT(no) ; AC_MSG_ERROR([Cannot seem to use the lfftw3_threads library (in C)! Check config.log for more details.])]
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
AC_CONFIG_FILES([ 	Makefile 
					src/Makefile 
          src/combined/Makefile
					src/serial/Makefile 
					doc/Makefile 
					src/serial/fftwmod-serial.f03 
					src/omp/fftwmod-omp.f03 
          src/combined/fftwmod.f03
					src/common.f03
				])
AC_SUBST(fftw3_inc_dir)
AC_SUBST(plan_dir)
AC_SUBST(OPENMP_FCFLAGS)
AC_SUBST(omp_lib)
AC_SUBST(omp_dir)

AC_OUTPUT

# Move shared files to serial and open-mp code versions
[cp src/common.f03 src/serial/]
[cp src/options.f03 src/serial/]
[cp src/common.f03 src/omp/]
[cp src/options.f03 src/omp/]
[cp src/common.f03 src/combined/]
[cp src/options.f03 src/combined/]
