AC_INIT([ggadt], [0.9.84], [jah5@princeton.edu])
AC_PREREQ([2.63]) 
#AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([src/sphere.f90])


#AM_MAINTAINER_MODE([disable])

AC_CANONICAL_HOST
# Check for which host we are on and setup a few things
# specifically based on the host
macports_prefix=""
macports_include="."


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

#specify that you want to use the FFTW3 algorithm for your FFT's
AC_ARG_ENABLE([fftw3],
[  --enable-fftw3    Use the FFTW3 library (if installed) ],
[case "${enableval}" in
  yes) fftw3=true ; using_fftw3=true ; using_gpfa=false ;;
  no)  fftw3=false ; using_fftw3=false ; using_gpfa=true ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-fftw3]) ;;
esac],[using_fftw3=false ; using_gpfa=true])
AM_CONDITIONAL([OMP], [test x$openmp = xtrue])

AC_CHECK_LIB([m],[cos]) #check for the math library


case $host_os in
  darwin* )
        # test for macports
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
        # Default case
        AC_MSG_ERROR([Your platform is not currently supported])
        ;;
esac
FFTW3_UNCOMMENT="!"
FFTW3_COMMENT=
USE_FFTW=

GPFA_UNCOMMENT="!"
GPFA_COMMENT=
USE_GPFA=

if test $using_fftw3 = true
  then
  FFTW3_UNCOMMENT=
  FFTW3_COMMENT="!"

  USE_FFTW="use fftw"
  AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library])) #check for fftw3 library
  AC_CHECK_HEADERS([ fftw3.h ]) #Check for necessary headers

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
  # Where to put the FFTW plans -- ideally we should figure out how to work
  # with the standard GNU datadir variable.
  data_dir="${HOME}/.ggadt"
  plan_dir="$data_dir/plans"

  mkdir -p $data_dir
  mkdir -p $plan_dir
  AC_MSG_NOTICE([using $plan_dir as the directory to store FFTW plans])
elif test $using_gpfa = true
  then
    GPFA_UNCOMMENT="!"
    GPFA_COMMENT=
    USE_GPFA="use gpfa"
else
  AC_MSG_ERROR([There is no fft algorithm set!])
fi
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
#AC_ARG_ENABLE([profiling],
#[  --enable-profiling    Add -pg compiler flag ],
#[case "${enableval}" in
#  yes) 
#		FCFLAGSOLD=$FCFLAGS
#		FCFLAGS="${FCFLAGS} -pg" 
#		AC_MSG_CHECKING([ if compiler recognizes -pg ])
#		AC_COMPILE_IFELSE([
#  			AC_LANG_PROGRAM([],	[[ 
#  									integer :: i 
#  								]]
#  				)],
#  		AC_MSG_RESULT(yes), 
#  		[	AC_MSG_RESULT(no) ; 
#  			FCFLAGS=$FCFLAGSOLD ; 
#  			AC_MSG_WARN([Your fortran compiler does not recognize the '-pg' tag. Ignoring --enable-profiling.]) 
#  		]
#  	)
#	;;
#  no)  ;;
#  *) AC_MSG_ERROR([bad value ${enableval} for --enable-profiling]) ;;
#esac],[])






USE_OMP_LIB=
OMP_COMMENT=
OMP_UNCOMMENT="! [OMP] "

omp_lib=true
if test $openmp = true
then
	USE_OMP_LIB="use omp_lib"
	OMP_UNCOMMENT=
  OMP_COMMENT="!"

	AC_LANG(C)
	AC_OPENMP
	AC_CHECK_HEADERS([omp.h])
#  if test $using_fftw3 = true
#  then
#  	AC_MSG_CHECKING([if we can use the lfftw3_threads library (in C)])
#  	CFLAGSOLD=$CFLAGS
#  	LIBSOLD=$LDFLAGS 
#  	LIBS="${LIBS} -lfftw3_threads"
#  	CFLAGS="${CFLAGSOLD} $OPENMP_CFLAGS"
#
#  	AC_RUN_IFELSE([
#    		AC_LANG_PROGRAM([[
#    							#include<omp.h>
#    							#include<fftw3.h>
#    						]], 
#    						[[int main() 
#    							{ fftw3_init_threads(); return 0; }
#    						]]
#    			)],
#    		[ AC_MSG_RESULT(yes) ], 
#    		[ AC_MSG_RESULT(no) ; LIBS=$LIBSOLD ; AC_MSG_ERROR([Cannot seem to use the lfftw3_threads library (in C)! Check config.log for more details.])]
#    	)
#  	CFLAGS=$CFLAGSOLD
#  fi
	AC_LANG([Fortran])
	AC_OPENMP
	AC_MSG_CHECKING([that we can utilize openmp in fortran])
	FCFLAGSOLD=$FCFLAGS
	FCFLAGS="${FCFLAGSOLD} $OPENMP_FCFLAGS"
	AC_RUN_IFELSE([	
		AC_LANG_PROGRAM([], 
  						[[  
  							use omp_lib
  						]]
  			)],
  		AC_MSG_RESULT(yes), 
  		[ AC_MSG_RESULT(no) ; AC_MSG_ERROR([Cannot seem to use openmp in fortran! Check config.log for more details.])]
  	)

	FCFLAGS=$FCFLAGSOLD
  FCFLAGS="${FCFLAGS} $OPENMP_FCFLAGS"
  	
fi
AC_LANG([Fortran])


#Figure out how the Fortran compiler deals with command line arguments
IF_GETARG= 
IF_GET_COM_ARG= 
IF_IARGC= 
IF_COM_ARG_CNT=
IF_NEWLINE=
IF_NO_NEWLINE=


#This is a f2003 thing, but if it works, woohoo!
AC_MSG_CHECKING([if Fortran compiler recognizes command_argument_count])

AC_RUN_IFELSE([ 
  AC_LANG_PROGRAM([], 
            [[  
               integer :: i 
               i = command_argument_count()
            ]]
      )],
    [ AC_MSG_RESULT(yes) ; com_arg_cnt_works=true],  
    [ AC_MSG_RESULT(no) ; com_arg_cnt_works=false]
  )

# if that didn't work, check that iargc() works.
if test $com_arg_cnt_works = false
  then
  AC_MSG_CHECKING([Fortran compiler didn't seem to recognize command_argument_count, trying iargc])

  AC_RUN_IFELSE([ 
  AC_LANG_PROGRAM([], 
            [[  
               integer :: i 
               i = iargc()
            ]]
      )],
    [ AC_MSG_RESULT(yes) ],  
    [ AC_MSG_RESULT(no) ; AC_MSG_ERROR([neither iargc nor command_argument_count seem to be recognized by your compiler...]) ]
  )
fi

# assume that if iargc works == > getarg works, and command_argument_count works ==> get_command_argument works.
if test $com_arg_cnt_works = true
  then 
    IF_GETARG="!"
    IF_IARGC="!"
  else
    IF_GET_COM_ARG="!"
    IF_COM_ARG_CNT="!"
fi

AC_MSG_CHECKING([if Fortran compiler recognizes new_line('a')])

AC_RUN_IFELSE([ 
  AC_LANG_PROGRAM([], 
            [[  
               character(len=*) :: buf
               buf = new_line('a')
               
            ]]
      )],
    [ AC_MSG_RESULT(yes) ; newline_works=true ],  
    [ AC_MSG_RESULT(no) ; newline_works=false ]
  )

if test $newline_works = true
  then 
    IF_NO_NEWLINE="!"
  else
    IF_NEWLINE="!"
fi

#FCFLAGS=$FCFLAGSOLD
#FCFLAGS="${FCFLAGS} $OPENMP_FCFLAGS"



if test $using_fftw3 = true
  then
  echo "USING FFTW3 -- should be in makefile..."
fi
if test $using_gpfa = true
  then
  echo "USING GPFA -- should be in makefile..."
fi
AM_CONDITIONAL([USING_FFTW], [test $using_fftw3 = true])
AM_CONDITIONAL([USING_GPFA], [test $using_gpfa = true])

AC_PROG_INSTALL
AC_CONFIG_FILES([ 	Makefile 
					src/Makefile 
					doc/Makefile 
					src/fftwmod.f90
          src/spheres.f90
					src/common.f90
          src/constants.f90
          src/options.f90
          src/ggadt.f90

          scripts/installation_vars.py
				])
AC_SUBST(fftw3_inc_dir)
AC_SUBST(plan_dir)
AC_SUBST(data_dir)
AC_SUBST(OPENMP_FCFLAGS)
AC_SUBST(USE_OMP_LIB)
AC_SUBST(OMP_COMMENT)
AC_SUBST(OMP_UNCOMMENT)
AC_SUBST(FFTW3_UNCOMMENT)
AC_SUBST(FFTW3_COMMENT)
AC_SUBST(USE_FFTW)
AC_SUBST(USING_FFTW)
AC_SUBST(GPFA_UNCOMMENT)
AC_SUBST(GPFA_COMMENT)
AC_SUBST(USE_GPFA)
AC_SUBST(omp_lib)
AC_SUBST(abs_builddir)
AC_SUBST(IF_GETARG) 
AC_SUBST(IF_GET_COM_ARG)
AC_SUBST(IF_IARGC)
AC_SUBST(IF_COM_ARG_CNT)
AC_SUBST(IF_NEWLINE)
AC_SUBST(IF_NO_NEWLINE)

AC_OUTPUT
