# Don't edit this file unless you know what you're doing!
# Autoconf configuration script.


AC_INIT([ggadt], [1.1.7r1], [jah5@princeton.edu])
AC_PREREQ([2.69]) 
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([src/sphere.f90])

AM_MAINTAINER_MODE([disable])

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

#if test "x$ac_datadir" = "xNONE"; then
#ac_datadir=$ac_prefix/share
#fi
#datadir=$ac_datadir
#AC_SUBST(datadir)

# User can specify non-standard location for macports.
AC_ARG_VAR([macports_prefix], [ (Apple users ONLY) The parent directory from which the macports tree starts. Usually /opt ])
if test "x$macports_prefix" = "x"; then
  macports_prefix=/opt
  AC_MSG_NOTICE([No macports_parent_dir set; using ${macports_prefix} ])
fi

# allows for --enable-openmp
AC_ARG_ENABLE([openmp],
[  --enable-openmp    Use OpenMP ],
[case "${enableval}" in
  yes) openmp=true ;; 
  no)  openmp=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-openmp]) ;;
esac],[openmp=false])
AM_CONDITIONAL([OMP], [test x$openmp = xtrue])

#Enable user to choose single precision
AC_ARG_ENABLE([single],
[  --enable-single    Use single precision (not recommended) ],
[case "${enableval}" in
  yes) use_double=false ;; 
  no)  use_double=true ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-double]) ;;
esac],[use_double=true])
AM_CONDITIONAL([SINGLE], [test x$use_double = xfalse])

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
        AC_MSG_WARN([Your platform is not recognized; we will try to configure anyway!])
        ;;
esac

# Initial values for comments (double by default, GPFA used by default)

DOUBLE_UNCOMMENT="!"
SINGLE_UNCOMMENT=

USE_OMP_LIB=
OMP_COMMENT=
OMP_UNCOMMENT="! [OMP] "

# If single-precision desired
if test x$use_double = xtrue
  then
  DOUBLE_UNCOMMENT=
  SINGLE_UNCOMMENT="!"
  AC_MSG_WARN([Using double precision])
fi



# Now make sure the Fortran compiler is sane.

AC_LANG([Fortran])
AC_FC_SRCEXT([f90], 
  AC_MSG_NOTICE([Using .f90 as suffix for tests]), 
  AC_MSG_ERROR([Cannot use .f90 suffix!])
)

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
  								integer :: i
  								i = getpid() 
  							]]
  			)],
  		AC_MSG_RESULT(yes), 
  		AC_MSG_RESULT(no)
  	)


# options file uses access='stream' option to read parameter files. This is not (to my knowledge)
# part of the Fortran 95 standard.
[echo 'testing. remove this file.' > testfile.dat]
AC_MSG_CHECKING([to see if Fortran compiler supports access='stream' option for open()])
AC_RUN_IFELSE([
      AC_LANG_PROGRAM([], [[ 
           integer :: unit       
           open(unit,file='testfile.dat',access='stream',status='old')
                  
                ]]
        )],
      AC_MSG_RESULT(yes), 
      [ AC_MSG_RESULT(no); 
        AC_MSG_WARN([
          ***WARNING*** your Fortran compiler does not support 'stream' access
          to files. This means that you will NOT be able to use parameter files.
        ])
      ]
      
    )
[rm testfile.dat]
#check that random_number() is recognized

AC_MSG_CHECKING([to see if Fortran supports random_number() ])
AC_RUN_IFELSE([
      AC_LANG_PROGRAM([], [[ 
                  
                  real, dimension(3) :: r
                  call random_number(r)

                ]]
        )],
      AC_MSG_RESULT(yes), 
      [ AC_MSG_RESULT(no) ; AC_MSG_ERROR([Fortran compiler does not recognize random_number()]) ]
    )


# Checks and settings for when user wants to use openmp
omp_lib=true
if test x$openmp = xtrue
then
  echo "USING OPENMP"
	USE_OMP_LIB="use omp_lib"
	OMP_UNCOMMENT=
  OMP_COMMENT="!"

	AC_LANG(C)
	AC_OPENMP
	AC_CHECK_HEADERS([omp.h])

	AC_LANG([Fortran])
	AC_OPENMP
	AC_MSG_CHECKING([that we can utilize openmp in fortran])
	FCFLAGSOLD=$FCFLAGS
  PARALLEL_FCFLAGS="$OPENMP_FCFLAGS"
	FCFLAGS="${FCFLAGSOLD} ${PARALLEL_FCFLAGS}"
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
  FCFLAGS="${FCFLAGS} $PARALLEL_FCFLAGS"
  	
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
  AC_MSG_CHECKING([Fortran compiler didnt seem to recognize command_argument_count, trying iargc])

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



AC_PROG_INSTALL
AC_CONFIG_FILES([ 	
	Makefile 
	src/Makefile 
        src/spheres.f90
	src/common.f90
        src/constants.f90
        src/options.f90
        src/ggadt.f90

        scripts/installation_vars.py

        data/default.params
				])
AC_SUBST(plan_dir)
AC_SUBST(data_dir)
AC_SUBST(OPENMP_FCFLAGS)
AC_SUBST(USE_OMP_LIB)
AC_SUBST(OMP_COMMENT)
AC_SUBST(OMP_UNCOMMENT)
AC_SUBST(omp_lib)
AC_SUBST(abs_builddir)
AC_SUBST(IF_GETARG) 
AC_SUBST(IF_GET_COM_ARG)
AC_SUBST(IF_IARGC)
AC_SUBST(IF_COM_ARG_CNT)
AC_SUBST(IF_NEWLINE)
AC_SUBST(IF_NO_NEWLINE)
AC_SUBST(DOUBLE_UNCOMMENT)
AC_SUBST(SINGLE_UNCOMMENT)
AC_OUTPUT
