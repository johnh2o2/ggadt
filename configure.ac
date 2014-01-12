AC_DEFUN([AC_FIND_FILE],
[

	$3=NO
	for x in $2
	do
		for y in $1
		do
			if test -r "$x/$y"
			then
				$3=$x
				break 2
			fi
		done
	done

]
)

AC_INIT([ggadt], [0.17])
AC_CONFIG_SRCDIR([src/ggadt.f03])
AM_INIT_AUTOMAKE([subdir-objects])
AC_ARG_ENABLE([mpi],
[  --enable-mpi    Compile mpi version],
[case "${enableval}" in
  yes) mpi=true ;;
  no)  mpi=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-mpi]) ;;
esac],[mpi=false])
AM_CONDITIONAL([MPI], [test x$mpi = xtrue])

AC_ARG_ENABLE([omp],
[  --enable-omp    Use OpenMP ],
[case "${enableval}" in
  yes) omp=true ;;
  no)  omp=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-omp]) ;;
esac],[omp=false])
AM_CONDITIONAL([OMP], [test x$omp = xtrue])
AC_PREFIX_DEFAULT(.)
AC_PROG_FC
AC_CHECK_LIB([m],[cos])
AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library]))
AC_FIND_FILE("fftw3.f03", [/usr/local/include /usr/include ${ac_srcdir}], fftw3_fortran_header)
AC_FIND_FILE("fftw3-mpi.f03", [/usr/local/include /usr/include ${ac_srcdir}], fftw3_mpi_fortran_header)

fftw3_inc_dir=${ac_srcdir}
mpi_prog=
omp_prog=

AC_MSG_CHECKING([for Fortran headers for fftw3])

if test -f "${fftw3_inc_dir}/fftw3.f03"
then 
	[ echo "    Found fftw3.f03 file in ${fftw3_inc_dir}. Will use this directory for fortran header files."]
elif test -f "/usr/local/include/fftw3.f03"
then 
	[ echo "    Found fftw3.f03 file in /usr/local/include. Will use this directory for fortran header files."]
	fftw3_inc_dir="/usr/local/include"
elif test -f "/usr/include/fftw3.f03"
then 
	[ echo "    Found fftw3.f03 file in /usr/include. Will use this directory for fortran header files."]
	fftw3_inc_dir="/usr/include"
else
	[ echo "    Cannot find fftw3 fortran headers."]
fi



if test $fftw3_fortran_header = NO
then
	AC_MSG_RESULT([no])
	AC_MSG_ERROR([Cannot find fortran headers for fftw3 -- Quitting!])
else
	AC_MSG_RESULT([yes])
fi
if test $fftw3_mpi_fortran_header = NO
then
	if test $mpi = true
	then
		AC_MSG_ERROR([Cannot find MPI fortran headers, and you've enabled mpi! -- Quitting! ])
	fi
	AC_MSG_WARN([Cannot find MPI fortran headers for fftw3.])
else
	[echo "  (found fortran MPI headers too!) "]
fi
AC_PROG_INSTALL
AC_CONFIG_FILES([ Makefile src/Makefile src/fftwmod.f03 src/mpi/fftwmod-mpi.f03 src/omp/fftwmod-omp.f03 ])

if test $mpi = true
then
	#AC_CONFIG_FILES([src/mpi/Makefile])
	mpi_prog=[ggadt-mpi]
	#[echo "Creating mpi makefile"]
fi
if test $omp = true
then
	#AC_CONFIG_FILES([src/omp/Makefile])
	[echo "Creating opemp makefile"]
	omp_prog=[ggadt-mpi]
fi
AC_SUBST(fftw3_inc_dir)
AC_SUBST(mpi_prog)
AC_SUBST(omp_prog)
AC_OUTPUT
