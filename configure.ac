
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
AM_INIT_AUTOMAKE

AC_PROG_FC
AC_CHECK_LIB([m],[cos])
AC_CHECK_LIB(fftw3,fftw_plan_dft_1d,, AC_MSG_ERROR([Missing FFTW library]))
AC_FIND_FILE("fftw3.f03", [/usr/local/include /usr/include], fftw3_fortran_header)
AC_MSG_CHECKING([for Fortran headers for fftw3])
if test $fftw3_fortran_header = NO
then
	AC_MSG_RESULT([no])
	AC_MSG_ERROR([Cannot find fortran headers for fftw3 -- Quitting!])
else
	AC_MSG_RESULT([yes])
fi
AC_PROG_INSTALL
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
