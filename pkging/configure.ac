# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.60)
AC_INIT([tea], [X.XX], [ben.klemens@census.gov])


# The normal /usr/local default just confused too many people
AC_PREFIX_DEFAULT([/usr])


#Cut & pasted from the R documentation. Uses R's preferred compiler
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
echo "could not determine R_HOME"
exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS="`"${R_HOME}/bin/R" CMD config CFLAGS` -O0"
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`


##AC_PROG_CC
########AC_PROG_LIBTOOL


# Windows:
##########AC_LIBTOOL_WIN32_DLL

stop=0
# Checks for libraries.
AC_CHECK_LIB([m],[cos])
libm=$ac_status

AC_CHECK_LIB([gslcblas],[cblas_dgemm])
AC_CHECK_LIB([gsl],[gsl_blas_dgemm])
libgsl=$ac_status

#AC_CHECK_LIB([mysqlclient], [mysql_query], , , [-L/usr/lib/mysql -lz])
AC_CHECK_LIB([sqlite3], [sqlite3_exec])
AC_CHECK_LIB([apophenia],[apop_data_alloc_base])
libapop=$ac_status
echo "-----------------------------------------------------------"

if test $libm -gt 0
 then echo "I can't find the standard math library. Panic."
 stop=1
fi

if test $libgsl -gt 0
 then echo "I can't find the GNU Scientific library. When installing, you will probably also need to include the GSL BLAS package."
 stop=1
fi

if test $libapop -gt 0
 then echo "I can't find the Apophenia library. Go to http://apophenia.info and download it."
 stop=1
fi

if test $stop -eq 0
then 
	echo "Check for GNU Scientific, SQLite3, and Apophenia libraries went OK." 
else 
	echo "Missing C libraries. Please install them and try again."
	echo "Is your LIBRARY_PATH or other such environment variable set correctly?"
 exit 1
fi
echo "-----------------------------------------------------------"





# Checks for header files.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AC_CHECK_HEADERS([float.h inttypes.h stdlib.h string.h unistd.h wchar.h])
AC_CHECK_HEADER([gsl/gsl_rng.h], , [echo -e "I couldn't find the GSL headers.\nIf it is in a nonstandard place, is your INCLUDE_PATH or CPATH right?\nSome systems break the library into a runtime package, like libgsl, and compile-time package, like libgsl-devel. Make sure both are installed."; exit 1])
AC_CHECK_HEADER([sqlite3.h], , [echo -e "I couldn't find the sqlite3 header.\nIf it is in a nonstandard place, is your INCLUDE_PATH or CPATH right?\nSome systems break the library into a runtime package, like libsqlite3, and compile-time package, like libsqlite3-devel. Make sure both are installed."; exit 1])
AC_CHECK_HEADER([apop.h], , [echo -e "I couldn't find the headers for Apophenia.\nIf it is in a nonstandard place, is your INCLUDE_PATH or CPATH right?"; exit 1])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([floor memset pow regcomp sqrt strcasecmp])

PACKAGE_NAME=tea

echo "PKG_LIBS=@LIBS@" > src/Makevars.in
AC_CONFIG_FILES([src/Makevars],)

AC_OUTPUT
rm src/Makevars.in
