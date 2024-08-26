#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([microbenchmark], [VERSION], [https://github.com/joshuaulrich/microbenchmark/issues])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

AC_CONFIG_SRCDIR([src/init.c])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([src/Makevars])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stdint.h stdlib.h sys/time.h])

# Checks for typedefs, structures, and compiler characteristics.
## nanotime_t is a 64-bit unsigned int
AC_TYPE_UINT64_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE

mb_cv_have_timer="no"

## mach_absolute_time (macOS)
if test "${mb_cv_have_timer}" = "no"; then
  AC_MSG_CHECKING(for mach_absolute_time)
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    #include <mach/mach_time.h>
  ]], [[
    uint64_t time;
    mach_timebase_info_data_t info;
    time = mach_absolute_time();
    mach_timebase_info(&info);
    time * (info.numer / info.denom);
  ]])],[mb_cv_have_timer="yes"],[])
  AC_MSG_RESULT(${mb_cv_have_timer})
  if test "${mb_cv_have_timer}" = "yes"; then
    AC_DEFINE_UNQUOTED([MB_HAVE_MACH_TIME], [1],
      [Define to 1 if you have the `mach_absolute_time` function.])
  fi
fi

# clock_gettime
if test "${mb_cv_have_timer}" = "no"; then

  # clock_gettime is included in glibc > 2.16, and librt before
  libs="${LIBS}"
  AC_SEARCH_LIBS(clock_gettime, rt)

  # we have clock_gettime, now check for best clockid_t
  if test "${ac_cv_search_clock_gettime}" != "no"; then
    AC_CACHE_CHECK([for best clockid_t to use with clock_gettime],
      mb_cv_clockid_t,
    [
      for clockid in CLOCK_MONOTONIC_PRECISE CLOCK_MONOTONIC_RAW \
                     CLOCK_HIGHRES CLOCK_MONOTONIC; do
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <time.h>
        ]], [[
          struct timespec ts;
          clock_gettime($clockid, &ts);
        ]])],[mb_cv_clockid_t="${clockid}"],[mb_cv_clockid_t="no"])
        test "${mb_cv_clockid_t}" = "no" || break
      done
    ])

    LIBS="${libs}"
  fi

  if test "${mb_cv_clockid_t}" = "no"; then
    AC_MSG_WARN(cannot find a monotonic clockid_t to use with clock_gettime; an alternative clock will be used)
  else
    AC_DEFINE([MB_HAVE_CLOCK_GETTIME], [1],
      [Define to 1 if you have the `clock_gettime` function.])
    AC_DEFINE_UNQUOTED([MB_CLOCKID_T], [$mb_cv_clockid_t],
      [clockid_t to use with clock_gettime])
    if test "${mb_cv_clockid_t}" = "CLOCK_MONOTONIC_PRECISE"; then
        # CLOCK_MONOTONIC_PRECISE is FreeBSD-specific that requires __BSD_VISIBLE to use
        AC_DEFINE([__BSD_VISIBLE], [1], [Enable FreeBSD-specific clockid])
    fi
    if test "${ac_cv_search_clock_gettime}" != "none required"; then
      AC_SUBST(MB_LIBS, $ac_cv_search_clock_gettime)
    fi
    mb_cv_have_timer="yes"
  fi
fi

# gethrtime
if test "${mb_cv_have_timer}" = "no"; then
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    #if defined(sun) || defined(__sun)
    #define __EXTENSIONS__
    #endif

    #include <sys/time.h>
    #include <time.h>
  ]], [[
    gethrtime();
  ]])],[mb_cv_gethrtime="yes"],[mb_cv_gethrtime="no"])

  if test "${mb_cv_gethrtime}" = "yes"; then
    AC_DEFINE_UNQUOTED([MB_HAVE_GETHRTIME], [1],
      [Define to 1 if you have the `gethrtime` function.])
    mb_cv_have_timer="yes"
  fi
fi

# gettimeofday
if test "${mb_cv_have_timer}" = "no"; then
  AC_MSG_WARN(cannot find a high-resolution timer. Falling back to system time, which is unreliable for benchmarks.)
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
    #include <sys/time.h>
  ]], [[
    struct timeval tv;
    gettimeofday(&tv, NULL);
  ]])],[mb_cv_gettimeofday="yes"],[mb_cv_gettimeofday="no"])

  if test "${mb_cv_func_gettimeofday}" = "yes"; then
    AC_DEFINE_UNQUOTED([MB_HAVE_GETTIMEOFDAY], [1],
      [Define to 1 if you have the `gettimeofday` function.])
    mb_cv_have_timer="yes"
  fi
fi

# nothing
if test "${mb_cv_have_timer}" = "no"; then
  AC_MSG_ERROR(cannot find a valid timing function.)
fi

AC_OUTPUT
