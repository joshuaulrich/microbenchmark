#include <stdint.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Memory.h>

#include "sexp_macros.h"

/* FIXME: Find portable 64 bit integer type. */
typedef uint64_t nanotime_t;

#if defined(WIN32) 
  #include "nanotimer_windows.h"
#elif defined(__MACH__) || defined(__APPLE__)
  #include "nanotimer_macosx.h"
#elif defined(linux) || defined(__linux)
  #include "nanotimer_linux.h"
#elif defined(sun) || defined(__sun) || defined(_AIX)
  #include "nanotimer_rtposix.h"
#else /* Unsupported OS */
  #error "Unsupported OS."
#endif

#if defined(__GNUC__)
  #define NOINLINE  __attribute__((noinline))
#else
  #define NOINLINE
#endif

SEXP do_nothing(SEXP a, SEXP b) NOINLINE;

SEXP do_nothing(SEXP a, SEXP b) {
    return a;
}

SEXP do_microtiming(SEXP s_times, SEXP s_expr, SEXP s_rho) {
    nanotime_t start, end, overhead;
    int i, n_under_overhead = 0;
    SEXP s_ret;
    double *ret;
    volatile SEXP s_tmp;

    UNPACK_INT(s_times, times);
    if(!isEnvironment(s_rho)) 
        error("'s_rho' should be an environment");

    PROTECT(s_ret = allocVector(REALSXP, times));
    ret = REAL(s_ret);

    /* Estimate minimal overhead and warm up the machine ... */
    overhead = 1 << 31;
    for (i = 0; i < 1 << 18; ++i) {
        start = get_nanotime();
        s_tmp = do_nothing(s_expr, s_rho);
        end = get_nanotime();

        const nanotime_t diff = end - start;
        if (diff > 0 && diff < overhead)
            overhead = diff;
    }

    /* Actual timing... */
    for (i = 0; i < times; ++i) {
        start = get_nanotime();
        s_tmp = eval(s_expr, s_rho);
        end = get_nanotime();
        
        const nanotime_t diff = end - start;
        if (diff < overhead) {
            ret[i] = R_NaReal;
            n_under_overhead++;
        } else {
            ret[i] = diff - overhead;
        }

        /* Housekeeping */
        R_CheckUserInterrupt();
        /* R_gc(); */
    }

    /* Issue waring if we observed some timings below the estimated
     * overhead.
     */
    if (n_under_overhead > 0) {
        if (n_under_overhead == 1) {
            warning("Estimated overhead was greater than measured evaluation time inr 1 run.");
        } else {
            warning("Estimated overhead was greater than measured evaluation time in %i runs.", 
                    n_under_overhead);
        }
    }

    UNPROTECT(1); /* s_ret */
    return s_ret;
}
