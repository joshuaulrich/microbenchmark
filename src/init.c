#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP do_get_nanotime(void);
extern SEXP do_microtiming(SEXP, SEXP, SEXP, SEXP);
extern SEXP do_microtiming_precision(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"do_get_nanotime",          (DL_FUNC) &do_get_nanotime,          0},
    {"do_microtiming",           (DL_FUNC) &do_microtiming,           4},
    {"do_microtiming_precision", (DL_FUNC) &do_microtiming_precision, 3},
    {NULL, NULL, 0}
};

void R_init_microbenchmark(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
