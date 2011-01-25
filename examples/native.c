#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

SEXP do_nothing(SEXP s_in) {
    return s_in;
}
