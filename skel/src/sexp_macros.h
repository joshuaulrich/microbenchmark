/*
 * sexp_macros.h - helper macros for SEXPs
 *
 * Collection of useful macros to handle S expressions. Most of these
 * are used to unpack arguments passed in via the .Call() or
 * .External() interface.
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 */

#if !defined(__SEXP_MACROS_H__)
#define __SEXP_MACROS_H__

#include <R.h>
#include <Rinternals.h>

#define CHECK_ARG_IS_REAL_MATRIX(A)					\
    if (!isReal(A) || !isMatrix(A))					\
	error("Argument '" #A "' is not a real matrix.");

#define CHECK_ARG_IS_REAL_VECTOR(A)					\
    if (!isReal(A) || !isVector(A))					\
	error("Argument '" #A "' is not a real vector.");

#define CHECK_ARG_IS_INT_VECTOR(A)					\
    if (!isInteger(A) || !isVector(A))					\
	error("Argument '" #A "' is not an integer vector.");

/*
 * Unpack a real matrix stored in SEXP S. 
 */
#define UNPACK_REAL_MATRIX(S, D, N, K)          \
    CHECK_ARG_IS_REAL_MATRIX(S);		\
    double *D = REAL(S);			\
    const R_len_t N = nrows(S);			\
    const R_len_t K = ncols(S);

/*
 * Unpack a real vector stored in SEXP S.
 */
#define UNPACK_REAL_VECTOR(S, D, N)             \
    CHECK_ARG_IS_REAL_VECTOR(S);		\
    double *D = REAL(S);			\
    const R_len_t N = length(S);                   

/*
 * Unpack a single real stored in SEXP S.
 */
#define UNPACK_REAL(S, D)			\
    CHECK_ARG_IS_REAL_VECTOR(S);		\
    double D = REAL(S)[0];			\

/*
 * Unpack an integer vector stored in SEXP S.
 */
#define UNPACK_INT_VECTOR(S, I, N)             \
    CHECK_ARG_IS_INT_VECTOR(S);		       \
    int *I = INTEGER(S);		       \
    const R_len_t N = length(S);                   

/*
 * Unpack a single integer stored in SEXP S.
 */
#define UNPACK_INT(S, I)			\
    CHECK_ARG_IS_INT_VECTOR(S);			\
    int I = INTEGER(S)[0];			\

#endif
