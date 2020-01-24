/*
    R package 'round' -- Versions of round(*, digits)
    Copyright (C) 2020  Martin Maechler

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  DESCRIPTION
 *
 *    Provide  R -> C  interface macros
 */


// Macro for *vectorized* .Call() function of 1 argument -  {double} -> {double}

#define R2C_1(_NM_)						\
SEXP _NM_ ## _R(SEXP x) {					\
    int nprot = 1;						\
    if (TYPEOF(x) != REALSXP) {					\
        PROTECT(x = coerceVector(x, REALSXP)); nprot++; }	\
								\
    R_xlen_t n = XLENGTH(x);					\
    SEXP ans = PROTECT(allocVector(REALSXP, n));		\
    double *x_ = REAL(x), *res = REAL(ans);			\
								\
    for(R_xlen_t i=0; i < n; i++)				\
	res[i] = _NM_(x_[i]);					\
								\
    UNPROTECT(nprot);						\
    return ans;							\
}

// Macro for *vectorized* .Call() function of 1 argument -  {double} -> {int}

#define R2C_1_DI(_NM_)						\
SEXP _NM_ ## _R(SEXP x) {					\
    int nprot = 1;						\
    if (TYPEOF(x) != REALSXP) {					\
        PROTECT(x = coerceVector(x, REALSXP)); nprot++; }	\
								\
    R_xlen_t n = XLENGTH(x);					\
    SEXP ans = PROTECT(allocVector(INTSXP, n));			\
    double *x_ = REAL(x);					\
    int *res = INTEGER(ans);					\
								\
    for(R_xlen_t i=0; i < n; i++)				\
	res[i] = _NM_(x_[i]);					\
								\
    UNPROTECT(nprot);						\
    return ans;							\
}

// Macro for *vectorized* .Call() function of 2 arguments -  {double}x{double} -> {double}

#define R2C_2(_NM_)						\
SEXP _NM_ ## _R(SEXP x, SEXP y) {				\
    int nprot = 1;						\
    if (TYPEOF(x) != REALSXP) {					\
        PROTECT(x = coerceVector(x, REALSXP)); nprot++; }	\
    if (TYPEOF(y) != REALSXP) {					\
        PROTECT(y = coerceVector(y, REALSXP)); nprot++; }	\
								\
    R_xlen_t nx = XLENGTH(x), ny = XLENGTH(y),			\
	n = (nx == 0 || ny == 0) ? 0 : ( /* max(nx, ny) := */	\
	    nx > ny ? nx : ny);					\
    SEXP ans = PROTECT(allocVector(REALSXP, n));		\
    double *x_ = REAL(x), *y_ = REAL(y), *res = REAL(ans);	\
								\
    for(R_xlen_t i=0; i < n; i++)				\
	res[i] = _NM_(x_[i % nx], y_[i % ny]);			\
								\
    UNPROTECT(nprot);						\
    return ans;							\
}

