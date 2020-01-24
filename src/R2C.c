#include "round.h"

typedef enum {
    r_0  = 1 // fround0 ()  {simplistic}
    , r_1    // fround1 ();  R's default till R 3.6.x
    , r_1a   // fround1a(): minimally improved fround1()
    , r_2    // fround2 (): Christmas 2019 version [R-devel, svn rev 77618]
    , r_2a   // fround2a(): Epiphany 2020
    , r_3    // fround3 (): translated from R's "r3", Jan.12
    , r_3d   // fround3d(): version w/o 'long double', Jan.13
} round_kind_type;

SEXP round_ver(SEXP x, SEXP digits, SEXP op) {
    int iop = asInteger(op);
    if(iop == NA_INTEGER || iop <= 0)
	error("invalid round_kind/trace, iop=%d", iop);
    round_kind_type i_op = iop % 100;
    int tr = iop / 100; // --> tr = trace_level (where supported)
    int nprot = 1;
    // digits = Inf or similar is allowed in R's round(), and the C codes
    if (TYPEOF(digits) != REALSXP) { PROTECT(digits = coerceVector(digits, REALSXP)); nprot++; }

    R_xlen_t nx = XLENGTH(x), nd = XLENGTH(digits),
	n = (nx == 0 || nd == 0) ? 0 : ( // max(nx, nd) :=
	    nx > nd ? nx : nd);
    SEXP ans = PROTECT(allocVector(REALSXP, n));
    double *x_ = REAL(x),
	*digits_ = REAL(digits),
	*res = REAL(ans);

    switch(i_op) {
    case r_0 : for(R_xlen_t i=0; i < n; i++) res[i] = fround0 (x_[i % nx], digits_[i % nd]); break;
    case r_1 : for(R_xlen_t i=0; i < n; i++) res[i] = fround1 (x_[i % nx], digits_[i % nd]); break;
    case r_1a: for(R_xlen_t i=0; i < n; i++) res[i] = fround1a(x_[i % nx], digits_[i % nd]); break;
    case r_2 : for(R_xlen_t i=0; i < n; i++) res[i] = fround2 (x_[i % nx], digits_[i % nd]); break;
    case r_2a: for(R_xlen_t i=0; i < n; i++) res[i] = fround2a(x_[i % nx], digits_[i % nd]); break;
    case r_3 : for(R_xlen_t i=0; i < n; i++) res[i] = fround3 (x_[i % nx], digits_[i % nd]); break;
    case r_3d: for(R_xlen_t i=0; i < n; i++) res[i] = fround3d(x_[i % nx], digits_[i % nd], tr); break;
    default : error("invalid round_kind, integer code = %d", i_op);
    }

    UNPROTECT(nprot);
    return ans;
}
