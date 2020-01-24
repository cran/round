/*
 *  Copyright (C) 2020  Martin Maechler
 *
 *  Originates from R's source  R/src/nmath/fround.c  part of
 *  Mathlib : A C Library of Special Functions

 *  Copyright (C) 2000-2019 The R Core Team
 *  Copyright (C) 1998 Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  DESCRIPTION
 *
 *    Rounds "x" to "digits" decimal digits.
 */

/* possibly needed for debugging */
#include <R_ext/Print.h>

#include "round.h"

const static int max10e = (int) DBL_MAX_10_EXP; // == 308 ("IEEE")

// used in fround0() and fround1() only
#define MAX_DIGITS_ DBL_MAX_10_EXP
    /* = 308 (IEEE); was till R 0.99: (DBL_DIG - 1) */
    /* Note that large digits make sense for very small numbers */

// *larger* version used in newer  fround1a()  etc :
#define MAX_DIGITS (DBL_MAX_10_EXP + DBL_DIG)
    /* was DBL_MAX_10_EXP (= 308, IEEE) till R 3.6.x; before,
       was (DBL_DIG - 1)  till R 0.99  */



// The version as it was in R 3.6.x ((and basically earlier R versions)
double fround1(double x, double digits) {
    LDOUBLE pow10, sgn, intx;
    int dig;

    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if(digits == ML_POSINF) return x;
    else if(digits == ML_NEGINF) return 0.0;

    if (digits > MAX_DIGITS_) digits = MAX_DIGITS_;
    dig = (int)floor(digits + 0.5);
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } else
	sgn = 1.;
    if (dig == 0) {
	return (double)(sgn * nearbyint(x));
    } else if (dig > 0) {
        pow10 = R_pow_di(10., dig);
	intx = floor(x);
	return (double)(sgn * (intx + nearbyint((double)((x-intx) * pow10)) / pow10));
    } else {
        pow10 = R_pow_di(10., -dig);
        return (double)(sgn * nearbyint((double)(x/pow10)) * pow10);
    }
}

// Slightly improved version of fround1(): larger MAX_DIGITS
double fround1a(double x, double digits) {
    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits == ML_NEGINF)
	return 0.0;
    else if(digits > DBL_MAX_10_EXP)
    	    digits = DBL_MAX_10_EXP;
    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    if (dig == 0) {
	return sgn * nearbyint(x);
    } else {
	LDOUBLE pow10;
	if (dig > 0) {
	    pow10 = R_pow_di(10., dig);
	    LDOUBLE intx = floor(x);
	    return (double)(sgn * (intx + nearbyint((double)((x-intx) * pow10)) / pow10));
	} else {
	    pow10 = R_pow_di(10., -dig);
	    return (double)(sgn * nearbyint((double)(x/pow10)) * pow10);
	}
    }
}

/* A version as it was shortly in R-devel (svn r77609): a *simplified* (and not quite correct!)
 * version of fround1(),   *not* subtracting (and re-adding) the  integer part  'intx' */
double fround0(double x, double digits) {

    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if(digits == ML_POSINF) return x;
    else if(digits == ML_NEGINF) return 0.0;

    if (digits > MAX_DIGITS_) digits = MAX_DIGITS_;

    int dig = (int)floor(digits + 0.5);
    LDOUBLE sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    }
    if (dig == 0) {
	return (double)(sgn * nearbyint(x));
    } else if (dig > 0) {
	LDOUBLE pow10 = R_pow_di(10., dig);
	return (double)(sgn * (nearbyint((double)(x * pow10)) / pow10));
    } else {
	LDOUBLE pow10 = R_pow_di(10., -dig);
        return (double)(sgn * nearbyint((double)(x/pow10)) * pow10);
    }
}



// my version as committed to R-devel, svn r77618 :
double fround2(double x, double digits) {

    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits == ML_NEGINF) return 0.0;

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    if (dig == 0) {
	return sgn * nearbyint(x);
    } else if (dig > 0) {
	double l10x = log10(x);
	if(l10x + dig > DBL_DIG) { // rounding to so many digits that no rounding is needed
	    return sgn * x;
	} else if (dig <= DBL_MAX_10_EXP) { // both pow10 := 10^d and (x * pow10) do *not* overflow
	    LDOUBLE pow10 = R_pow_di(10., dig);
	    return sgn *  (double)(nearbyint((double)(x * pow10)) / pow10);
	} else { // DBL_MAX_10_EXP < dig <= DBL_DIG - log10(x) : case of |x| << 1; ~ 10^-305
	    int e10 = dig - max10e; // > 0
	    LDOUBLE p10 = R_pow_di(10., e10),
		  pow10 = R_pow_di(10., max10e);
	    return  sgn * (double) (nearbyint((double)((x*pow10)*p10))/pow10/p10);
	}
    } else {
	LDOUBLE pow10 = R_pow_di(10., -dig);
        return sgn *  (double) (nearbyint((double)(x/pow10)) * pow10);
    }
}

/* my version as of Jan.6, 2020 (to be committed to R-devel ?) :
 * mostly improving  dig < 0 cases */
double fround2a(double x, double digits) {

    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits < -max10e) // includes -Inf {aka ML_NEGINF}
	return 0.;
    else if (digits == 0.) // common
	return nearbyint(x);

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    double l10x = M_LOG10_2*(0.5 + logb(x)); // ~= log10(x), but cheaper (presumably)
    if(l10x + dig > DBL_DIG) // rounding to so many digits that no rounding is needed
	return sgn * x;
    if (dig > 0) {
	if (dig <= DBL_MAX_10_EXP) { // both pow10 := 10^d and (x * pow10) do *not* overflow
	    LDOUBLE pow10 = R_pow_di(10., dig);
	    return sgn * (double)(nearbyint((double)(x * pow10)) / pow10);
	} else { /* DBL_MAX_10_EXP =: max10e < dig <= DBL_DIG - log10(x):
		    case of |x| << 1; ~ 10^-305 */
	    int e10 = dig - max10e; // > 0
	    LDOUBLE p10 = R_pow_di(10., e10),
		  pow10 = R_pow_di(10., max10e);
	    return sgn * (double) (nearbyint((double)((x*pow10)*p10))/pow10/p10);
	}
    } else { // -max10e <= dig < 0
	LDOUBLE pow10 = R_pow_di(10., -dig); // >= 10
	return sgn * (double) (nearbyint((double)(x/pow10)) * pow10);
    }
}

// "r3.C": Translation of "r3" from R to C
double fround3(double x, double digits) {

    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits < -max10e) // includes -Inf {aka ML_NEGINF}
	return 0.;
    else if (digits == 0.) // common
	return nearbyint(x);

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    double l10x = M_LOG10_2*(0.5 + logb(x)); // ~= log10(x), but cheaper (presumably)
    if(l10x + dig > DBL_DIG) // rounding to so many digits that no rounding is needed
	return sgn * x;

    // using long double : allows also "large" dig  where  10^dig  would overflow in double prec
    long double
	pow10 = powl(10.L, (long double) dig),
	x10 = x * pow10,
	i10 = floorl(x10);
    // Rboolean is_odd_i10 = (fmodl(i10, 2.L) == 1);
    double
	xd = (double) (    i10     / pow10),
	xu = (double) (ceill (x10) / pow10);
    long double
	du = ((long double)xu) - (long double)x,
	dd = ((long double)x ) - (long double)xd;
    //  D =  du - dd
    //  return sgn * ((D < 0 || (is_odd_i10 && D == 0)) ? xu : xd);
    return sgn * ((du < dd || (fmodl(i10, 2.L) == 1 && du == dd)) ? xu : xd);
}

// "r3d.C": Translation of "r3" from R to C -- *not* using 'long double'
double fround3d(double x, double digits, int trace_lev) {

    if(trace_lev) REprintf("fround3d(%.15g, digits=%.0f):\n", x, digits);
    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits < -max10e) // includes -Inf {aka ML_NEGINF}
	return 0.;
    else if (digits == 0.) // common
	return nearbyint(x);

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    double l10x = M_LOG10_2*(0.5 + logb(x)); // ~= log10(x), but cheaper (presumably)
    if(l10x + dig > DBL_DIG) { // rounding to so many digits that no rounding is needed
	if(trace_lev)
	    REprintf(" + l10x + dig > DBL_DIG ( = %d): returning x\n", DBL_DIG);
	return sgn * x;
    } else {
	double pow10, x10, i10,
	    xd, xu; // x, rounded _d_own or _u_p
	if(trace_lev) REprintf(" + l10x=%g, dig=%d, sign=%.0f\n", l10x, dig, sgn);
	if (dig <= max10e) { // both pow10 := 10^d and x10 := x * pow10 do *not* overflow
	    pow10 = R_pow_di(10., dig);
	    x10 = x * pow10;
	    i10 = floor(x10);
	    xd =    i10     / pow10;
	    xu = ceil (x10) / pow10;
	    if(trace_lev) REprintf(" + dig <= %d: ", max10e);
	} else { // DBL_MAX_10_EXP =: max10e < dig <= DBL_DIG - l10x: case of |x| << 1; ~ 10^-305
	    int e10 = dig - max10e; // > 0
	    double
		p10 = R_pow_di(10., e10);
	    pow10   = R_pow_di(10., max10e);
	    x10 = (x * pow10) * p10;
	    i10 = floor(x10);
	    xd =    i10     / pow10 / p10;
	    xu = ceil (x10) / pow10 / p10;
	    if(trace_lev) REprintf(" + dig > %d: e10=%d, p10=%g", max10e, e10, p10);
	}
	if(trace_lev)
	    REprintf(" pow10=%g, x10=%g, i10=%g ==> (xd, xu) = (%.15g,%.15g)\n",
		     pow10, x10, i10, xd, xu);
	double
	    du = xu - x,
	    dd = x  - xd;
	//  D =  du - dd
	//  return sgn * ((D < 0 || (is_odd_i10 && D == 0)) ? xu : xd);

	// FIXME: Remove, once windows uses a more modern toolchain (?)
	// Use intermediate var so compiler (@ 32-bit windows) does not optimize wrongly:
	int take_xu = du < dd || (fmod(i10, 2.) == 1 && du == dd);
#ifdef _WIN32 // fake some action on take_xu
	take_xu += (take_xu - take_xu)*123 - (int)(i10 - i10);
	if(trace_lev) REprintf("on WIN32_ fake action on take_xu ==> %d\n", take_xu);
#endif
	if(trace_lev)
	    REprintf(" + d{u,d} = %.15g,%.15g; i10 is %s ==> choosing %s\n", du, dd,
		     (fmod(i10, 2.) == 1) ? "odd" : "even", take_xu ? "xu" : "xd");
	return sgn * (take_xu ? xu : xd);
    }
}
