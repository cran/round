

#include <R.h>
#include <Rinternals.h>
//-> XLENGTH, R_xlen_t

#include <Rmath.h>
#include <R_ext/Arith.h>

//--------------- from src/nmath/nmath.h -----------------------

// unconditionally, must work:
#define LDOUBLE long double

#define ML_POSINF	R_PosInf
#define ML_NEGINF	R_NegInf
#define ML_NAN		R_NaN


// ./mathC99.c :
SEXP  logb_R(SEXP x);
SEXP ilogb_R(SEXP x);
SEXP nearbyint_R(SEXP x);

SEXP fpclassify_R(SEXP x);
SEXP isnormal_R(SEXP x);
SEXP signbit_R(SEXP x);

SEXP nextafter_R (SEXP from, SEXP to);
SEXP nexttoward_R(SEXP from, SEXP to);


// ./R2C.c :
SEXP round_ver(SEXP x, SEXP digits, SEXP op);

// ./fround.c :

double fround0(double x, double digits); /* simplified version fround1(), as proposed by
					  * Adam Wheeler in R's bugzilla PR#17668
					  * https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17668
					  */

double fround1(double x, double digits); // as it was in R 3.6.x

double fround1a(double x, double digits); // slightly improved fround1() -- any difference ???

double fround2(double x, double digits);/* my version as committed to R-devel, svn r77618 , */
/* ------------------------------------------------------------
   r77618 | maechler | 2019-12-24 16:11:50 +0100 (Tue, 24. Dec 2019)

   M src/nmath/fround.c
   M tests/reg-tests-1d.R

   tweaks to round() bug fix for PR#17668 in r77609
   ---------------------------------------------------------------*/

double fround2a(double x, double digits); // my version 2020-01-06 ...

double fround3 (double x, double digits);
double fround3d(double x, double digits, int trace_lev);
