

#include <R.h>
#include <Rinternals.h>
//-> XLENGTH, R_xlen_t

#include <Rmath.h>
#include <R_ext/Arith.h>

//--------------- from src/nmath/nmath.h -----------------------


/* Required by C99, but might be slow */
#define HAVE_LONG_DOUBLE
// Should replace by creating a  '../configure'  which checks
// if (sizeof(long double) > sizeof(double))

#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

//  partly inspired by  Pkgs/DPQ/src/DPQpkg.h :
#ifdef HAVE_LONG_DOUBLE
# define EXP   expl
# define EXPm1 expm1l
# define FABS  fabsl
# define LOG   logl
# define LOG1p log1pl
# define POW   powl
# define CEIL  ceill
# define FLOOR floorl
# define FMOD  fmodl
# define N_10  10.L
# define N_2    2.L

#else //-------------------- no long double

# define EXP   exp
# define EXPm1 expm1
# define FABS  fabs
# define LOG   log
# define LOG1p log1p
# define POW   pow
# define CEIL  ceil
# define FLOOR floor
# define FMOD  fmod
# define N_10  10.
# define N_2    2.

#endif
//----------------------- LONG_DOUBLE -- LDOUBLE ----------------


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
