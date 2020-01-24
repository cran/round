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
 *    Provide R interfaces to  C99  libmath (<math.h>) functions not yet in R
 */

#include "round.h"
//	  -->  <Rmath.h>  -->  <math.h>
#include "R2C.h"

R2C_1(logb)
R2C_1_DI(ilogb)

R2C_1(nearbyint)

R2C_1_DI(fpclassify)
/* Could also have a "character" returning function starting from
#include <float.h>

const char *show_classification(double x) {
    switch(fpclassify(x)) {
        case FP_INFINITE:  return "Inf";
        case FP_NAN:       return "NaN";
        case FP_NORMAL:    return "normal";
        case FP_SUBNORMAL: return "subnormal";
        case FP_ZERO:      return "zero";
        default:           return "unknown";
    }
}
*/

// R2C_1_DI(isfinite)  -- R's  is.finite()
// R2C_1_DI(isinf)     -- R's  is.infinite()
// R2C_1_DI(isnan)     -- R's  is.na() [! not is.nan() !]
R2C_1_DI(isnormal)
R2C_1_DI(signbit)


R2C_2(nextafter)
R2C_2(nexttoward)

