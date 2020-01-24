/*
  Copyright (C) 2020 Martin Maechler

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
  for more details.

  Notably, see ../LICENSE
*/

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "round.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {
    CALLDEF(round_ver, 3),

    CALLDEF(ilogb_R, 1),
    CALLDEF(logb_R, 1),
    CALLDEF(nearbyint_R, 1),
    CALLDEF(fpclassify_R, 1),
    CALLDEF(isnormal_R, 1),
    CALLDEF(signbit_R, 1),

    CALLDEF(nextafter_R, 2),
    CALLDEF(nexttoward_R, 2),

    {NULL, NULL, 0}
};

/**
 * register routines
 * @param dll pointer
 * @return none
 * @author Martin Maechler
 */
void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_round(DllInfo *dll)
{
    R_registerRoutines(dll, NULL /* CEntries*/, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


