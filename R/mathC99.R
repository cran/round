## logb() in R is  log(x, base)
##' binary integer log, i.e., the power 'P' of 2 when   x == m * 2^P
logB       <- function(x) .Call(logb_R, x)
ilogb      <- function(x) .Call(ilogb_R, x)

nearbyint  <- function(x) .Call(nearbyint_R, x)

fpclassify <- function(x) .Call(fpclassify_R, x)
isnormal   <- function(x) .Call(isnormal_R, x)
signbit    <- function(x) .Call(signbit_R, x)

nextafter  <- function(x, y) .Call(nextafter_R, x, y)
nexttoward <- function(x, y) .Call(nexttoward_R, x, y)
