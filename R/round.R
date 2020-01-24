## exported
roundVersions <- c("sprintf" ## C-versions must end with ".C" -- do *sort* historically:
                 , "r0.C", "r1.C", "r1a.C", "r2.C", "r2a.C", "r3.C", "r3d.C"
                 , "r3")
## hidden:
roundVer.C  <- grep("\\.C$", roundVersions, value=TRUE)
nr.R.ver <- length(roundVersions) - length(roundVer.C)

##' round() with several / all `roundVersions` :
roundAll <- function(x, digits, versions = roundVersions)
   vapply(versions, function(ver) roundX(x, digits, ver),
          ## recycling {x, digits}:
          cbind(x, digits, deparse.level=0L)[,1])


roundX <- function(x, digits, version = roundVersions, trace = 0) {
    version <- match.arg(version)
    if(length(digits) == 0) stop("'digits' is of length 0")
    if (anyNA(digits)) stop("'digits' has NAs")
    if(match(version, roundVersions) <= nr.R.ver) ## have R version;
        digits <- sign(digits) * pmin(9999, abs(floor(digits + 0.5)))
    switch(version,
           "sprintf" = {
               ##' r1(): the version assuming  length(digits) == 1 :
               r1 <- function(x, d) sprintf("%.*f", d, x)
               as.numeric(if(length(digits) == 1) r1(x, digits)
                          else if(length(x) == 1) vapply(digits, r1, x=x, "1")
                          else ## recycle to common length
                              vapply(seq_along({
                                  m <- cbind(x, digits, deparse.level=0L)
                                  x <- m[,1]; d <- m[,2] }),
                                  function(i) r1(x[i], d[i]), "1"))
           },
           "r3" = {
               if(length(digits) == 1) round_r3(x, digits, check=FALSE)
               else if(length(x) == 1) vapply(digits, round_r3, x=x, check=FALSE, 1.5)
               else ## recycle to common length
                   vapply(seq_along({m <- cbind(x, digits, deparse.level=0L)
                       x <- m[,1]; d <- m[,2]}),
                       function(i) round_r3(x[i], d[i], check=FALSE), 1.5)
           },
           ##
           { ## no match above: ==> C versions
               i_ver <- match(version, roundVer.C)
               if(length(i_ver) != 1 || is.na(i_ver))
                   stop(gettextf("invalid version (must be one of %s): %s",
                                 paste(dQuote(roundVersions), collapse=", "), version),
                        domain=NA)
               if(trace) i_ver <- i_ver + 100L*as.integer(abs(trace))
               .Call(round_ver, as.double(x), digits, i_ver) # -> ../src/R2C.c
           })
}

round_r3 <- function(x, d, info=FALSE, check=TRUE) {
    if(check)
        stopifnot(!anyNA(d), length(d) == 1L) # length(x) is arbitrary
    max10e <- 308L # in C, =  (int) DBL_MAX_10_EXP; // == 308 ("IEEE")
    if(d > +max10e + 15L) # assuming DBL_DIG = 15
        return(x)
    else if(d < -max10e)
        return(0. * x)
    ## else  # "regular" digits ---------------------------
    p10 <- 10^d
    x10 <- as.vector(p10*x) # drop attributes for computation
    xd <- (i10 <- floor(x10)) / p10
    xu <-       ceiling(x10)  / p10
    ## should have xd <= x <= xu
    D <- (xu - x) - (x - xd)
    ## D >  0 ==> xu is farther away from x than xd ==> round *down*
    ## D <  0 ==> xd is farther  ..   ..  .. ..  .. ==> round *up*
    ## D == 0 ==> both in *same* distance: round "to even"
    e <- i10 %% 2 # = 1  <==>  i10 is odd <==> "even" is *up*
    r <- x
    i <- (D < 0) | (e & (D == 0)) # round up
    r[ i] <- xu[ i]
    r[!i] <- xd[!i]
    if(info) list(r=r, D=D, e=e) else r
}
