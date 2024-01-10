## ----build-info, eval=FALSE, echo=FALSE---------------------------------------
#    setwd("~/R/Pkgs/round/vignettes")
#    rmarkdown::render("rationalRound.Rmd", clean=FALSE)

## ----simple-------------------------------------------------------------------
d1 <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9)
fivers <- list(
    d1 =    .5
  , d2 = c( .05, .15,  .25,  .35,  .45,  .55,  .65,  .75,  .85,  .95)
  , d3 = c(.005, .015, .025, .035, .045, .055, .065, .075, .085, .095,
           .105, .115, .125, .135, .145, .155, .165, .175, .185, .195,
           .205, .215, .225, .235, .245, .255, .265, .275, .285, .295,
           .305, .315, .325, .335, .345, .355, .365, .375, .385, .395,
           .405, .415, .425, .435, .445, .455, .465, .475, .485, .495,
           .505, .515, .525, .535, .545, .555, .565, .575, .585, .595,
           .605, .615, .625, .635, .645, .655, .665, .675, .685, .695,
           .705, .715, .725, .735, .745, .755, .765, .775, .785, .795,
           .805, .815, .825, .835, .845, .855, .865, .875, .885, .895,
           .905, .915, .925, .935, .945, .955, .965, .975, .985, .995)
    )

## ----q-fivers-----------------------------------------------------------------
require(gmp)
q5er <- lapply(fivers, as.bigq)
q5er
as.bigz(2)^60 == max(denominator(q5er$d3))

## ----round0-roundQ------------------------------------------------------------
if(print(packageVersion("gmp")) < "0.6-1") {
    ## From 'gmp's namespace, usually "hidden", needed here :
    is.whole.bigq  <- gmp:::is.whole.bigq
    biginteger_mod <- gmp:::biginteger_mod
    .mod.bigz <- function(e1, e2) .Call(biginteger_mod, e1, e2)

  ##' rounding to integer a la "nearbyint()" -- i.e. "round to even"
  round0 <- function(x) {
      nU <- as.bigz.bigq(xU <- x + as.bigq(1, 2)) # traditional round: .5 rounded up
      if(any(I <- is.whole.bigq(xU))) { # I <==>  x == <n>.5 : "hard case"
          I[I] <- .mod.bigz(nU[I], 2L) == 1L # rounded up is odd  ==> round *down*
          nU[I] <- nU[I] - 1L
      }
      nU
  }

  roundQ <- function(x, digits = 0, r0 = round0) {
      ## round(x * 10^d) / 10^d
      p10 <- as.bigz(10) ^ digits
      r0(x * p10) / p10
  }

  ##' round() method ==>  arguments = (x, digits)
  round.bigq <- function(x, digits = 0) roundQ(x, digits)
  .S3method("round","bigq", round.bigq)

} else ## all the above is part of gmp >= 0.6-1 :
  withAutoprint({
    round0
    roundQ
    gmp:::round.bigq
  })

## "simple round" was used in round.bigq() for several months in 2020:
round0s <- function(x) as.bigz.bigq(x + as.bigq(1, 2))

## ----round-x-2----------------------------------------------------------------
round(q5er$d2, 1)
round(q5er$d3, 2)

## ----roundAll-x-1-------------------------------------------------------------
require(round)

roundAllX <- function(x, digits, versions = roundVersions) {
    xQ <- as.bigq(x)
    M <- cbind(roundAll(x, digits, versions=versions)
             , SxctQ = asNumeric(roundQ(xQ, digits, r0=round0s))
             ,  xctQ = asNumeric(roundQ(xQ, digits))
               )
    rownames(M) <- format(x)
    M
}

(rA1 <- roundAllX(fivers$d2, 1))

## ----roundAll-x-2-------------------------------------------------------------
(rA2 <- roundAllX(fivers$d3, 2))

## ----eqAll-1------------------------------------------------------------------
symnum(rA1 == rA1[,"xctQ"])

## ----eqAll-2------------------------------------------------------------------
symnum(rA2 == rA2[,"xctQ"])

## ----echo=FALSE---------------------------------------------------------------
print(sessionInfo(), locale=FALSE)

