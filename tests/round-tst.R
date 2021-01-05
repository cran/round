library(round)

## Utilities (sync with ../man/roundX.Rd ! )

##' maybe add to  package -- it's really useful here :
formatF <- function(x, ...) noquote(format(x, scientific=FALSE, drop0trailing=TRUE, ...))

same_cols <- function(m) all(m == m[,1])
matPm <- function(y) {
   matplot(y=y, type = "l", lwd = 2, xlab = "i", ylab = deparse(substitute(y)))
   abline(h = 0, lty=2, col="gray")
   legend("topleft", legend = setdiff(roundVersions, "r3"),
          col = 1:6, lty = 1:5, lwd = 2, bty = "n")
}

str(.M <- .Machine, digits = 8)
capabilities()
str(.M[grep("^sizeof", names(.M))]) ## also differentiate long-double..
(b64 <- .M$sizeof.pointer == 8)
(arch <- Sys.info()[["machine"]])
(onWindows <- .Platform$OS.type == "windows")
(hasLD <- unname(capabilities("long.double")))
win32 <- onWindows && !b64
doExtras <- round:::doExtras()

if(!dev.interactive(orNone=TRUE)) pdf("round-tst.pdf")
options(width = 150) # for wide "tables"

dd <- 0:12
x55 <- 55 + as.numeric(vapply(dd+1, function(k) paste0(".", strrep("5",k)), ""))
rnd.x <- roundAll(x55, dd)
formatF(rnd.x, digits = 14)
## and the deviations from "r3d.C":
noquote(cbind(dig = format(dd), format(rnd.x - rnd.x[,"r3d.C"],
                                       zero.print = ".", digits = 4)))
## FIXME: test !

## Had a bug in "r3d.C we don't want --- are getting into denormalized regions!
xx <- c(7e-304
  , pi*10^-(300:323)
  , 5e-324) # = 4.94..e-324, the 'mm' number
dig <- 305:340
okD <- dig <= 323L
(typV <- setdiff(roundVersions, c("r0.C", "r1.C")))
(no_printf <- setdiff(roundVersions, "sprintf"))
(no_LDOUBLE <- setdiff(roundVersions, "r3.C"))
epsC <- .Machine$double.eps
for(x in xx) {
    cat("\n", x, ":\n")
    dx <- x - roundAll(x, dig)
    print(cbind(dig = dig, "d+l10" = dig + .30103*(ilogb(x) + .5), abs(dx)/x)[okD,], digits = 4)
    ## FIXME: can do much more rational tests:  err  |dx| =~ 10^-dd (if dd > 0)
    if(hasLD) {
        stopifnot(is.finite(dx))
        if(x == 7e-304)
            stopifnot(abs(dx) < 10* x * epsC)
        stopifnot(dx[!okD, typV] == 0)
    } else { # no 'long double' -- Lnx 64b:  interestingly "sprintf" is quite a bit affected
        stopifnot(is.finite(dx[,no_LDOUBLE]))
        cat("No  long_double  capability\n")
        ## FIXME .. apart from "sprintf" things look good ... "yeah.."
        ## -----   TODO derive from above print( .... abs(dx)/x)
        if(FALSE) stopifnot(dx[!okD, no_printf] < 10 * x  * epsC)
    }
}


###--- keep in sync with ../man/roundX.Rd ------------------

## There now have "exhaustive small digit cases"
## where the conclusion could be  that "r1" is best

### More studies -- Study of small digits cases ----------------------------------------

## 1. all numbers in [0,1]
set.seed(47)
I <- c(0:999, randI(1000, 4), randI(2000, 5), randI(3000, 6))
ndI <- 1L + as.integer(log10(pmax(1,I))) # number of (decimal) digits of I
Ix <- I + 0.5
x <- Ix / (10^ndI) # numbers to be rounded; all of the form  <n>.5
SM <- roundAll(x, digits = ndI)
print(cbind(x, ndI, SM)[1:100,], digits = 9)
## sprintf *is* quite different from the r[12]* R versions ...

## Error, assuming "r3" to be best, as it *does* really go to nearest:
errM <- SM - SM[,"r3"]
rbind(exact   = colSums(err0 <-     errM != 0) ,           # (can have very small irrelevant differences)
      relevant= colSums(errT <- abs(errM) > .001*10^-ndI)) # --> sprintf clearly 2nd best (has changed +1)
## For F30 Linux 64-bit (gcc):
##          sprintf r0.C r1.C r1a.C r2.C r2a.C r3.C r3d.C r3
## exact       1494 1829 1829  1829 1829  1829    2     0  0
## relevant    1493 1828 1828  1828 1828  1828    1     0  0


## The middle round() versions ("r0*", "r1*", "r2*") all give the same (here, for x in [0,1]):
(ver012 <- grep("^r[012]", roundVersions, value=TRUE))
stopifnot(same_cols(errM[, ver012]))

## Visualization of error happening
cumErr <- apply(errT[,colnames(SM) != "r3"], 2L, cumsum)
matPm(cumErr)
sp <- search()
if(require("Matrix")) {
  showSp <- function(m) print(image(as(m, "sparseMatrix"), aspect = 4,
         ## fails, bug in lattice?  useRaster = !dev.interactive(TRUE) && (nrow(m) >= 2^12),
                                border.col = if(nrow(m) < 1e3) adjustcolor(1, 1/2) else NA))
  showSp(errM[1:100,])
  showSp(errM)
  showSp(errM != 0)
}

## 2. Numbers of varying sizes :
n <- length(I)
n1 <- round(n*.4)
n2 <- n - n1
set.seed(7)
dp <- sample(c(sample( -2:2, n1, replace=TRUE),
               sample(c(-1,1), n2, replace=TRUE) * rpois(n2, lambda = pi)))
stopifnot(length(dp) == n)
nd2 <- ndI + dp
nd2[nd2 == 0] <- 1 + rpois(sum(nd2 == 0), 0.5)
x <- Ix / (10^nd2)
S2 <- roundAll(x, digits = nd2) # --> warning  "NAs introduced by coercion"
## "Error", assuming "r3" to be best, as it *does* really go to nearest:
dP <- nd2 >= 0 # as  negative digits  don't work for "sprintf"
err2 <- S2[dP,] - S2[dP, "r3"]
rbind(exact   = colSums(err0  <-     err2 != 0),           # (not quite correct)
      relevant= colSums(err2T <- abs(err2) > .01*10^-nd2[dP])) # --> sprintf is *worst* !
## For F30 Linux 64-bit (gcc):
##          sprintf r0.C r1.C r1a.C r2.C r2a.C r3.C r3d.C r3
## exact       1639 1454 1503  1503 1454  1454    1     0  0
## relevant    1638 1453 1502  1502 1453  1453    0     0  0

## Visualization of error happening
cumEr2 <- apply(err2T, 2L, cumsum)
matPm(cumEr2[,colnames(S2) != "r3"])
## Really 4 distinct groups:   r0-r2* ||  r1*  ||  sprintf ||  r3*
plot(hclust(dist(t(cumEr2)/1e6, "manhattan")))

if(require("Matrix")) {
  showSp(err2[1:100,])
  showSp(err2)
  showSp(err2 != 0)
}
stopifnot(same_cols(err2[, c("r0.C", "r2.C", "r2a.C")]))

## The  "3C" versions *not* identical to the last bit but
stopifnot(same_cols(err2T[, c("r3.C", "r3d.C")])) # the "3C" versions are equivalent


## 3. Even more "widely varying" sizes and digits:
set.seed(43)
I <- c(0:9999, randI(2000, 6), randI(8000, 8), randI(20000, 11), randI(40000, 12))
ndI <- 1L + as.integer(log10(pmax(1,I))) # number of (decimal) digits of I
Ix <- I + 0.5
n <- length(I)
n1 <- round(n*.4)
n2 <- n - n1  ## length(I) =: n == n1+n2
set.seed(7)
dp <- sample(c(sample(-2:2, n1, replace=TRUE),
               sample(c(-1,1), n2, replace=TRUE) * rpois(n2, lambda = 7)))
stopifnot(length(dp) == n)
nd3 <- ndI + dp ## nd3 := #{digits} to round to .. should all be >= 1 :
nd3[nd3 == 0] <- 1 + rpois(sum(nd3 == 0), 0.5)
x <- Ix / (10^nd3)
S3 <- roundAll(x, digits = nd3) # --> warnings ...
## err3 := "Error", assuming "r3" to be correct ..
dP <- nd3 >= 0 # as  negative digits  don't work for "sprintf"
err3 <- S3[dP,] - S3[dP, "r3"]

rbind(exact   = colSums(err3. <-     err3 !=  0),           # (not quite correct)
      relevant= colSums(err3T <- abs(err3) > .01*10^-nd3[dP])) # --> sprintf  is  *worst* !
## For F30 Linux 64-bit (gcc):
##          sprintf  r0.C  r1.C r1a.C  r2.C r2a.C r3.C r3d.C r3
## exact      18958.17108 17512 17512 17108 17108  752   688  0 (earlier 2020?)
## exact      18885 17108 17512 17512 17108 17108  752   688  0
## relevant   18609 16762 17166 17166 16762 16762  464   357  0

## "r0*" and "r2*" are the same (here),  and so are the two "r1*" :
stopifnot(same_cols(err3[, vg1 <- c("r0.C", "r2.C", "r2a.C")]),
          same_cols(err3[, vg2 <- c("r1.C", "r1a.C")]))


## Visualization of error happening
cumEr3 <- apply(err3T, 2L, cumsum)
matPm(cumEr3[,colnames(S3) != "r3"])
plot(hclust(dist(t(cumEr3)/1e6, "manhattan")))

if(require("Matrix")) {
  showSp(err3[1:100,])  # 0:99
  showSp(err3[1:1000,]) # 0:999
  showSp(err3)
  showSp(err3 != 0) # B/W version of the above
}

## if(!any(sp == "package:Matrix")) detach("package:Matrix")

if(doExtras && require("gmp")) {
    ## version from ../vignettes/rationalRound.Rmd
    ##                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    }

    roundAllX <- function(x, digits, versions = roundVersions) {
        M <- cbind(roundAll(x, digits, versions=versions),
                   xctQ = asNumeric(roundQ(as.bigq(x), digits)))
        rownames(M) <- format(x)
        M
    }
    S3q <- roundAllX(x, digits = nd3) # --> warnings ...
    if(length(w <- warnings())) print(summary(w))

    ## err3q := "Error", assuming "xctQ" to be correct ..
    dP <- nd3 >= 0 # as  negative digits  don't work for "sprintf"
    err3q <- S3q[dP,] - S3q[dP, "xctQ"]
    print(
        rbind(exact   = colSums(              err3q !=  0),
              relevant= colSums(err3qT <- abs(err3q) > .01*10^-nd3[dP]))
    )
    ## For F30 Linux 64-bit (gcc):
    ##          sprintf  r0.C  r1.C r1a.C  r2.C r2a.C r3.C r3d.C r3
    ## ...............

    ## "r0*" and "r2*" are the same (here),  and so are the two "r1*" :
    stopifnot(same_cols(err3q[, vg1]),
              same_cols(err3q[, vg2]))

    ## Visualization of error happening
    cumEr3q <- apply(err3qT, 2L, cumsum)
    matPm(cumEr3q[,colnames(S3q) != "xctQ"])
    mtext('Comparison with "exact rational" ("xctQ") :')
    plot(hclust(dist(t(cumEr3q)/1e6, "manhattan")))

    ## if(!any(sp == "package:gmp")) detach("package:gmp")
}


## digits < 0, large |digits| ----  was wrong in R <= 3.6.x ---

M <- .Machine$double.xmax
options(digits=4)
round(M  , -(1:400)) # the horror for R <= 3.6.x: many 'Inf' (early on); ending in all NaN;
## for R >= 4.0.0, now ends in  {1.797, 1.79, 1.7, 1}e308 0 0 0 ... 0
round(M/2, -(0:320)) # much better for R <= 3.6; still ending in NaN
round(3141.5,  -(0:320)) # ditto : "only" NaN
## much of this is tested now in  <R>/tests/reg-tests-1d.R, ~line 3666

