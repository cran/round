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
epsC <- .Machine$double.eps
for(x in xx) {
    cat("\n", x, ":\n")
    dx <- x - roundAll(x, dig)
    print(cbind(dig = dig, "d+l10" = dig + .30103*(ilogb(x) + .5), abs(dx)/x)[okD,], digits = 4)
    stopifnot(is.finite(dx))
    ## FIXME: can do much more rational tests:  err  |dx| =~ 10^-dd (if dd > 0)
    if(hasLD) {
        if(x == 7e-304)
            stopifnot(abs(dx) < 10* x * epsC)
        stopifnot(dx[!okD, typV] == 0)
    } else { # no 'long double' -- Lnx 64b:  interestingly "sprintf" is quite a bit affected
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
x <- Ix / (10^ndI)
SM <- roundAll(x, digits = ndI)
print(cbind(x, ndI, SM)[1:100,], digits = 9)
## sprintf *is* quite different from the r[12]* R versions ...

## Error, assuming "r3" to be best, as it *does* really go to nearest:
errM <- SM - SM[,"r3"]
rbind(exact   = colSums(err0 <- errM != 0),           # (not quite correct: can have very small irrelevant differences)
      relevant= colSums(errT <- errM > 1e-3*10^-ndI)) # --> sprintf clearly 2nd best
## For F30 Linux 64-bit (gcc):
##          sprintf r0.C r1.C r1a.C r2.C r2a.C r3.C r3d.C r3
## exact       1493 1829 1829  1829 1829  1829    2     0  0
## relevant     745  913  913   913  913   913    1     0  0


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
dp <- sample(c(sample(-2:2, n1, replace=TRUE),
               sample(c(-1,1), n2, replace=TRUE) * rpois(n2, lambda = pi)))
stopifnot(length(dp) == n)
nd2 <- ndI + dp
nd2[nd2 == 0] <- 1 + rpois(sum(nd2 == 0), 0.5)
x <- Ix / (10^nd2)
S2 <- roundAll(x, digits = nd2) # --> warning  "NAs introduced by coercion"
## "Error", assuming "r3" to be best, as it *does* really go to nearest:
dP <- nd2 >= 0 # as  negative digits  don't work for "sprintf"
err2 <- S2[dP,] - S2[dP, "r3"]
rbind(exact   = colSums(err0  <- err2 != 0),           # (not quite correct)
      relevant= colSums(err2T <- err2 > 1e-3*10^-nd2[dP])) # --> sprintf  is  *worst* !
## For F30 Linux 64-bit (gcc):
##          sprintf r0.C r1.C r1a.C r2.C r2a.C r3.C r3d.C r3
## exact       1638 1454 1503  1503 1454  1454    1     0  0
## relevant     835  726  761   761  726   726    0     0  0

## Visualization of error happening
cumEr2 <- apply(err2T[,colnames(S2) != "r3"], 2L, cumsum)
matPm(cumEr2)
if(require("Matrix")) {
  showSp(err2[1:100,])
  showSp(err2)
  showSp(err2 != 0)
}
stopifnot(same_cols(err2[, c("r0.C", "r2.C", "r2a.C")]))
if(FALSE) # nope
stopifnot(same_cols(err2[, c("r3.C", "r3d.C")])) # the 2 C versions *are* identical

## 3. Even more "widely varying" sizes and digits:
set.seed(43)
I <- c(0:9999, randI(2000, 6), randI(8000, 8), randI(20000, 11), randI(40000, 12))
ndI <- 1L + as.integer(log10(pmax(1,I))) # number of (decimal) digits of I
Ix <- I + 0.5
n <- length(I)
n1 <- round(n*.4)
n2 <- n - n1
set.seed(7)
dp <- sample(c(sample(-2:2, n1, replace=TRUE),
               sample(c(-1,1), n2, replace=TRUE) * rpois(n2, lambda = 7)))
stopifnot(length(dp) == n)
nd3 <- ndI + dp
nd3[nd3 == 0] <- 1 + rpois(sum(nd3 == 0), 0.5)
x <- Ix / (10^nd3)
S3 <- roundAll(x, digits = nd3) # --> warnings ...
## err3 := "Error", assuming "r3" to be correct ..
dP <- nd3 >= 0 # as  negative digits  don't work for "sprintf"
err3 <- S3[dP,] - S3[dP, "r3"]

rbind(exact   = colSums(err3.  <- err3 != 0),           # (not quite correct)
      relevant= colSums(err3T <- err3 > 1e-3*10^-nd3[dP])) # --> sprintf  is  *worst* !
## For F30 Linux 64-bit (gcc):
##          sprintf  r0.C  r1.C r1a.C  r2.C r2a.C r3.C r3d.C r3
## exact      18958 17108 17512 17512 17108 17108  752   688  0
## relevant    9061  8191  8405  8405  8191  8191   56     0  0

## "r0*" and "r2*" are the same (here),  and so are the two "r1*" :
stopifnot(same_cols(err3[, vg1 <- c("r0.C", "r2.C", "r2a.C")]),
          same_cols(err3[, vg2 <- c("r1.C", "r1a.C")]))


## Visualization of error happening
cumEr3 <- apply(err3T[,colnames(S3) != "r3"], 2L, cumsum)
matPm(cumEr3)
if(require("Matrix")) {
  showSp(err3[1:100,])  # 0:99
  showSp(err3[1:1000,]) # 0:999
  showSp(err3)
  showSp(err3 != 0) # B/W version of the above
}

## if(!any(sp == "package:Matrix")) detach("package:Matrix")



## digits < 0, large |digits| ----  Something else, still wrong ---------------------

M <- .Machine$double.xmax
options(digits=4)
round(M  , -(1:400)) # the horror ...
round(M/2, -(0:320)) # much better .. (only NaN),  same as for
round(3141.5,  -(0:320)) # ditto : "only" NaN


