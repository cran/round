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
win32 <- onWindows && !b64

if(!dev.interactive(orNone=TRUE)) pdf("round-extreme.pdf")

## the extreme case
M <- .Machine$double.xmax; dig <- -(1:314)
resM <- roundAll(M, dig)
if(length(warnings() -> wrngs)) print(summary(wrngs))
rownames(resM) <- paste0("dig=", dig)
op <- options(digits = 9, width = 149)
  print(head(resM,  9), digits = 7)
  print(tail(resM, 20))  # "r3*" are best
options(op)
iO <- 1:308
all.eq.M <- function(x,y, ...) all.equal(x, y, countEQ=TRUE, scale=M, ...)
all.eq.M(resM[iO, "r3.C"], resM[iO,"r3"], tol = 0) # "Mean scaled diff.: 4.58e-17"
all.eq.M(resM[iO,"r3d.C"], resM[iO,"r3"], tol = 0) #    "    "     "     7.17e-17"

## Windows 32 bit (on Winbuilder, gcc 4.9.x, -O3) is "exception"
stopifnot(exprs = {
   is.finite(resM[, c("r3.C", if(!win32) "r3d.C", "r3")])
   all.eq.M(resM[iO, "r3.C"], resM[iO,"r3"], tol = 1e-15)
   if(win32) TRUE else all.eq.M(resM[iO,"r3d.C"], resM[iO,"r3"], tol = 1e-15)
})
## but there are quite a few "small noise" differences:
table(resM[iO, "r3.C"] == resM[iO,"r3"])
## FALSE  TRUE
##   124   184  (F30 Linux gcc; 64-bit)
table(resM[iO,"r3d.C"] == resM[iO,"r3"])
##   136   172  (F30 Linux gcc; 64-bit)


## Unbelievably, this gives *NO* 'Inf' values (on 32-bit Windows),
## but the default trace=0 *does* give 'Inf' (on Winbuilder, see above):
roundX(.Machine$double.xmax, -(291:310), "r3d.C", trace = 2)
roundX(.Machine$double.xmax, -(291:310), "r3d.C")# trace = 0 --> contains 7 x Inf
