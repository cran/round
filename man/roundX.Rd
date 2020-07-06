\name{roundX}
\title{Rounding Numbers to Decimal Digits -- Variants}
\alias{roundX}
\alias{roundVersions}
\alias{roundAll}
\alias{round_r3}
\description{
  Provide several \code{version}s of algorithms for \code{\link{round}(x,
    digits)}, i.e., rounding to decimal digits.  In particular, provides
  previous and current implementations of \R's \code{round()}.
}
\usage{
roundX  (x, digits, version = roundVersions, trace = 0)

roundAll(x, digits, versions = roundVersions)

round_r3(x, d, info=FALSE, check=TRUE)

roundVersions # "sprintf" "r0.C" "r1.C" "r1a.C" "r2.C"  "r3.C" "r3d.C"  "r3"
}
\arguments{
  \item{x}{numeric vector}
  \item{digits, d}{integer number (for \code{d}) or numeric vector.}
  \item{version}{a \code{\link{character}} string specifying the version of
    rounding.  Must match \code{roundVersions} (via
    \code{\link{match.arg}}).}
  \item{trace}{integer; if positive, the corresponding computations should
    be \dQuote{traced} (possibly proportionally to the value of
    \code{trace}); currently only implemented for \code{version = "r3.C"}.}
  \item{versions}{a \code{\link{character}} vector, a subset of
    \code{roundVersions}.}
  \item{info}{\code{\link{logical}} specifying if \code{round_r3(*)} should
    result in a \code{\link{list}} with components
    \describe{
      \item{\code{"r"}:}{the rounded \code{x},}
      \item{\code{"D"}:}{the difference \code{(xu - x) - (x - xd)}, where
	\code{xd} and \code{xu} are the round \bold{d}own and \bold{u}p
	versions of \code{x},}
      \item{\code{"e"}:}{the modulo-2 remainder of \code{floor(x * 10^d)},
	which determines rounding to even (only) in case \code{D = 0}.}
    }
  }
  \item{check}{\code{\link{logical}} indicating if \code{x} and
    \code{digits} should be checked for validity.  Is set to \code{FALSE}
    when used in \code{roundX()} (or \code{roundAll}), as the checks happen
    before \code{round_r3()} is called.}
}
\details{
  Rounding to decimal digits is non-trivial in binary arithmetic.  ISO
  standard \dQuote{round to even}, see \code{\link{round}()}'s (help page),
  is more rare than typically assumed as most decimal fractions
  are not exactly representable in binary \code{\link{double}} precision
  numbers.

  Decimal rounding is well defined when \code{digits = 0}, and calls the
  (C99 standard) C library function \code{\link{nearbyint}()} (which
  provide in this package as well, for completeness):
  \code{\link{round}(x)} is (\R level) equivalent to \code{round(x, digits
  = 0)} and is also equivalent to (\R and C level) \code{nearbyint(x)}
  which is defined to return the closest integer number (as
  \code{\link{double}}) and in the case of \dQuote{doubt}, where both
  integer number neighbours are of the same distance, i.e., distance
  \code{0.5} the famous \dQuote{round to even} strategy is used, such that,
  e.g., \code{round(0:7 + 0.5) == c(0, 2, 2, 4, 4, 6, 6, 8)}.

  The following strategy / algorithms are used for the different
  \code{roundVersions}; note that we \emph{only} consider the crucial case
  \code{digits > 0} in the following description:
  %%
  \describe{
    \item{\code{"sprintf"}:}{diverts the operation to
      \code{\link{sprintf}("\%.*f", digits, x)} which in turn diverts to the
      corresponding C library function \code{sprintf()}; consequently may
      be platform dependent (though we have not yet seen differences from
      what we get by the most widely used GNU \file{glibc} library,
      \url{https://gnu.org/software/libc}).  This version does \emph{not}
      work with negative digits, returning \code{\link{NA}} with a
      \code{\link{warning}} there.}
    \item{\code{"r0.C"}:}{a (too much) simplified version of \R's
      \code{"r1.C"}, just skipping the whole integer part computations;
      this was the first patch proposal in R-bugs' report \PR{17668}.
      %% \url{https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17668}
      However, this completely breaks down in extreme cases.}% TODO show examples
    \item{\code{"r1.C"}:}{the version of \code{\link{round}()} as in \R
      3.6.2 and earlier.  It first removes the integer part(s) of \code{x},
      then rounds and re-adds the integer part.}
    \item{\code{"r1a.C"}:}{a slightly improved version of \code{"r1.C"},
      notably for |digits| > 308.}
    \item{\code{"r2.C"}:}{the version of \code{\link{round}()} as added to
      \sQuote{R-devel} (the development version of \R) with \file{svn} revision
      \code{.....}.  It does \emph{not} remove and re-add the integer part(s)
      of \code{x} but ensures that no unnecessary overflow to \code{+/-\link{Inf}} or
      underflow to \code{0} happens when numbers are multiplied and divided
      by \eqn{10^d}.}
    \item{\code{"r2a.C"}:}{a slightly improved version of \code{"r2.C"},
      notably for large negative digits.}
    \item{\code{"r3"}:}{(\R level) implementation of \dQuote{correct}
      rounding, rounding to the nearest double precision number (with
      \dQuote{round to even} in case of equal distance) as seen in the
      function definition of \code{\link{round_r3}()}.  Note that
      \code{info=TRUE} is only applied when when the \code{digits} \eqn{d}
      fulfill \eqn{|d| <= 308}.}
    \item{\code{"r3.C"}:}{a C translation of \code{"r3"}, using \code{long
	double} for intermediate computations which is particularly
      convenient for \code{digits} \eqn{308 < d < ~324} as overflow is not
      a possible then.}
    \item{\code{"r3d.C"}:}{a version of \code{"r3.C"}, only using
      \code{double} precision, and hence typically fast and less platform
      dependent, and also more often identical to \code{"r3"}.}
  }% describe
}
\value{
  \code{roundX()} returns a numeric vector (of length of recycled \code{x}
  and \code{digits}, i.e., typically (when \code{digits} is of
  length one) of \code{length(x)}.

  \code{round_r3()} is the workhorse of \code{roundX(.., version = "r3")};
  it vectorizes in \code{x} but needs \code{length(d) == 1}.

  \code{roundVersions} is a \code{\link{character}} vector of the versions
  available for \code{roundX()}.

  \code{roundAll()} applies \code{roundX()} for all \code{versions},
  returning a matrix if one of \code{x} or \code{digits} is not of length one.
}
%% \references{
%% }
\author{Martin Maechler (R Core for version "r1.C")}
%% \note{
%% }
\seealso{
  \code{\link{round}}, also \code{\link{signif}} which is relatively
  sophisticated (also by code from M.M.).}
\examples{
roundVersions

round (55.55, 1)
roundX(55.55, 1, "r3")

## round() with all  roundVersions; quite simple (w/ recycling!)
roundAll # shows the function's definition

roundAll(55.55, 1)
roundAll(55.555, 2)
roundAll(55.5555, 3)
roundAll(55.55555, 4)
roundAll(55.555555, 5)
roundAll(55.5555555, 6)

## other "controversial" cases
rEx <- cbind( x =  c(10.7775, 12.345, 9.18665),
         digits =  c(   3  ,    2   ,    4   ))

resEx <- matrix(, length(roundVersions), nrow(rEx),
                dimnames = list(roundVersions, as.character(rEx[,"x"])))
for(i in 1:nrow(rEx))
  resEx[,i] <-  roundAll(rEx[[i,"x"]], digits = rEx[[i,"digits"]])

resEx  # r0.C & r2* agree and differ from the r1*;
       # "r3*" is close to "r2*" but not for 12.345
## The parts of "r3" :
r3rE <- sapply(1:nrow(rEx), function(i)
               round_r3(rEx[[i,"x"]], rEx[[i,"digits"]], info=TRUE))
colnames(r3rE) <- sapply(rEx[,"x"], format)
r3rE # rounding to even when D=0, but not when D < 0

## "Deterministic" Simulation - few digits only:
long <- interactive() # save time/memory e.g. when checking
I <- if(long) 0:9999 else 0:999
Ix <- I + 0.5
ndI <- 1L + as.integer(log10(pmax(1,I))) # number of (decimal) digits of I
nd2 <- outer(ndI, if(long) -3:4 else -2:3, `+`)
x   <- c(t( Ix / (10^nd2) ))
nd2 <- c(t( nd2 ))
 x  <-  x [nd2 > 0]
nd2 <- nd2[nd2 > 0]
rx <- roundAll(x, digits = nd2)

formatF <- function(.) format(., scientific=FALSE, drop0trailing=TRUE)
rownames(rx) <- formatF(x)
options(width = 123)
noquote(cbind(d = nd2, formatF(rx))[1:140,])
## -> The first cases already show a diverse picture; sprintf() a bit as outlier

## Error, assuming "r3" to be best, as it *does* really go to nearest:
Err <- rx - rx[, "r3"]
## careful : allowing small "noise" differences:
tErr <- abs(Err) > 1e-3* 10^-nd2 # "truly" differing from "r3"
colSums(tErr)  ## --> old R "r1*" is best here, then sprintf (among non-r3):
## For F30 Linux 64-bit (gcc),  and this selection of cases, r0+r2 are worst; r1 is best
## sprintf   r0.C   r1.C  r1a.C    r2.C  r2a.C   r3.C r3d.C  r3
##   15559  19778  14078  14078   19778  19778      8     0   0  {  long }
##    1167   1457   1290   1290    1457   1457      0     0   0  { !long }
if(long) { ## Q: where does  "r3.C"  differ from "r3" == "r3d.C" ?    A: in 10 cases; 8 "real"
  i3D <- which(Err[,"r3.C"] != 0)
  print(cbind(d = nd2[i3D], formatF(rx[i3D,])), quote=FALSE)
  print.table(zapsmall(Err[i3D,]), zero.print = ".")# differences (not very small ones!)
}

## Visualization of error happening (FIXME: do zapsmall()-like not count "noise")
cumErr <- apply(tErr[,colnames(rx) != "r3"], 2L, cumsum)
matPm <- function(y) {
   matplot(y=y, type = "l", lwd = 2, xlab = "i", ylab = deparse(substitute(y)))
   abline(h = 0, lty=2, col="gray")
   legend("topleft", legend = setdiff(roundVersions, "r3"),
          col = 1:6, lty = 1:5, lwd = 2, bty = "n")
}
matPm(head(cumErr,  100)) # sprintf seems worst
matPm(head(cumErr,  250)) # now r0+2 is worst, sprintf best
matPm(head(cumErr, 1000)) # now sprintf clearly worst again
matPm(head(cumErr, 2000)) # 0r/r2 best sprintf catching up
if(long) {
matPm(head(cumErr, 5000)) # now sprintf clearly worst again
matPm(head(cumErr,10000)) # now r0+2 is worst,  r1 best
}
matPm( cumErr )

same_cols <- function(m) all(m == m[,1])
stopifnot(same_cols(Err[, c("r0.C", "r2.C", "r2a.C")]))
stopifnot(same_cols(Err[, c("r1.C", "r1a.C")]))
if(FALSE) ## *not* in 'long' case, see above
stopifnot(same_cols(Err[, c("r3", "r3.C", "r3d.C")]))

sp <- search()
if(long && require("Matrix")) {
  showSp <- function(m) print(image(as(m, "sparseMatrix"), aspect = 4,
         ## fails, bug in lattice?  useRaster = !dev.interactive(TRUE) && (nrow(m) >= 2^12),
                                border.col = if(nrow(m) < 1e3) adjustcolor(1, 1/2) else NA))
  showSp(head(Err, 100))
  showSp(head(Err, 1000))
  showSp(Err)
  showSp(Err != 0) # B&W version ..
  if(!any(sp == "package:Matrix")) detach("package:Matrix")
}

## More digits random sample simulation tend go against "sprintf";
## see ../tests/  and also the vignette
}
\keyword{arith}
\concept{Rounding}
