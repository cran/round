\name{mathC99}
\title{C99 'math' Library Functions (where Not in Standard aka 'Base' \R)}
\alias{logB}
\alias{ilogb}
\alias{fpclassify}
\alias{isnormal}
\alias{nearbyint}
\alias{signbit}
\alias{nextafter}
\alias{nexttoward}
\description{
  Provides simple \R versions of those C99 \dQuote{math lib} / \dQuote{libmath} /
  \dQuote{libm} functions that are not (yet) in standard (aka \sQuote{base} \R).
}
\usage{
 logB(x) # C's logb(x), numeric integer-valued "log2".
         # R's logb() is defined as "log wrt base"
ilogb(x) # == logB(), but of *type* integer

fpclassify(x)
isnormal(x)
nearbyint(x)
signbit(x)

nextafter(x, y)
nexttoward(x, y)
}
\arguments{
  \item{x, y}{numeric vector(s); will be recycled to common length.}
}
%% \details{
%% }
\value{
  a \code{\link{numeric}} (\code{\link{double}} or
  \code{\link{integer}}) vector of the same (or recycled) length of
  \code{x} (and \code{y} where appropriate) with the values of
  \code{<fn>(x)} for the corresponding C99 libmath function \code{<fn>}.
}
\references{
  Wikipedia (2020) \emph{C mathematical functions}
  \url{https://en.wikipedia.org/wiki/C_mathematical_functions}
}
\author{Martin Maechler}
\seealso{
  \code{\link{sqrt}},  \code{\link{log}}, \code{\link{exp}}, \code{\link{Trig}};

  \code{\link{floor}}, \code{\link{ceiling}}, \code{\link{trunc}};

  \code{\link{is.finite}}, \code{\link{is.na}} .
}
\examples{
x <- (1:20)*pi
stopifnot(ilogb (x) == logB (x),  is.integer(ilogb(x)),
          ilogb(-x) == logB(-x),  is.double ( logB(x)))
cbind(x, "2^il(x)"= 2^logB(x), ilogb = ilogb(x), signbit = signbit(x),
         fpclassify = fpclassify(x), isnormal = isnormal(x))

x <- c(2^-(10:22), rexp(1000));
summary(x / 2^ilogb(x)) # in   [1, 2)  interval
stopifnot(nearbyint(x) == round(x))

nextafter(-0, +0)
nextafter(+0,  1)
nextafter(+0, -1)
nextafter(Inf, -1)
nextafter(-Inf, 0)
}
\keyword{arithmetic}
\keyword{math}
