\name{randI}
\alias{randI}
\title{Random Integers of Specified Number of Digits}
\description{
  Create \code{n} random integer valued numbers all with a specified number of
  digits \code{d}.
}
\usage{
randI(n, d)
}
\arguments{
  \item{n}{numeric sample size, i.e., \code{\link{length}()} of result.}
  \item{d}{a positive integer, giving the exact number of digits the
    resulting numbers must have.}
}
\value{
  A \code{\link{numeric}} vector of \code{\link{length}} \code{n} of
  numbers \code{N} where each \eqn{N} has exactly \code{d} digits;
  equivalently,
  \deqn{10^{d-1} \le N_i < 10^d,}{10^(d-1) <= N[i] < 10^d,}
  \bold{and} every \eqn{N_i}{N[i]} appears with the same probability.
}
\details{This is based on \code{\link{runif}()} and not
  \code{\link{sample}()}, which for now also makes it less \R version
  dependent.
}
\author{Martin Maechler}
\seealso{
  Uniform random numbers \code{\link{runif}}; Random number generators,
  seeds, etc: \code{\link{RNG}}.
}
\examples{
plot(
 T2 <- table(randI(1e6, 2)))
chisq.test(T2) # typically not at all significant
T3 <- table(randI(1e6, 3))
chisq.test(T3)
stopifnot(exprs = {
  identical( 10:99 , as.integer(names(T2)))
  identical(100:999, as.integer(names(T3)))
})
}
\keyword{distribution}

