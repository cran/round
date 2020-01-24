## n random integers with exactly 'd' digits {in practice, rather be *exhaustive* for say, d <= 3}
randI <- function(n, d) {
    stopifnot(length(n) == 1, n >= 0,
              length(d) == 1, d >= 1)
    p10 <- 10^d ## ; off <- .5 / p10
    ## nearbyint(p10 * runif(n, 0.1 - off, 1 - off)) ==
    ## nearbyint(p10 * (runif(n, 0.1, 1) - off))  ==
    nearbyint(p10 * runif(n, 0.1, 1) - .5)
}



