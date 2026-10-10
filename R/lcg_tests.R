# Empirical tests of uniformity and independence for numbers u in [0, 1).
#
# The tests follow Knuth (1997), Section 3.3.2. Each returns one row: statistic, degrees of freedom (where
# applicable) and p-value. A small p-value is evidence against the hypothesis of independent U(0, 1) draws.
# Passing a handful of such tests is a necessary, not a sufficient, condition for a good generator; serious
# evaluation uses large batteries such as TestU01 (L'Ecuyer and Simard, 2007).
#
# Author: Muhammed Ikbal Yilmaz

test_row <- function(test, statistic, df, p_value) {
  data.frame(test = test, statistic = unname(statistic), df = df, p_value = unname(p_value))
}

#' Chi-square test of equidistribution on `bins` equal cells.
test_equidistribution <- function(u, bins = 100) {
  counts <- tabulate(floor(u * bins) + 1, nbins = bins)
  expected <- length(u) / bins
  stat <- sum((counts - expected)^2 / expected)
  test_row(sprintf("Equidistribution (%d cells)", bins), stat, bins - 1, pchisq(stat, bins - 1, lower.tail = FALSE))
}

#' Kolmogorov-Smirnov test against U(0, 1).
test_ks <- function(u) {
  res <- suppressWarnings(ks.test(u, "punif"))  # an LCG output can contain ties only if n exceeds the period
  test_row("Kolmogorov-Smirnov", res$statistic, NA, res$p.value)
}

#' Serial test on non-overlapping d-tuples, each coordinate cut into `cells` intervals.
#'
#' Non-overlapping tuples are used so that the cell counts are multinomial and the chi-square reference
#' distribution with cells^d - 1 degrees of freedom is valid.
test_serial <- function(u, dim = 2, cells = 10) {
  n_tuples <- length(u) %/% dim
  idx <- matrix(floor(u[seq_len(n_tuples * dim)] * cells), ncol = dim, byrow = TRUE)
  cell <- as.vector(idx %*% cells^(seq_len(dim) - 1)) + 1
  k <- cells^dim
  counts <- tabulate(cell, nbins = k)
  expected <- n_tuples / k
  stat <- sum((counts - expected)^2 / expected)
  test_row(sprintf("Serial, %d-tuples (%d^%d cells)", dim, cells, dim), stat, k - 1,
           pchisq(stat, k - 1, lower.tail = FALSE))
}

#' Lag-k serial correlation; under the null sqrt(n) * r is approximately standard normal.
test_autocorrelation <- function(u, lag = 1) {
  n <- length(u)
  r <- cor(u[1:(n - lag)], u[(1 + lag):n])
  z <- sqrt(n - lag) * r
  test_row(sprintf("Serial correlation, lag %d", lag), r, NA, 2 * pnorm(-abs(z)))
}

#' Runs up and down (Wald-Wolfowitz type): number of monotone runs compared with its null distribution.
#'
#' Under independence the number of runs R in a sequence of n distinct values has mean (2n - 1) / 3 and
#' variance (16n - 29) / 90.
test_runs_updown <- function(u) {
  n <- length(u)
  signs <- sign(diff(u))
  signs <- signs[signs != 0]
  runs <- 1 + sum(signs[-1] != signs[-length(signs)])
  mean_r <- (2 * n - 1) / 3
  var_r <- (16 * n - 29) / 90
  z <- (runs - mean_r) / sqrt(var_r)
  test_row("Runs up and down", z, NA, 2 * pnorm(-abs(z)))
}

#' All tests above, in one data frame.
run_test_battery <- function(u) {
  rbind(test_equidistribution(u),
        test_ks(u),
        test_serial(u, dim = 2, cells = 10),
        test_serial(u, dim = 3, cells = 10),
        test_autocorrelation(u, lag = 1),
        test_runs_updown(u))
}
