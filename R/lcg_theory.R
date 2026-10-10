# Number-theoretic results on the period and the lattice structure of an LCG.
#
# References: Hull and Dobell (1962); Knuth (1997), Sections 3.2.1.2 and 3.3.4.
#
# Author: Muhammed Ikbal Yilmaz

gcd <- function(x, y) {
  while (y != 0) {
    r <- x %% y
    x <- y
    y <- r
  }
  abs(x)
}

lcm <- function(x, y) x / gcd(x, y) * y

#' Prime factorisation by trial division, for n <= 2^51.
#'
#' Returns a data frame with one row per distinct prime and its exponent. Candidate divisors are tested in
#' vectorised chunks, so even n near 2^51 (about 4.7e7 candidates) is handled in seconds.
prime_factors <- function(n) {
  if (length(n) != 1L || !is_whole(n) || n < 1) stop("`n` must be a positive whole number.", call. = FALSE)
  primes <- numeric(0)
  exps <- numeric(0)
  take_out <- function(p) {
    e <- 0
    while (n %% p == 0) {
      n <<- n %/% p
      e <- e + 1
    }
    if (e > 0) {
      primes <<- c(primes, p)
      exps <<- c(exps, e)
    }
  }
  take_out(2)
  d <- 3
  chunk <- 1e6
  while (d * d <= n) {
    candidates <- seq(d, max(d, min(floor(sqrt(n)) + 1, d + 2 * (chunk - 1))), by = 2)
    hits <- candidates[n %% candidates == 0]
    if (length(hits) > 0) {
      take_out(hits[1])
      d <- hits[1] + 2
    } else {
      d <- candidates[length(candidates)] + 2
    }
  }
  if (n > 1) {
    primes <- c(primes, n)
    exps <- c(exps, 1)
  }
  data.frame(prime = primes, exponent = exps)
}

format_factors <- function(pf) {
  if (nrow(pf) == 0) return("1")
  paste(ifelse(pf$exponent == 1, format(pf$prime, scientific = FALSE, trim = TRUE),
               paste0(format(pf$prime, scientific = FALSE, trim = TRUE), "^", pf$exponent)),
        collapse = " * ")
}

#' The three conditions of the Hull-Dobell theorem.
#'
#' For c != 0 the period equals m for every seed if and only if all three hold. The theorem does not apply to
#' the multiplicative case c = 0, whose period is at most the Carmichael function lambda(m).
hull_dobell <- function(a, c, m) {
  check_lcg_params(a, c, m)
  primes <- prime_factors(m)$prime
  cond1 <- gcd(c, m) == 1
  cond2 <- all((a - 1) %% primes == 0)
  cond3 <- if (m %% 4 == 0) (a - 1) %% 4 == 0 else TRUE
  list(c_coprime_to_m = cond1,
       a_minus_1_divisible_by_every_prime_factor = cond2,
       a_minus_1_divisible_by_4_if_4_divides_m = cond3,
       applicable = c != 0,
       full_period = c != 0 && cond1 && cond2 && cond3)
}

#' Carmichael function lambda(m): the largest multiplicative order of any unit modulo m.
carmichael_lambda <- function(m) {
  pf <- prime_factors(m)
  out <- 1
  for (i in seq_len(nrow(pf))) {
    p <- pf$prime[i]
    e <- pf$exponent[i]
    term <- if (p == 2) {
      if (e == 1) 1 else if (e == 2) 2 else 2^(e - 2)
    } else {
      p^(e - 1) * (p - 1)
    }
    out <- lcm(out, term)
  }
  out
}

#' The largest period any seed can reach: m if c != 0, lambda(m) if c = 0.
max_period <- function(c, m) if (c != 0) m else carmichael_lambda(m)

#' Exact a^k mod m.
powmod <- function(a, k, m) {
  result <- 1 %% m
  a <- a %% m
  while (k > 0) {
    if (k %% 2 == 1) result <- mulmod(a, result, m)
    a <- mulmod(a, a, m)
    k <- k %/% 2
  }
  result
}

#' Multiplicative order of a modulo m, for gcd(a, m) = 1.
#'
#' The order divides lambda(m); prime factors are removed from lambda(m) while a^(order / q) is still 1.
#' For c = 0 and a seed coprime to m the sequence is purely periodic with exactly this period.
multiplicative_order <- function(a, m) {
  if (gcd(a, m) != 1) stop("`a` and `m` must be coprime.", call. = FALSE)
  order <- carmichael_lambda(m)
  for (q in prime_factors(order)$prime) {
    while (order %% q == 0 && powmod(a, order / q, m) == 1) order <- order / q
  }
  order
}

#' Two-dimensional spectral test (Knuth, 1997, Section 3.3.4).
#'
#' The overlapping pairs (u[n], u[n+1]) lie on parallel lines; nu2 is the length of the shortest non-zero
#' vector (s1, s2) with s1 + a * s2 = 0 (mod m), and 1 / nu2 is the largest distance between adjacent lines.
#' The reduced basis is found with Lagrange-Gauss reduction. Exact for m <= 2^26 (products stay below 2^53).
#' `normalized` divides nu2 by its upper bound (4/3)^(1/4) * sqrt(m), so values close to 1 are good.
spectral_test_2d <- function(a, m) {
  if (m > 2^26) return(c(nu2 = NA_real_, line_spacing = NA_real_, normalized = NA_real_))
  u <- c(m, 0)
  v <- c(-a, 1)
  norm2 <- function(w) sum(w * w)
  if (norm2(u) < norm2(v)) { t <- u; u <- v; v <- t }
  repeat {
    q <- round(sum(u * v) / norm2(v))
    w <- u - q * v
    if (norm2(w) >= norm2(v)) break
    u <- v
    v <- w
  }
  nu2 <- sqrt(norm2(v))
  c(nu2 = nu2, line_spacing = 1 / nu2, normalized = nu2 / ((4 / 3)^(1 / 4) * sqrt(m)))
}
