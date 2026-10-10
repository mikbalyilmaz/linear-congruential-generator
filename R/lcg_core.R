# Linear congruential generator: exact arithmetic, generation, jump-ahead and cycle detection.
#
# All quantities are integers stored as R doubles. A double represents every integer up to 2^53 exactly,
# so the code keeps every intermediate value below 2^53. Moduli up to 2^51 are supported.
#
# Author: Muhammed Ikbal Yilmaz

LCG_MAX_MODULUS <- 2^51
EXACT_LIMIT <- 2^53

is_whole <- function(x) is.numeric(x) && all(is.finite(x)) && all(x == floor(x))

#' Validate the parameters of x[n+1] = (a * x[n] + c) mod m.
check_lcg_params <- function(a, c, m, seed = 0) {
  for (nm in c("a", "c", "m", "seed")) {
    v <- get(nm)
    if (length(v) != 1L || !is_whole(v)) stop(sprintf("`%s` must be a single whole number.", nm), call. = FALSE)
  }
  if (m < 2 || m > LCG_MAX_MODULUS) stop("`m` must satisfy 2 <= m <= 2^51.", call. = FALSE)
  if (a < 1 || a >= m) stop("`a` must satisfy 1 <= a < m.", call. = FALSE)
  if (c < 0 || c >= m) stop("`c` must satisfy 0 <= c < m.", call. = FALSE)
  if (seed < 0 || seed >= m) stop("`seed` must satisfy 0 <= seed < m.", call. = FALSE)
  invisible(TRUE)
}

#' Exact (a * x) mod m for a scalar a and a vector x, with 0 <= a, x < m <= 2^51.
#'
#' If a * (m - 1) is below 2^53 the product is formed directly. Otherwise a is written in base 2^s and the
#' product is accumulated digit by digit (Horner's rule); s is chosen so that no intermediate value
#' reaches 2^53.
mulmod <- function(a, x, m) {
  a <- as.numeric(a)  # R integers overflow at 2^31; doubles are exact up to 2^53
  x <- as.numeric(x)
  m <- as.numeric(m)
  if (a * (m - 1) < EXACT_LIMIT) return((a * x) %% m)
  s <- floor(52 - log2(m))
  base <- 2^s
  digits <- numeric(0)
  while (a > 0) {
    digits <- c(a %% base, digits)
    a <- a %/% base
  }
  r <- numeric(length(x))
  for (d in digits) r <- (r * base + d * x) %% m
  r
}

#' One step of the recurrence, exact for every admissible parameter set.
lcg_step <- function(x, a, c, m) (mulmod(a, x, m) + c) %% m

#' Compose two affine maps modulo m: first (a2, c2), then (a1, c1).
compose_affine <- function(a1, c1, a2, c2, m) {
  c(mulmod(a1, a2, m), (mulmod(a1, c2, m) + c1) %% m)
}

#' Coefficients (A, C) with x[n+k] = (A * x[n] + C) mod m (Brown, 1994).
#'
#' Computed by binary exponentiation of the affine map in O(log k) steps.
lcg_jump <- function(k, a, c, m) {
  if (length(k) != 1L || !is_whole(k) || k < 0) stop("`k` must be a non-negative whole number.", call. = FALSE)
  result <- c(1, 0)  # identity map
  power <- c(a, c)
  while (k > 0) {
    if (k %% 2 == 1) result <- compose_affine(power[1], power[2], result[1], result[2], m)
    power <- compose_affine(power[1], power[2], power[1], power[2], m)
    k <- k %/% 2
  }
  c(A = result[1], C = result[2])
}

#' Generate x[1], ..., x[n] from the seed x[0].
#'
#' The first `block` values are produced one by one; every further block is obtained from the previous one
#' with a single vectorised jump of length `block`. This replaces n interpreted iterations with about
#' 2 * sqrt(n) vector operations.
lcg_generate <- function(n, a, c, m, seed, block = NULL) {
  check_lcg_params(a, c, m, seed)
  a <- as.numeric(a); c <- as.numeric(c); m <- as.numeric(m); seed <- as.numeric(seed)
  if (length(n) != 1L || !is_whole(n) || n < 0) stop("`n` must be a non-negative whole number.", call. = FALSE)
  if (n == 0) return(numeric(0))
  if (is.null(block)) block <- ceiling(sqrt(n))
  block <- max(1, min(n, block))
  out <- numeric(n)
  x <- seed
  for (i in seq_len(block)) {
    x <- lcg_step(x, a, c, m)
    out[i] <- x
  }
  if (n > block) {
    jump <- lcg_jump(block, a, c, m)
    start <- 1
    while (start + block <= n) {
      from <- start:min(start + block - 1, n - block)
      out[from + block] <- (mulmod(jump[["A"]], out[from], m) + jump[["C"]]) %% m
      start <- start + block
    }
  }
  out
}

#' Uniform numbers u[i] = x[i] / m in [0, 1).
lcg_uniform <- function(n, a, c, m, seed, block = NULL) lcg_generate(n, a, c, m, seed, block) / m

#' Tail length mu and cycle length lambda of the orbit of the seed (Brent, 1980).
#'
#' x[mu] is the first value that is visited again, and x[n + lambda] = x[n] for every n >= mu. Memory use is
#' constant. The search stops with NA after `max_steps` steps of the cycle search.
lcg_cycle <- function(a, c, m, seed, max_steps = 1e8) {
  check_lcg_params(a, c, m, seed)
  a <- as.numeric(a); c <- as.numeric(c); m <- as.numeric(m); seed <- as.numeric(seed)
  direct <- a * (m - 1) + c < EXACT_LIMIT
  f <- if (direct) function(x) (a * x + c) %% m else function(x) lcg_step(x, a, c, m)

  # Phase 1: cycle length.
  power <- 1
  lambda <- 1
  tortoise <- seed
  hare <- f(seed)
  steps <- 1
  while (tortoise != hare) {
    if (power == lambda) {
      tortoise <- hare
      power <- power * 2
      lambda <- 0
    }
    hare <- f(hare)
    lambda <- lambda + 1
    steps <- steps + 1
    if (steps > max_steps) {
      warning("Cycle not found within `max_steps` steps; returning NA.", call. = FALSE)
      return(c(mu = NA_real_, lambda = NA_real_))
    }
  }

  # Phase 2: tail length. The hare starts lambda steps ahead, placed there with one jump.
  jump <- lcg_jump(lambda, a, c, m)
  tortoise <- seed
  hare <- (mulmod(jump[["A"]], seed, m) + jump[["C"]]) %% m
  mu <- 0
  while (tortoise != hare) {
    tortoise <- f(tortoise)
    hare <- f(hare)
    mu <- mu + 1
  }
  c(mu = mu, lambda = lambda)
}
