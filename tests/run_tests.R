# Unit tests. Run from the repository root:  Rscript tests/run_tests.R
#
# Each result of the library is compared with an independent computation: a plain loop, a brute-force
# search over all values, or reference values computed with arbitrary-precision integers.

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

n_failed <- 0
check <- function(name, ok) {
  ok <- isTRUE(all(ok))
  cat(sprintf("%-4s %s\n", if (ok) "ok" else "FAIL", name))
  if (!ok) n_failed <<- n_failed + 1
}

naive_sequence <- function(n, a, c, m, seed) {
  out <- numeric(n)
  x <- seed
  for (i in seq_len(n)) {
    x <- (a * x + c) %% m
    out[i] <- x
  }
  out
}

brute_cycle <- function(a, c, m, seed) {
  first_seen <- rep(NA_real_, m)
  x <- seed
  i <- 0
  while (is.na(first_seen[x + 1])) {
    first_seen[x + 1] <- i
    x <- (a * x + c) %% m
    i <- i + 1
  }
  c(mu = first_seen[x + 1], lambda = i - first_seen[x + 1])
}

set.seed(20261010)

# Generation -------------------------------------------------------------------------------------------
check("generation matches a plain loop (several n and block sizes)", {
  ok <- TRUE
  for (n in c(1, 2, 7, 100, 1001, 12345)) for (block in list(NULL, 1, 3, 64)) {
    ok <- ok && identical(lcg_generate(n, 1079, 863, 7919219, 1, block = block), naive_sequence(n, 1079, 863, 7919219, 1))
  }
  ok
})

# Reference values for a = 0x5DEECE66D, c = 11, m = 2^48, seed 1, computed with exact integer arithmetic
# (Python integers). A naive double-precision loop already differs at the second value.
rand48_reference <- c(25214903928, 206026503483683, 245470556921330, 105707381795861, 223576932655868)
check("exact arithmetic when a * x exceeds 2^53 (rand48 parameters)",
      identical(lcg_generate(5, 25214903917, 11, 2^48, 1), rand48_reference))
check("the naive loop is wrong for the same parameters",
      !identical(naive_sequence(5, 25214903917, 11, 2^48, 1), rand48_reference))
check("vectorised blocks agree with step-by-step generation for rand48",
      identical(lcg_generate(5000, 25214903917, 11, 2^48, 1), lcg_generate(5000, 25214903917, 11, 2^48, 1, block = 1)))

check("jump of k steps equals k single steps", {
  ok <- TRUE
  for (k in c(0, 1, 2, 5, 64, 999)) {
    jump <- lcg_jump(k, 1722, 990, 9969219)
    expected <- if (k == 0) 12345 else naive_sequence(k, 1722, 990, 9969219, 12345)[k]
    ok <- ok && ((jump[["A"]] * 12345 + jump[["C"]]) %% 9969219 == expected)
  }
  ok
})

check("mulmod agrees with direct products where those are exact", {
  m <- 2^26 - 5
  a <- as.numeric(sample(m - 1, 200))
  x <- as.numeric(sample(m - 1, 200))
  all(mapply(function(ai, xi) mulmod(ai, xi, m) == (ai * xi) %% m, a, x))
})

# Cycle detection --------------------------------------------------------------------------------------
check("Brent's method equals a brute-force search on 500 random small generators", {
  ok <- TRUE
  for (i in 1:500) {
    m <- sample(2:3000, 1)
    a <- sample(seq_len(m - 1), 1)
    c <- sample(0:(m - 1), 1)
    seed <- sample(0:(m - 1), 1)
    ok <- ok && identical(lcg_cycle(a, c, m, seed), brute_cycle(a, c, m, seed))
  }
  ok
})

check("README example a = 11, c = 0, m = 32, seed 27: sequence and period 8", {
  identical(lcg_generate(8, 11, 0, 32, 27), c(9, 3, 1, 11, 25, 19, 17, 27)) &&
    identical(lcg_cycle(11, 0, 32, 27), c(mu = 0, lambda = 8))
})

check("version 1 examples: tail and cycle lengths agree with the first repetition reported by v1", {
  identical(lcg_cycle(1079, 863, 7919219, 1), c(mu = 0, lambda = 56826)) &&
    identical(lcg_cycle(1722, 990, 9969219, 1), c(mu = 2, lambda = 14076))
})

# Theory -----------------------------------------------------------------------------------------------
check("prime factorisation reconstructs n", {
  ok <- TRUE
  for (n in c(1, 2, 32, 7919219, 9969219, 2^31 - 1, 2^48, 999983^2, 2 * 3 * 5 * 7 * 11 * 13 * 999983,
              sample(1e9, 50))) {
    pf <- prime_factors(n)
    ok <- ok && prod(pf$prime^pf$exponent) == n
  }
  ok
})

check("Hull-Dobell conditions hold exactly when the period is m (all m <= 40, c != 0)", {
  ok <- TRUE
  for (m in 2:40) for (a in 1:(m - 1)) for (c in 1:(m - 1)) {
    ok <- ok && (hull_dobell(a, c, m)$full_period == (brute_cycle(a, c, m, 0)[["lambda"]] == m))
  }
  ok
})

brute_order <- function(a, m) {
  k <- 1
  x <- a %% m
  while (x != 1) {
    x <- (x * a) %% m
    k <- k + 1
  }
  k
}

check("Carmichael function equals the largest multiplicative order (m <= 300)", {
  all(sapply(2:300, function(m) {
    units <- Filter(function(a) gcd(a, m) == 1, seq_len(m - 1))
    carmichael_lambda(m) == max(sapply(units, brute_order, m = m))
  }))
})

check("multiplicative order equals brute force (m <= 300)", {
  all(sapply(2:300, function(m) {
    units <- Filter(function(a) gcd(a, m) == 1, seq_len(m - 1))
    all(sapply(units, function(a) multiplicative_order(a, m) == brute_order(a, m)))
  }))
})

check("integer-typed arguments give the same result as doubles",
      identical(lcg_generate(50L, 16807L, 0L, 2147483647, 1L), lcg_generate(50, 16807, 0, 2^31 - 1, 1)))

check("Park-Miller multiplier 16807 is a primitive root modulo 2^31 - 1",
      multiplicative_order(16807, 2^31 - 1) == 2^31 - 2)

check("two-dimensional spectral test equals an exhaustive search (m <= 400)", {
  ok <- TRUE
  for (m in c(31, 32, 101, 128, 257, 400)) for (a in seq_len(m - 1)) {
    best <- Inf
    for (s2 in -m:m) {
      s1 <- (-a * s2) %% m
      for (s1c in c(s1, s1 - m)) {
        if (s1c != 0 || s2 != 0) best <- min(best, s1c^2 + s2^2)
      }
    }
    ok <- ok && isTRUE(all.equal(unname(spectral_test_2d(a, m)[["nu2"]]), sqrt(best)))
  }
  ok
})

# Statistical tests ------------------------------------------------------------------------------------
check("test battery returns valid p-values", {
  res <- run_test_battery(runif(30000))
  nrow(res) == 6 && all(res$p_value >= 0 & res$p_value <= 1)
})

cat(sprintf("\n%d check(s) failed.\n", n_failed))
quit(status = if (n_failed == 0) 0 else 1)
