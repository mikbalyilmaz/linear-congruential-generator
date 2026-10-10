# Period of each generator: Hull-Dobell conditions, the theoretical maximum, and the exact tail and cycle
# lengths of the orbit of the chosen seed.

source("00_setup.R")

rows <- lapply(seq_len(nrow(GENERATORS)), function(i) {
  g <- GENERATORS[i, ]
  hd <- hull_dobell(g$a, g$c, g$m)
  bound <- max_period(g$c, g$m)

  if (g$c != 0 && hd$full_period) {
    cyc <- c(mu = 0, lambda = g$m)
    method <- "Hull-Dobell theorem"
  } else if (g$c == 0 && gcd(g$seed, g$m) == 1 && gcd(g$a, g$m) == 1) {
    cyc <- c(mu = 0, lambda = multiplicative_order(g$a, g$m))
    method <- "Multiplicative order"
  } else {
    cyc <- lcg_cycle(g$a, g$c, g$m, g$seed)
    method <- "Brent's algorithm"
  }

  data.frame(
    generator = g$label, a = fmt_int(g$a), c = fmt_int(g$c), m = fmt_int(g$m),
    m_factorised = format_factors(prime_factors(g$m)),
    gcd_a_m = gcd(g$a, g$m),
    hd_c_coprime = if (hd$applicable) hd$c_coprime_to_m else NA,
    hd_prime_factors = if (hd$applicable) hd$a_minus_1_divisible_by_every_prime_factor else NA,
    hd_four = if (hd$applicable) hd$a_minus_1_divisible_by_4_if_4_divides_m else NA,
    max_period = bound,
    tail = cyc[["mu"]],
    period = cyc[["lambda"]],
    share_of_m = cyc[["lambda"]] / g$m,
    method = method
  )
})
period_table <- do.call(rbind, rows)
save_table(period_table, "table1_periods")
print(period_table)

# The worked example of the version 1 README, as stated and as computed.
ex <- generator("v1_ex3")
computed <- c(ex$seed, lcg_generate(8, ex$a, ex$c, ex$m, ex$seed))
readme_example <- data.frame(
  n = 0:8,
  stated_in_v1_readme = c(27, 9, 3, 1, 11, 23, 29, NA, NA),
  computed = computed
)
save_table(readme_example, "table2_readme_example")
print(readme_example)

# What version 1 reports for its own examples, next to the tail and cycle lengths.
v1 <- load_v1()
v1_rows <- lapply(c("v1_ex1", "v1_ex2", "v1_ex3"), function(id) {
  g <- generator(id)
  res <- v1$lcg(g$a, g$c, g$m, g$seed, max_iter = 1e5, verbose = FALSE)
  cyc <- lcg_cycle(g$a, g$c, g$m, g$seed)
  repeated <- res$sequence[length(res$sequence)]
  data.frame(generator = g$label,
             v1_repeat_step = res$repeat_step,
             v1_repeated_value = repeated,
             v1_unique_count = length(unique(res$sequence)),
             tail = cyc[["mu"]], period = cyc[["lambda"]],
             first_index_of_repeated_value = match(repeated, res$sequence) - 1)
})
v1_table <- do.call(rbind, v1_rows)
save_table(v1_table, "table3_v1_outputs")
print(v1_table)
