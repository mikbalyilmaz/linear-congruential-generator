# Linear Congruential Generator (LCG) Implementation in R
# Author: Muhammed Ikbal Yilmaz

# LCG function: generates a sequence and detects repetition
lcg <- function(a, c, m, seed, max_iter = 1e6, verbose = TRUE) {
  x <- numeric(max_iter + 1)
  x[1] <- seed
  
  for (i in 1:max_iter) {
    x[i + 1] <- (a * x[i] + c) %% m
    
    # Check for repetition
    if (any(x[1:i] == x[i + 1])) {
      if (verbose) {
        cat("Value", x[i + 1], "repeated at step", i + 1, "\n")
      }
      return(list(
        sequence = x[1:(i + 1)],
        repeat_step = i + 1,
        parameters = list(a = a, c = c, m = m, seed = seed)
      ))
    }
  }
  return(list(
    sequence = x,
    repeat_step = NA,
    parameters = list(a = a, c = c, m = m, seed = seed)
  ))
}

# Utility function: analyze a generated sequence
analyze_sequence <- function(seq) {
  return(list(
    length = length(seq),
    median = median(seq),
    unique_count = length(unique(seq)),
    first_values = head(seq, 10)
  ))
}

# Example 1
res1 <- lcg(a = 1079, c = 863, m = 7919219, seed = 1, max_iter = 1e5)
print(analyze_sequence(res1$sequence))

# Example 2
res2 <- lcg(a = 1722, c = 990, m = 9969219, seed = 1, max_iter = 1e5)
print(analyze_sequence(res2$sequence))

# Example 3 - Small modulus test
res3 <- lcg(a = 11, c = 0, m = 32, seed = 27, max_iter = 100)
print(res3$sequence)
cat("Median value:", median(res3$sequence), "\n")

