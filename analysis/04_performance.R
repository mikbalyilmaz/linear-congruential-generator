# Running time of version 1 and version 2. Absolute times depend on the machine; the growth with the
# problem size does not.

source("00_setup.R")

# Rscript 04_performance.R --plot-only redraws Figure 3 from figures/table8_performance.csv without timing again
# (the timings depend on the machine; the published table and figure come from one run).
PLOT_ONLY <- "--plot-only" %in% commandArgs(trailingOnly = TRUE)

if (!PLOT_ONLY) {
  # Median over `reps` measurements; each measurement repeats the call until at least 0.2 seconds have
  # passed, so that calls far shorter than the timer resolution are still measured.
  timed <- function(expr_fun, reps) {
    one <- function() {
      calls <- 0
      start <- proc.time()[["elapsed"]]
      repeat {
        expr_fun()
        calls <- calls + 1
        elapsed <- proc.time()[["elapsed"]] - start
        if (elapsed >= 0.2) return(elapsed / calls)
      }
    }
    median(replicate(reps, one()))
  }

  # Cycle detection. Full-period generators x -> (5x + 1) mod 2^k satisfy the Hull-Dobell conditions, so the
  # cycle length is exactly 2^k.
  v1 <- load_v1()
  cycle_rows <- list()
  for (k in 8:24) {
    m <- 2^k
    v2_time <- timed(function() lcg_cycle(5, 1, m, 0), reps = 3)
    stopifnot(lcg_cycle(5, 1, m, 0)[["lambda"]] == m)
    v1_time <- if (k <= 15) timed(function() v1$lcg(5, 1, m, 0, max_iter = m + 1, verbose = FALSE), reps = 3) else NA
    cycle_rows[[length(cycle_rows) + 1]] <- data.frame(task = "cycle detection", size = m, v1_seconds = v1_time,
                                                       v2_seconds = v2_time)
  }
  cycle_times <- do.call(rbind, cycle_rows)

  # Generation of n numbers: a plain loop (as in version 1, without its repetition check) against the
  # blocked jump-ahead of version 2.
  plain_loop <- function(n, a, c, m, seed) {
    out <- numeric(n)
    x <- seed
    for (i in seq_len(n)) {
      x <- (a * x + c) %% m
      out[i] <- x
    }
    out
  }
  g <- generator("minstd")
  gen_rows <- list()
  for (n in 10^(3:7)) {
    stopifnot(identical(plain_loop(1000, g$a, g$c, g$m, g$seed), lcg_generate(1000, g$a, g$c, g$m, g$seed)))
    gen_rows[[length(gen_rows) + 1]] <- data.frame(
      task = "generation", size = n,
      v1_seconds = timed(function() plain_loop(n, g$a, g$c, g$m, g$seed), reps = 3),
      v2_seconds = timed(function() lcg_generate(n, g$a, g$c, g$m, g$seed), reps = 3))
  }
  gen_times <- do.call(rbind, gen_rows)

  perf <- rbind(cycle_times, gen_times)
  perf$machine <- sprintf("%s; %s", R.version.string, R.version$platform)
  save_table(perf, "table8_performance")
  print(perf)
}

# Why this figure: on log-log axes a running time proportional to n^k is a straight line of slope k, so the
# figure shows the order of growth directly. Version 1's cycle search has slope 2 (quadratic), version 2's
# slope 1 (linear); for generation both have slope 1 and differ only by a constant factor.
perf <- utils::read.csv(file.path(FIG_DIR, "table8_performance.csv"), stringsAsFactors = FALSE)
cycle_times <- perf[perf$task == "cycle detection", ]
gen_times <- perf[perf$task == "generation", ]

save_figure("fig3_performance", height = 2.5,
            heading = function() L("Figure 3. Running time of version 1 and version 2 (median of three runs)",
                                   "\u015eekil 3. S\u00fcr\u00fcm 1 ve s\u00fcr\u00fcm 2'nin \u00e7al\u0131\u015fma s\u00fcresi"), function() {
  par(mfrow = c(1, 2))
  draw_panel <- function(d, title, xlab) {
    x_ax <- log10_limits(d$size)
    y_ax <- log10_limits(c(d$v1_seconds, d$v2_seconds))
    plot(NA, xlim = x_ax$lim, ylim = y_ax$lim, log = "xy", xlab = xlab, ylab = "", axes = FALSE)
    log10_axis(1, x_ax$at)
    log10_axis(2, y_ax$at)
    lines(d$size, d$v1_seconds, col = INK, lwd = 1.4, type = "o", pch = 16, cex = 0.5)
    lines(d$size, d$v2_seconds, col = BLUE, lwd = 1.4, type = "o", pch = 16, cex = 0.5)
    legend("bottomright", legend = c(L("Version 1", "S\u00fcr\u00fcm 1"), L("Version 2", "S\u00fcr\u00fcm 2")),
           col = c(INK, BLUE), text.col = c(INK, BLUE), lwd = 1.4, pch = 16, pt.cex = 0.5, bty = "n",
           inset = c(0.02, 0), text.font = 2)
    panel_title(title)
  }
  draw_panel(cycle_times, L("(a) Finding the cycle, seconds", "(a) D\u00f6ng\u00fcn\u00fcn bulunmas\u0131, saniye"),
             L("Cycle length", "D\u00f6ng\u00fc uzunlu\u011fu"))
  draw_panel(gen_times, L("(b) Generating n numbers, seconds", "(b) n say\u0131 \u00fcretme, saniye"), "n")
})
