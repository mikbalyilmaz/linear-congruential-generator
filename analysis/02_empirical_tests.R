# Empirical tests of uniformity and independence.
#
# Experiment A: the full battery on n = 10,000 numbers from each generator, a sample shorter than every
# period except that of V1 example 3 (period 8), which is left out.
# Experiment B: the three-dimensional serial test on 300,000 numbers with a finer grid, which only generators
# with a period longer than the sample can take. R's default Mersenne-Twister (Matsumoto and Nishimura, 1998)
# is the benchmark in both experiments. The results are reported as tables (no figure: a plot of the p-values
# would repeat the table).

source("00_setup.R")

N_A <- 10000
N_B <- 300000
ALPHA <- 0.01

draw <- function(id, n) {
  if (id == "mt") {
    set.seed(1, kind = "Mersenne-Twister")
    return(runif(n))
  }
  g <- generator(id)
  lcg_uniform(n, g$a, g$c, g$m, g$seed)
}
label_of <- function(id) if (id == "mt") "Mersenne-Twister (R)" else GENERATORS$label[GENERATORS$id == id]

ids_a <- c("v1_ex1", "v1_ex2", "randu", "minstd", "rand48", "mt")
battery <- do.call(rbind, lapply(ids_a, function(id) {
  res <- run_test_battery(draw(id, N_A))
  cbind(generator = label_of(id), n = N_A, res)
}))
battery$reject_at_1pct <- battery$p_value < ALPHA
save_table(battery, "table4_test_battery")
print(battery)

periods <- read.csv(file.path(FIG_DIR, "table1_periods.csv"))
ids_b <- c("v1_ex1", "v1_ex2", "randu", "minstd", "rand48", "mt")
serial3 <- do.call(rbind, lapply(ids_b, function(id) {
  period <- if (id == "mt") Inf else periods$period[periods$generator == label_of(id)]
  if (period < N_B) {
    return(data.frame(generator = label_of(id), n = N_B, test = "Serial, 3-tuples (20^3 cells)",
                      statistic = NA, df = NA, p_value = NA,
                      note = sprintf("period %s is shorter than the sample", fmt_int(period))))
  }
  cbind(generator = label_of(id), n = N_B, test_serial(draw(id, N_B), dim = 3, cells = 20), note = "")
}))
save_table(serial3, "table5_serial3d")
print(serial3)

# RANDU's planes: for every triple, 9 u[k] - 6 u[k+1] + u[k+2] is an integer (Marsaglia, 1968).
g <- generator("randu")
x <- lcg_generate(N_B, g$a, g$c, g$m, g$seed)
k <- seq_len(N_B - 2)
identity_holds <- all((9 * x[k] - 6 * x[k + 1] + x[k + 2]) %% g$m == 0)
plane_index <- (9 * x[k] - 6 * x[k + 1] + x[k + 2]) / g$m
planes <- data.frame(
  triples = length(k),
  identity_holds_for_all = identity_holds,
  distinct_planes = length(unique(plane_index)),
  min_index = min(plane_index), max_index = max(plane_index)
)
save_table(planes, "table6_randu_planes")
print(planes)

ids_fig <- c("randu", "minstd")
values <- lapply(ids_fig, function(id) {
  u <- draw(id, N_B)
  9 * u[k] - 6 * u[k + 1] + u[k + 2]
})

# Why this figure: the three-dimensional serial test only says that RANDU's triples are not uniform. The
# histogram shows why: the combination 9u(k) - 6u(k+1) + u(k+2) takes integer values only, i.e. every triple
# lies on one of a few parallel planes. MINSTD, with the same modulus size, is shown for comparison.
save_figure("fig2_randu_planes", height = 2.5,
            heading = function() L("Figure 2. Value of 9u(k) - 6u(k+1) + u(k+2) over 299,998 consecutive triples (bins of width 0.05)",
                                   "\u015eekil 2. 299.998 ard\u0131\u015f\u0131k \u00fc\u00e7l\u00fcde 9u(k) - 6u(k+1) + u(k+2) de\u011feri (0,05 geni\u015fli\u011finde s\u0131n\u0131flar)"), function() {
  par(mfrow = c(1, 2))
  x_ax <- axis_limits(c(-6, 10), 2)
  for (j in seq_along(ids_fig)) {
    v <- values[[j]]
    breaks <- seq(-6, 10, by = 0.05)
    h <- hist(v, breaks = breaks, plot = FALSE)
    counts <- h$counts / 1000
    y_ax <- axis_limits(c(0, counts), if (max(counts) > 10) 10 else 0.5)
    plot(NA, xlim = x_ax$lim, ylim = y_ax$lim, xlab = "9u(k) - 6u(k+1) + u(k+2)",
         ylab = L("Triples, thousands", "\u00dc\u00e7l\u00fc say\u0131s\u0131, bin"), axes = FALSE)
    value_axis(1, x_ax$at)
    value_axis(2, y_ax$at)
    col <- if (j == 1) INK else BLUE
    rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], counts, col = col, border = col, lwd = 0.3)
    panel_title(sprintf("(%s) %s", letters[j], label_of(ids_fig[j])))
  }
})
