# Lattice structure of successive pairs (u[n], u[n+1]) and the two-dimensional spectral test.
#
# Why: a long period does not make successive numbers independent. The spectral test measures how far apart the
# lines are on which the pairs lie; wide gaps mean that whole regions of the unit square are never visited.

source("00_setup.R")

spectral <- do.call(rbind, lapply(seq_len(nrow(GENERATORS)), function(i) {
  g <- GENERATORS[i, ]
  s <- spectral_test_2d(g$a, g$m)
  data.frame(generator = g$label, a = g$a, m = g$m, nu2 = s[["nu2"]], line_spacing = s[["line_spacing"]],
             normalized = s[["normalized"]],
             note = if (is.na(s[["nu2"]])) "m > 2^26: not computed with double-precision arithmetic" else "")
}))
save_table(spectral, "table7_spectral_2d")
print(spectral)

# Why this figure: nu2 is a single number; the plot shows what it means. Over one full period of V1 example 1
# (56,826 pairs) the pairs with u(n) < 0.004 fall on a few parallel lines of slope a, at least 1 / nu2 apart.
# The same number of pairs from R's Mersenne-Twister is drawn beside it: without a lattice the points scatter.
# A narrow window is needed because the lines, about 0.001 apart, are invisible on the whole unit square.
WINDOW <- 0.004
g <- generator("v1_ex1")
period <- lcg_cycle(g$a, g$c, g$m, g$seed)[["lambda"]]
u_lcg <- lcg_uniform(period + 1, g$a, g$c, g$m, g$seed)
set.seed(1, kind = "Mersenne-Twister")
u_mt <- runif(period + 1)
window_pairs <- function(u) {
  keep <- u[-length(u)] < WINDOW
  list(x = u[-length(u)][keep], y = u[-1][keep])
}
panels <- list(window_pairs(u_lcg), window_pairs(u_mt))
cols <- c(BLUE, ORANGE)

save_figure("fig1_pairs", width = 7, height = 3.4,
            heading = function() L(sprintf("Figure 1. Successive pairs (u(n), u(n+1)) with u(n) < %s, out of %s pairs",
                                           WINDOW, fig_int(period)),
                                   sprintf("\u015eekil 1. %s \u00e7ift i\u00e7inden u(n) < %s olan ard\u0131\u015f\u0131k \u00e7iftler", fig_int(period), WINDOW)),
            function() {
  par(mfrow = c(1, 2))
  titles <- c(L("V1 example 1", "S\u00fcr\u00fcm 1, \u00f6rnek 1"),
              "Mersenne-Twister (R)")
  for (j in 1:2) {
    p <- panels[[j]]
    plot(NA, xlim = c(0, WINDOW), ylim = c(0, 1), xlab = "u(n)", ylab = "u(n+1)", xaxs = "i", yaxs = "i")
    grid_lines(x = pretty(c(0, WINDOW)), y = seq(0, 1, 0.25))
    points(p$x, p$y, pch = 16, cex = 0.35, col = cols[j])
    panel_title(sprintf("%s (%d %s)", titles[j], length(p$x), L("points", "nokta")))
  }
})
