# Shared figure style.
#
# The style is the one used for the charts on muhammedikbalyilmaz.com: Latin Modern Roman (bundled in ../fonts;
# install it once so that the system can find it), no grid, axis lines only on the left and at the bottom, ticks
# pointing outwards, series in black and deep blue (red marks reference values), bold panel titles at the left.
#
# Axis rule: every value axis starts and ends at a labelled tick, so that the axis line never runs past its last
# tick, and there are no minor ticks. Limits come from axis_limits() (linear) or log10_limits() (logarithmic) and
# are used with xaxs = "i" and yaxs = "i", so that the plotting region ends exactly at the first and the last
# tick; the axes are then drawn with value_axis() or log10_axis().
#
# Every figure is drawn twice: in English (figures/*.png and figures/*.pdf, used by the README and the English
# note) and in Turkish (figures/tr/*.pdf, used by the Turkish note), with the decimal comma in the Turkish
# version. Figures are 4.75 inches wide, the width of the text in the notes.
#
# Author: Muhammed Ikbal Yilmaz

FIG_DIR <- file.path("..", "figures")
FONT_FAMILY <- "Latin Modern Roman"
FIG_WIDTH <- 4.75
POINTSIZE <- 7.5

INK <- "#000000"
MUTED <- "#7a7a7a"
BAND <- "#c9d4ec"
BLUE <- "#0033a0"
RED <- "#d7191c"

FIG_LANG <- "en"

#' Pick the English or the Turkish version of a label, depending on the figure being drawn.
L <- function(en, tr) if (FIG_LANG == "tr") tr else en

#' Format a whole number with the thousands separator of the current figure language.
fig_int <- function(x) format(x, big.mark = L(",", "."), scientific = FALSE, trim = TRUE)

#' Format a tick value with the decimal mark of the current figure language and a typographic minus sign.
fig_num <- function(x) {
  s <- trimws(formatC(x, format = "fg", digits = 10, drop0trailing = TRUE))
  if (FIG_LANG == "tr") s <- gsub(".", ",", s, fixed = TRUE)
  gsub("-", "\u2212", s, fixed = TRUE)
}

# Graphics devices. Cairo is preferred; on macOS it needs XQuartz, so if cairo cannot be loaded the Quartz
# devices are used (macOS), and otherwise the standard png() and pdf() devices with a serif face. The last
# option cannot encode every Turkish letter (for example the dotless i), so the Turkish figures are complete
# only with cairo or Quartz.
device_kind <- local({
  kind <- NULL
  function() {
    if (is.null(kind)) {
      cairo_ok <- tryCatch({
        f <- tempfile(fileext = ".pdf")
        grDevices::cairo_pdf(f, width = 2, height = 2)
        on.exit(grDevices::dev.off(), add = TRUE)
        graphics::plot.new()
        graphics::text(0.5, 0.5, "x")
        TRUE
      }, warning = function(w) FALSE, error = function(e) FALSE)
      kind <<- if (cairo_ok) "cairo" else if (capabilities("aqua")) "quartz" else "base"
      if (kind != "cairo") message("Cairo graphics not available; using the ", kind, " devices.")
    }
    kind
  }
})

open_png <- function(file, width, height) {
  switch(device_kind(),
         cairo = grDevices::png(file, width = width, height = height, units = "in", res = 300, type = "cairo",
                                family = FONT_FAMILY, pointsize = POINTSIZE),
         quartz = grDevices::png(file, width = width, height = height, units = "in", res = 300, type = "quartz",
                                 family = FONT_FAMILY, pointsize = POINTSIZE),
         base = grDevices::png(file, width = width, height = height, units = "in", res = 300,
                               pointsize = POINTSIZE))
}

open_pdf <- function(file, width, height) {
  switch(device_kind(),
         cairo = grDevices::cairo_pdf(file, width = width, height = height, family = FONT_FAMILY,
                                      pointsize = POINTSIZE),
         quartz = grDevices::quartz(type = "pdf", file = file, width = width, height = height, family = FONT_FAMILY,
                                    pointsize = POINTSIZE),
         base = grDevices::pdf(file, width = width, height = height, family = "Times", pointsize = POINTSIZE))
}

# The base pdf() device only knows its built-in font families.
plot_family <- function() if (device_kind() == "base") "serif" else FONT_FAMILY

apply_par <- function() {
  par(family = plot_family(), cex = 1, cex.axis = 7 / 7.5, cex.lab = 1, cex.main = 8 / 7.5, font.main = 2,
      col.axis = INK, col.lab = INK, fg = INK, las = 1, tcl = -0.3, mgp = c(1.9, 0.4, 0),
      mar = c(3, 3.6, 1.6, 1.6), bty = "n", lwd = 0.7, xaxs = "i", yaxs = "i")
}

#' Limits and ticks of a linear value axis: from the multiple of `step` at or below the smallest value to the
#' multiple at or above the largest.
axis_limits <- function(values, step) {
  v <- values[is.finite(values)]
  lo <- floor(min(v) / step + 1e-9) * step
  hi <- ceiling(max(v) / step - 1e-9) * step
  if (hi <= lo) hi <- lo + step
  list(lim = c(lo, hi), at = seq(lo, hi, by = step))
}

#' Limits and ticks of a logarithmic axis: powers of ten around the values, every `every` decades.
log10_limits <- function(values, every = 1) {
  v <- values[is.finite(values) & values > 0]
  a <- floor(log10(min(v)) + 1e-9)
  b <- ceiling(log10(max(v)) - 1e-9)
  b <- a + every * max(1, ceiling((b - a) / every))
  list(lim = 10^c(a, b), at = 10^seq(a, b, by = every))
}

#' Linear value axis with labels in the figure language; the axis line runs from the first to the last tick.
value_axis <- function(side, at) {
  axis(side, at = at, labels = vapply(at, fig_num, ""), lwd = 0.7, lwd.ticks = 0.7)
}

#' Logarithmic axis with tick labels written as powers of ten.
log10_axis <- function(side, at) {
  minus <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u2212" else "-"   # true minus sign where the locale allows it
  labels <- do.call(expression, lapply(round(log10(at)), function(k)
    if (k < 0) bquote(10^{.(minus) * .(-k)}) else bquote(10^.(k))))
  axis(side, at = at, labels = labels, lwd = 0.7, lwd.ticks = 0.7)
}

#' Bold panel title at the left.
panel_title <- function(text) title(main = text, adj = 0, line = 0.5)

#' Label written at the end of a line, in the colour of the line.
end_label <- function(x, y, text, col) {
  text(x, y, text, pos = 4, offset = 0.3, col = col, font = 2, xpd = NA)
}

#' The heading broken into lines no wider than the figure (measured in the bold 9.5 pt face).
wrap_heading <- function(text, width) {
  f <- tempfile(fileext = ".pdf")
  open_pdf(f, width, 2)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  par(family = plot_family(), font = 2)
  plot.new()
  out <- character(0)
  for (w in strsplit(text, " ", fixed = TRUE)[[1]]) {
    trial <- if (length(out)) paste(out[length(out)], w) else w
    if (length(out) && strwidth(trial, units = "inches", cex = 9.5 / POINTSIZE) > 0.97 * width) {
      out <- c(out, w)
    } else if (length(out)) {
      out[length(out)] <- trial
    } else {
      out <- w
    }
  }
  out
}

#' Draw `plot_fun` in both languages. English: `name`.png (300 dpi, with `heading`) and `name`.pdf (vector,
#' without heading). Turkish: tr/`name`.pdf (vector, without heading: in the note the caption carries the
#' title). `heading` is evaluated after the language is set, so it may use L().
save_figure <- function(name, plot_fun, width = FIG_WIDTH, height = 2.6, heading = NULL) {
  dir.create(file.path(FIG_DIR, "tr"), showWarnings = FALSE, recursive = TRUE)
  old_dec <- getOption("OutDec")
  on.exit({ FIG_LANG <<- "en"; options(OutDec = old_dec) }, add = TRUE)
  draw <- function(lines) {
    apply_par()
    if (length(lines)) par(oma = c(0, 0, 1.45 * length(lines) + 0.3, 0))
    plot_fun()
    for (i in seq_along(lines)) {
      mtext(lines[i], side = 3, outer = TRUE, adj = 0, line = 1.45 * (length(lines) - i) + 0.2,
            cex = 9.5 / POINTSIZE, font = 2)
    }
  }

  FIG_LANG <<- "en"
  options(OutDec = ".")
  lines <- if (is.null(heading)) character(0) else wrap_heading(if (is.function(heading)) heading() else heading,
                                                                width)
  extra <- if (length(lines)) (1.45 * length(lines) + 0.3) * 1.2 * POINTSIZE / 72 else 0
  open_png(file.path(FIG_DIR, paste0(name, ".png")), width, height + extra)
  draw(lines)
  grDevices::dev.off()
  open_pdf(file.path(FIG_DIR, paste0(name, ".pdf")), width, height)
  draw(character(0))
  grDevices::dev.off()

  FIG_LANG <<- "tr"
  options(OutDec = ",")
  open_pdf(file.path(FIG_DIR, "tr", paste0(name, ".pdf")), width, height)
  draw(character(0))
  invisible(grDevices::dev.off())
}

#' Write a data frame as CSV to the figures folder.
save_table <- function(df, name) {
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, file.path(FIG_DIR, paste0(name, ".csv")), row.names = FALSE)
  invisible(df)
}
