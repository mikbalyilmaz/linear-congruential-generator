# Shared figure style.
#
# Typeface: Latin Modern Roman, the typeface of LaTeX documents (bundled in ../fonts; install it once so that
# the system can find it). If it is not installed, the devices fall back to the default serif face.
# Colours: three colour-blind-safe hues used in a fixed order.
#
# Every figure is drawn twice: in English (figures/*.png and figures/*.pdf, used by the README) and in
# Turkish (figures/tr/*.pdf, used by the project note), with the decimal comma in the Turkish version.
#
# Author: Muhammed Ikbal Yilmaz

FIG_DIR <- file.path("..", "figures")
FONT_FAMILY <- "Latin Modern Roman"

INK <- "#000000"
INK2 <- "#333333"
MUTED <- "#6f6f6f"
GRID <- "#d9d9d9"
BLUE <- "#2a78d6"
ORANGE <- "#eb6834"
AQUA <- "#1baf7a"

FIG_LANG <- "en"

#' Pick the English or the Turkish version of a label, depending on the figure being drawn.
L <- function(en, tr) if (FIG_LANG == "tr") tr else en

#' Format a whole number with the thousands separator of the current figure language.
fig_int <- function(x) format(x, big.mark = L(",", "."), scientific = FALSE, trim = TRUE)

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
                                family = FONT_FAMILY),
         quartz = grDevices::png(file, width = width, height = height, units = "in", res = 300, type = "quartz",
                                 family = FONT_FAMILY),
         base = grDevices::png(file, width = width, height = height, units = "in", res = 300))
}

open_pdf <- function(file, width, height) {
  switch(device_kind(),
         cairo = grDevices::cairo_pdf(file, width = width, height = height, family = FONT_FAMILY),
         quartz = grDevices::quartz(type = "pdf", file = file, width = width, height = height, family = FONT_FAMILY),
         base = grDevices::pdf(file, width = width, height = height, family = "Times"))
}

# The base pdf() device only knows its built-in font families.
plot_family <- function() if (device_kind() == "base") "serif" else FONT_FAMILY

apply_par <- function() {
  par(family = plot_family(), cex = 0.95, cex.axis = 0.9, cex.lab = 0.95, cex.main = 1, font.main = 1,
      col.axis = INK2, col.lab = INK, fg = INK2, las = 1, tcl = -0.25, mgp = c(2.6, 0.5, 0),
      mar = c(3.6, 4.4, 2.2, 1), bty = "l", lwd = 0.6)
}

#' Light horizontal and vertical grid lines drawn under the data.
grid_lines <- function(x = NULL, y = NULL) {
  if (!is.null(x)) abline(v = x, col = GRID, lwd = 0.5)
  if (!is.null(y)) abline(h = y, col = GRID, lwd = 0.5)
}

#' Axis for a log scale with tick labels written as powers of ten.
log_axis <- function(side, at) {
  labels <- do.call(expression, lapply(round(log10(at)), function(k) bquote(10^.(k))))
  axis(side, at = at, labels = labels)
}

#' Left-aligned panel title.
panel_title <- function(text) title(main = text, adj = 0, line = 0.6)

#' Draw `plot_fun` in both languages. English: `name`.png (300 dpi, with `heading`) and `name`.pdf (vector,
#' without heading). Turkish: tr/`name`.pdf (vector, without heading: in the note the caption carries the
#' title). `heading` is evaluated after the language is set, so it may use L().
save_figure <- function(name, plot_fun, width = 7, height = 4, heading = NULL) {
  dir.create(file.path(FIG_DIR, "tr"), showWarnings = FALSE, recursive = TRUE)
  old_dec <- getOption("OutDec")
  on.exit({ FIG_LANG <<- "en"; options(OutDec = old_dec) }, add = TRUE)
  draw <- function(with_heading) {
    apply_par()
    text <- if (is.function(heading)) heading() else heading
    if (with_heading && !is.null(text)) par(oma = c(0, 0, 1.6, 0))
    plot_fun()
    if (with_heading && !is.null(text)) mtext(text, side = 3, outer = TRUE, adj = 0, line = 0.2, cex = 1.1)
  }

  FIG_LANG <<- "en"
  options(OutDec = ".")
  open_png(file.path(FIG_DIR, paste0(name, ".png")), width, height)
  draw(TRUE)
  grDevices::dev.off()
  open_pdf(file.path(FIG_DIR, paste0(name, ".pdf")), width, height)
  draw(FALSE)
  grDevices::dev.off()

  FIG_LANG <<- "tr"
  options(OutDec = ",")
  open_pdf(file.path(FIG_DIR, "tr", paste0(name, ".pdf")), width, height)
  draw(FALSE)
  invisible(grDevices::dev.off())
}

#' Write a data frame as CSV to the figures folder.
save_table <- function(df, name) {
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, file.path(FIG_DIR, paste0(name, ".csv")), row.names = FALSE)
  invisible(df)
}
