# Writes every number and table of the project note from ../figures/*.csv, so that the text cannot drift away
# from the analysis. Run from inside paper/:  Rscript make_paper.R
#
# Output, once in Turkish (suffix _tr, for lcg_notu.tex) and once in English (suffix _en, for lcg_note.tex):
# numbers_xx.tex (one macro per quoted number) and tab_*_xx.tex (table bodies, each ending with its bottom
# rule, because a rule cannot follow \input directly inside a tabular). Turkish numbers use the decimal comma
# and the full stop as thousands separator; English numbers the decimal point and the comma. Turkish
# characters in strings are written as \u escapes, so that the script runs the same in every locale.

# The tables contain Turkish characters; switch to a UTF-8 character locale if the session does not use one.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "tr_TR.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
}

fig <- function(name) read.csv(file.path("..", "figures", paste0(name, ".csv")), stringsAsFactors = FALSE)

# Write lines as UTF-8 bytes whatever the session locale.
write_utf8 <- function(lines, file) writeLines(enc2utf8(lines), file, useBytes = TRUE)

TEXT <- list(
  tr = list(
    names = c("V1 example 1" = "S\u00fcr\u00fcm 1, \u00f6rnek 1", "V1 example 2" = "S\u00fcr\u00fcm 1, \u00f6rnek 2",
              "V1 example 3" = "S\u00fcr\u00fcm 1, \u00f6rnek 3", "RANDU" = "RANDU",
              "MINSTD (Park-Miller)" = "MINSTD (Park--Miller)", "rand48 parameters" = "rand48 parametreleri",
              "Mersenne-Twister (R)" = "Mersenne-Twister (R)"),
    yes = "evet", no = "hay\u0131r", short = "periyot k\u0131sa",
    method = c("Brent's algorithm" = "Brent", "Multiplicative order" = "$a$'n\u0131n mertebesi",
               "Hull-Dobell theorem" = "Hull--Dobell"),
    big = ".", dec = "{,}"),
  en = list(
    names = c("V1 example 1" = "Version 1, example 1", "V1 example 2" = "Version 1, example 2",
              "V1 example 3" = "Version 1, example 3", "RANDU" = "RANDU",
              "MINSTD (Park-Miller)" = "MINSTD (Park--Miller)", "rand48 parameters" = "rand48 parameters",
              "Mersenne-Twister (R)" = "Mersenne-Twister (R)"),
    yes = "yes", no = "no", short = "period too short",
    method = c("Brent's algorithm" = "Brent", "Multiplicative order" = "order of $a$",
               "Hull-Dobell theorem" = "Hull--Dobell"),
    big = "{,}", dec = ".")
)

build <- function(lang) {
  txt <- TEXT[[lang]]
  int <- function(x) gsub("~", txt$big, format(round(x), big.mark = "~", scientific = FALSE, trim = TRUE))
  dec <- function(x, d) sub(".", txt$dec, formatC(x, format = "f", digits = d), fixed = TRUE)
  pval <- function(p) ifelse(is.na(p), "--", ifelse(p < 0.001, paste0("$<$0", txt$dec, "001"), dec(p, 3)))

  macros <- list()
  def <- function(name, value) macros[[name]] <<- value

  # Analysis 1: period
  periods <- fig("table1_periods")
  row_of <- function(df, label) df[df$generator == label, ]
  ex1 <- row_of(periods, "V1 example 1")
  ex2 <- row_of(periods, "V1 example 2")
  ex3 <- row_of(periods, "V1 example 3")
  def("ExOnePeriod", int(ex1$period))
  def("ExOneShare", dec(100 * ex1$share_of_m, 2))
  def("ExTwoTail", int(ex2$tail))
  def("ExTwoPeriod", int(ex2$period))
  def("ExTwoShare", dec(100 * ex2$share_of_m, 2))
  def("ExTwoGcd", int(ex2$gcd_a_m))
  def("ExThreePeriod", int(ex3$period))

  v1 <- fig("table3_v1_outputs")
  def("VOneStepTwo", int(v1$v1_repeat_step[v1$generator == "V1 example 2"]))

  readme <- fig("table2_readme_example")
  def("ReadmeStated", paste(readme$stated_in_v1_readme[!is.na(readme$stated_in_v1_readme)], collapse = ", "))
  def("ReadmeComputed", paste(readme$computed, collapse = ", "))

  # Analysis 2: lattice
  spectral <- fig("table7_spectral_2d")
  s1 <- row_of(spectral, "V1 example 1")
  s2 <- row_of(spectral, "V1 example 2")
  def("NuOne", dec(s1$nu2, 1))
  def("NuOneNorm", dec(s1$normalized, 3))
  def("NuOneSpacing", dec(s1$line_spacing, 5))
  def("NuTwo", dec(s2$nu2, 1))
  def("NuTwoNorm", dec(s2$normalized, 3))
  def("NuTwoSpacing", dec(s2$line_spacing, 5))

  # Analysis 3: empirical tests
  battery <- fig("table4_test_battery")
  def("BatteryN", int(battery$n[1]))
  def("BatteryTests", int(nrow(battery)))
  def("BatteryExpectedFalse", dec(nrow(battery) * 0.01, 2))
  def("BatteryRejections", int(sum(battery$reject_at_1pct)))
  def("BatteryMinP", dec(min(battery$p_value), 3))
  def("BatteryMinPGen", txt$names[[battery$generator[which.min(battery$p_value)]]])

  serial3 <- fig("table5_serial3d")
  randu3 <- row_of(serial3, "RANDU")
  def("SerialN", int(serial3$n[1]))
  def("RanduChi", int(randu3$statistic))
  def("SerialDf", int(randu3$df))

  planes <- fig("table6_randu_planes")
  def("RanduTriples", int(planes$triples))
  def("RanduPlanes", int(planes$distinct_planes))
  def("RanduPlaneMin", int(planes$min_index))
  def("RanduPlaneMax", int(planes$max_index))

  # Analysis 4: running time
  perf <- fig("table8_performance")
  cyc <- perf[perf$task == "cycle detection", ]
  gen <- perf[perf$task == "generation", ]
  last_v1 <- cyc[max(which(!is.na(cyc$v1_seconds))), ]
  def("PerfCycleSize", int(last_v1$size))
  def("PerfCycleVOne", dec(last_v1$v1_seconds, 2))
  def("PerfCycleVTwo", dec(last_v1$v2_seconds, 3))
  def("PerfCycleRatio", int(last_v1$v1_seconds / last_v1$v2_seconds))
  def("PerfCycleMax", int(max(cyc$size)))
  def("PerfCycleMaxTime", dec(cyc$v2_seconds[which.max(cyc$size)], 1))
  big <- gen[which.max(gen$size), ]
  def("PerfGenVOne", dec(big$v1_seconds, 2))
  def("PerfGenVTwo", dec(big$v2_seconds, 2))
  def("PerfGenRatio", dec(big$v1_seconds / big$v2_seconds, 1))
  def("PerfMachine", sub("R version ([0-9.]+).*", "R \\1", perf$machine[1]))

  # Order of growth: least-squares slope of log(time) on log(size), for cycle lengths of at least 4096 (shorter
  # runs are dominated by fixed overheads).
  slope <- function(size, secs) {
    keep <- !is.na(secs) & size >= 4096
    unname(coef(lm(log(secs[keep]) ~ log(size[keep])))[2])
  }
  def("SlopeVOne", dec(slope(cyc$size, cyc$v1_seconds), 2))
  def("SlopeVTwo", dec(slope(cyc$size, cyc$v2_seconds), 2))

  write_utf8(sprintf("\\newcommand{\\%s}{%s}", names(macros), unlist(macros)), paste0("numbers_", lang, ".tex"))

  # Table 1: period
  num <- function(x) as.numeric(gsub(",", "", x))
  hd <- function(x) ifelse(is.na(x), "--", ifelse(x, txt$yes, txt$no))
  m_tex <- ifelse(periods$m_factorised == "2147483647", "2^{31}-1",
                  gsub("\\^(\\d+)", "^{\\1}", gsub(" \\* ", "\\\\cdot ", periods$m_factorised)))
  period_tex <- ifelse(periods$period > 1e7, sprintf("$2^{%d}$", round(log2(periods$period))), int(periods$period))
  period_tex[grepl("MINSTD", periods$generator)] <- "$2^{31}-2$"
  lines <- sprintf("%s & %s & %s & $%s$ & %s & %s & %s & %s & %s & %s \\\\",
                   txt$names[periods$generator], int(num(periods$a)), int(num(periods$c)), m_tex,
                   hd(periods$hd_c_coprime), hd(periods$hd_prime_factors), hd(periods$hd_four),
                   int(periods$tail), period_tex, txt$method[periods$method])
  write_utf8(c(lines, "\\bottomrule"), paste0("tab_periods_", lang, ".tex"))

  # Table 2: test p-values
  tests <- unique(battery$test)
  gens <- unique(battery$generator)
  body <- sapply(gens, function(g) {
    p <- sapply(tests, function(t) battery$p_value[battery$generator == g & battery$test == t])
    r <- serial3[serial3$generator == g, ]
    last <- if (nrow(r) == 0) "" else if (is.na(r$p_value)) txt$short else pval(r$p_value)
    paste(txt$names[[g]], paste(sprintf("& %s", pval(p)), collapse = " "), "&", last, "\\\\")
  })
  write_utf8(c(body, "\\bottomrule"), paste0("tab_battery_", lang, ".tex"))
}

for (lang in c("tr", "en")) build(lang)
cat("Wrote numbers_*.tex, tab_periods_*.tex and tab_battery_*.tex (tr, en).\n")
