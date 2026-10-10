# Runs every analysis script in order, each in a fresh R process. Run from inside analysis/.

scripts <- c("01_period_analysis.R", "02_empirical_tests.R", "03_lattice.R", "04_performance.R")
for (s in scripts) {
  cat("Running", s, "\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), s)
  if (status != 0) stop(s, " failed.", call. = FALSE)
}
cat("Done. Figures and tables are in ../figures.\n")
