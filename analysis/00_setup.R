# Common set-up for the analysis scripts. Run every script from inside analysis/ (paths are relative).

for (f in list.files(file.path("..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

# Generators studied. The first three are the examples of version 1 of this repository; the others are
# reference points from the literature.
GENERATORS <- data.frame(
  id    = c("v1_ex1", "v1_ex2", "v1_ex3", "randu", "minstd", "rand48"),
  label = c("V1 example 1", "V1 example 2", "V1 example 3", "RANDU", "MINSTD (Park-Miller)", "rand48 parameters"),
  a     = c(1079, 1722, 11, 65539, 16807, 25214903917),
  c     = c(863, 990, 0, 0, 0, 11),
  m     = c(7919219, 9969219, 32, 2^31, 2^31 - 1, 2^48),
  seed  = c(1, 1, 27, 1, 1, 1),
  stringsAsFactors = FALSE
)

generator <- function(id) as.list(GENERATORS[GENERATORS$id == id, ])

# Functions of version 1, loaded without running the examples at the end of that file.
load_v1 <- function() {
  env <- new.env()
  for (e in parse(file.path("..", "legacy", "lcg_v1.R"))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      eval(e, env)
    }
  }
  env
}

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
