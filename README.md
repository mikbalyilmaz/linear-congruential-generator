# Linear Congruential Generator in R: period, lattice structure and empirical tests

Version 2 of an R implementation of the linear congruential generator (LCG)

$$x_{n+1} = (a\,x_n + c) \bmod m, \qquad u_n = x_n / m,$$

with exact arithmetic, constant-memory cycle detection, jump-ahead, the period theory of Hull and Dobell and of the
multiplicative case, a two-dimensional spectral test and a small battery of empirical tests. The analysis applies these
tools to the three examples of version 1 and to three generators from the literature, and a short project note (in
English and Turkish) explains why each analysis was done and what it shows. Base R only; no packages are needed.

## Contents

| Path | What it is |
|---|---|
| `R/lcg_core.R` | Exact modular multiplication, generation, jump-ahead (Brown, 1994), tail and cycle lengths (Brent, 1980) |
| `R/lcg_theory.R` | Prime factorisation, Hull–Dobell conditions, Carmichael function, multiplicative order, two-dimensional spectral test |
| `R/lcg_tests.R` | Equidistribution, Kolmogorov–Smirnov, serial (pairs, triples), serial correlation, runs up and down |
| `R/plot_style.R` | Shared figure style (typeface, colours, `save_figure()` helper) |
| `tests/run_tests.R` | Unit tests against brute-force searches, plain loops and exact reference values |
| `analysis/01_period_analysis.R` | Period theory and exact tail and cycle lengths; audit of version 1 |
| `analysis/02_empirical_tests.R` | Test battery; three-dimensional serial test; RANDU's planes (Figure 2) |
| `analysis/03_lattice.R` | Spectral test; successive pairs of example 1 against the Mersenne-Twister (Figure 1) |
| `analysis/04_performance.R` | Running time of version 1 and version 2 (Figure 3) |
| `analysis/run_all.R` | Runs the four scripts in order |
| `figures/` | Every figure as PNG (300 dpi) and PDF (vector), every reported table as CSV; `figures/tr/` holds the Turkish versions of the figures |
| `paper/lcg_note.pdf`, `paper/lcg_notu.pdf` | Project note in English and in Turkish; `make_paper.R` writes all their numbers and tables from `figures/*.csv` |
| `legacy/lcg_v1.R` | Version 1, unchanged, used by the audit and the timing comparison |
| `fonts/` | Latin Modern Roman, the typeface of the note and the figures (GUST Font License) |

## Usage

```r
source("R/lcg_core.R")
source("R/lcg_theory.R")

lcg_generate(5, a = 1079, c = 863, m = 7919219, seed = 1)
#> 1942 2096281 4910647 631465 298764

lcg_cycle(a = 1722, c = 990, m = 9969219, seed = 1)
#>     mu lambda
#>      2  14076

hull_dobell(a = 1079, c = 863, m = 7919219)$full_period
#> FALSE

multiplicative_order(16807, 2^31 - 1)   # MINSTD: period 2^31 - 2
#> 2147483646
```

`lcg_uniform()` returns $u_n = x_n / m$. All functions accept moduli up to $2^{51}$ and are exact over that whole range.

## How to run

```
Rscript tests/run_tests.R
cd analysis
Rscript run_all.R                       # about 4 minutes, most of it in 04_performance.R
cd ../paper
Rscript make_paper.R
xelatex lcg_note.tex && xelatex lcg_note.tex   # English; needs a TeX installation with XeLaTeX
xelatex lcg_notu.tex && xelatex lcg_notu.tex   # Turkish
```

Run the analysis scripts from inside `analysis/` (paths are relative). Seeds are fixed. Figures use Latin Modern Roman if it
is installed on the system (install the files in `fonts/`); otherwise they fall back to the default serif typeface. The
figures are drawn with cairo where it is available; on macOS without XQuartz the Quartz devices are used instead.

## Theory in brief

**Period.** Every orbit is eventually periodic: after a tail of $\mu$ values it enters a cycle of $\lambda$ values. For
$c \neq 0$ the period equals $m$ for every seed if and only if (Hull and Dobell, 1962)

1. $c$ and $m$ are coprime,
2. $a - 1$ is divisible by every prime factor of $m$,
3. $a - 1$ is divisible by 4 if $m$ is.

The theorem does not apply to the multiplicative case $c = 0$. There, for a seed coprime to $m$, the period is the
multiplicative order of $a$ modulo $m$, at most the Carmichael function $\lambda(m)$ (Knuth, 1997, Section 3.2.1.2); for
$m = 2^e$, $e \geq 3$, this is $2^{e-2}$.

**Lattice structure.** Successive pairs $(u_n, u_{n+1})$ lie on parallel lines at most $1/\nu_2$ apart, where $\nu_2$ is
the length of the shortest non-zero $(s_1, s_2)$ with $s_1 + a s_2 \equiv 0 \pmod m$ (spectral test; Knuth, 1997,
Section 3.3.4). Since $(-a, 1)$ always qualifies, a small multiplier forces close lines far apart. In three dimensions RANDU
($a = 65539$, $c = 0$, $m = 2^{31}$) puts every triple on one of 15 planes (Marsaglia, 1968).

## Main results

| | Example 1 | Example 2 | Example 3 |
|---|---|---|---|
| Parameters $(a, c, m)$ | (1079, 863, 7919219) | (1722, 990, 9969219) | (11, 0, 32) |
| Tail, cycle | 0, 56,826 | 2, 14,076 | 0, 8 |
| Cycle as share of $m$ | 0.72 % | 0.14 % | 25 % (the maximum, $\lambda(32) = 8$) |
| Reason | Condition 2 fails | Conditions 1 and 2 fail; $\gcd(a, m) = 3$ | Multiplicative case |

- All six generators pass six classical tests on 10,000 numbers. On 300,000 numbers a serial test of triples rejects
  RANDU ($\chi^2 = 80{,}064$ on 7,999 degrees of freedom); the first two examples cannot take that test, because their
  cycles are shorter than the sample.
- Finding a cycle of length 32,768 takes 2.14 s with version 1 and 0.018 s with version 2 (R 4.5.1 on an Apple
  silicon Mac; times depend on the machine). Generating $10^7$ numbers takes 0.69 s in a plain loop and 0.16 s with
  blocked jumps.

Every number above is in `figures/*.csv` and in the project note.

## Changes from version 1

See [`NEWS.md`](NEWS.md). In short: version 1 compared each value with all earlier ones (quadratic time), reported the step
of the first repetition without separating tail and cycle, lost exactness when $a(m-1) \geq 2^{53}$, and described its
output by the median. Its README stated the Hull–Dobell theorem without the condition $c \neq 0$, and its worked example, which is
multiplicative, had two wrong values.

## Limitations

Moduli above $2^{51}$ (for example $2^{64}$) need big-integer arithmetic and are not supported. The spectral test is
two-dimensional and computed only for $m \leq 2^{26}$. The test battery illustrates; it does not certify a generator.
LCGs are not suitable for cryptography, and for simulation R's built-in generators are the better choice.

## References

- Brent, R. P. (1980). An improved Monte Carlo factorization algorithm. *BIT*, 20, 176–184.
- Brown, F. B. (1994). Random number generation with arbitrary strides. *Transactions of the American Nuclear Society*, 71, 202–203.
- Hull, T. E. and Dobell, A. R. (1962). Random number generators. *SIAM Review*, 4(3), 230–254.
- Knuth, D. E. (1997). *The Art of Computer Programming, Volume 2: Seminumerical Algorithms*, 3rd ed. Addison-Wesley.
- L'Ecuyer, P. (1999). Tables of linear congruential generators of different sizes and good lattice structure. *Mathematics of Computation*, 68, 249–260.
- L'Ecuyer, P. and Simard, R. (2007). TestU01: A C library for empirical testing of random number generators. *ACM Transactions on Mathematical Software*, 33, Article 22.
- Marsaglia, G. (1968). Random numbers fall mainly in the planes. *Proceedings of the National Academy of Sciences of the USA*, 61(1), 25–28.
- Matsumoto, M. and Nishimura, T. (1998). Mersenne Twister: A 623-dimensionally equidistributed uniform pseudo-random number generator. *ACM Transactions on Modeling and Computer Simulation*, 8(1), 3–30.
- Park, S. K. and Miller, K. W. (1988). Random number generators: Good ones are hard to find. *Communications of the ACM*, 31(10), 1192–1201.

## Citation

Citation metadata are in [`CITATION.cff`](CITATION.cff); GitHub shows them under "Cite this repository".

> Yılmaz, M. İ. (2026). *Linear congruential generator in R: period, lattice structure and empirical tests* (Version 2.0.0)
> [Computer software]. https://github.com/mikbalyilmaz/linear-congruential-generator

## Licence

MIT; see [`LICENSE`](LICENSE). The fonts in `fonts/` are distributed under the GUST Font License.

## Status

The project note is a draft and has not been peer reviewed.
