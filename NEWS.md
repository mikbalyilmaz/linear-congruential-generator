# Changes

## Version 2.1.1

No result changed: every table and every number in the notes is the same as in version 2.1.0.

- Figure 2: the combination is written 9u(k) − 6u(k+1) + u(k+2), with a minus sign instead of a hyphen, in the heading
  and on the horizontal axis; panel (b) reads "MINSTD (Park–Miller)" with an en dash.
- Figure 3: negative powers of ten on the logarithmic axes (10⁻¹, …, 10⁻⁵) are written with a minus sign
  (`log10_axis()` in `R/plot_style.R`; a hyphen is kept where the R session does not use UTF-8).

## Version 2.1.0

No result changed: every table and every number in the notes is the same as in version 2.0.0.

- Figures redrawn in the style of the charts on muhammedikbalyilmaz.com: series in black and deep blue (#0033a0),
  red dotted lines for dates and reference values, labels at the end of the lines where possible, bold panel titles,
  Latin Modern Roman. Every figure is 4.75 inches wide, the width of the text in the notes.
- Every figure is drawn in English and in Turkish (decimal comma), as before.
- Axis rule: every value axis starts and ends at a labelled tick, and there are no minor ticks (`R/plot_style.R`:
  `axis_limits()`, `log10_limits()`, `value_axis()`, `log10_axis()`). Headings and panel titles are shortened or wrapped
  so that no text is clipped.
- `Rscript 04_performance.R --plot-only` redraws Figure 3 from the stored timings (`figures/table8_performance.csv`)
  without timing again; timings depend on the machine.
- The project notes now use the note template: 11 pt, A4, 4.5 cm side margins, a header with the short name and
  the compilation date, and the page count. Wide equations and tables were split or set in a smaller size to fit the
  narrower text block.
- References: one paragraph per entry with a 1.27 cm hanging indent, an ampersand before the last author (APA 7), full
  journal names, issue numbers where they were missing and editions in parentheses.

## Version 2.0.0

### New

- `R/` holds the library; the analysis and the tests are separate from it, and sourcing the library no longer runs examples.
- Exact modular multiplication for every modulus up to 2^51, with parameter validation.
- Generation in blocks of about sqrt(n) values, each obtained from the previous block with one vectorised jump.
- Jump-ahead by k steps in O(log k) operations (Brown, 1994).
- Tail length and cycle length with Brent's (1980) algorithm in constant memory.
- Prime factorisation, Hull–Dobell conditions, Carmichael function, multiplicative order, two-dimensional spectral test.
- Six empirical tests of uniformity and independence.
- Unit tests (`tests/run_tests.R`) against brute-force searches, plain loops and reference values computed with exact integers.
- Analysis scripts, figures (English and Turkish), CSV tables and a project note in English and Turkish whose numbers are written from the tables.
- `CITATION.cff`.

### Corrections to version 1

| # | Version 1 | Version 2 |
|---|---|---|
| 1 | Every new value was compared with all earlier ones, so detecting a cycle of length L took time proportional to L². A cycle of 32,768 took 2.14 s on the test machine. | Brent's algorithm, linear time and constant memory: 0.018 s for the same cycle. |
| 2 | The step of the first repetition was reported as the result (56,827 and 14,079 for examples 1 and 2). It mixes tail and cycle: in example 2 the repeated value 4,671,054 is x₂, not x₀. | Tail and cycle are reported separately: 0 and 56,826 for example 1; 2 and 14,076 for example 2. |
| 3 | `a * x` was formed in double precision. Results are silently wrong once a(m − 1) ≥ 2^53; with the rand48 parameters they differ from the exact sequence at the second value. | Exact for every m ≤ 2^51. |
| 4 | The output was described by its median and its number of distinct values, which say nothing about randomness. | Period theory, spectral test and empirical tests. |
| 5 | The README stated the Hull–Dobell theorem without its condition c ≠ 0, and its worked example is multiplicative (c = 0), a case the theorem does not cover. | Both cases are stated; for c = 0 the period is the multiplicative order of a, at most λ(m). |
| 6 | The README example (a = 11, c = 0, m = 32, x₀ = 27) listed 27, 9, 3, 1, 11, 23, 29. | The sequence is 27, 9, 3, 1, 11, 25, 19, 17, then 27 again: period 8. |
| 7 | In the README a `---` line placed directly under a paragraph turned that paragraph into a heading; the BibTeX block was not closed; the citation contained `<your-username>/<repo-name>` placeholders. | Corrected; citation metadata in `CITATION.cff`. |
| 8 | The file name contained spaces. | Files renamed; version 1 is kept unchanged as `legacy/lcg_v1.R`. |

## Version 1.0.0

First version: `lcg()` and `analyze_sequence()` with three examples.
