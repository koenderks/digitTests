[![CRAN](https://img.shields.io/cran/v/digitTests?color=yellow&label=CRAN&logo=r)](https://cran.r-project.org/package=digitTests)
[![R_build_status](https://github.com/koenderks/digitTests/workflows/Build/badge.svg)](https://github.com/koenderks/digitTests/actions)
[![Codecov](https://codecov.io/gh/koenderks/digitTests/branch/master/graph/badge.svg?token=ZoxIB8p8PW)](https://codecov.io/gh/koenderks/digitTests)
[![Bugs](https://img.shields.io/github/issues/koenderks/digitTests/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/digitTests/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![Monthly](https://cranlogs.r-pkg.org/badges/digitTests?color=blue)](https://cranlogs.r-pkg.org)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/digitTests?color=blue)](https://cranlogs.r-pkg.org)

# digitTests: Tests for Detecting Irregular Digit Patterns

<img src='https://github.com/koenderks/digitTests/raw/master/man/figures/logo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

`digitTests` is an R package providing statistical tests for detecting irregular digit patterns. Such irregular digit patterns can be an indication of potential data manipulation or fraud. Therefore, the type of tests that the package provides can be useful in (but not limited to) the field of auditing to assess whether data have potentially been tampered with. However, please note that real data will never be perfect, and therefore caution should be used when relying on the statistical decision metrics that the package provides.

The package is also implemented with a graphical user interface in the Audit module of [JASP](https://jasp-stats.org), a free and open-source statistical software program.

## Overview

For complete documentation of the `digitTests` package download the [package manual](https://cran.r-project.org/package=digitTests/digitTests.pdf).

1. [Installation](#1-installation)
2. [Benchmarks](#2-benchmarks)
3. [Intended usage](#3-intended-usage)
4. [References](#4-references)

## 1. Installation

The most recently released version of `digitTests` can be downloaded from [CRAN](https://cran.r-project.org/package=digitTests) by running the following command in R:

```r
install.packages('digitTests')
```

Alternatively, you can download the development version from GitHub using:

```r
devtools::install_github('koenderks/digitTests')
```

After installation, the package can be loaded with:

```r
library(digitTests)
```

## 2. Benchmarks

To validate the statistical results, `digitTests`'s automated [unit tests](https://github.com/koenderks/digitTests/tree/master/tests/testthat) regularly verify the main output from the package against the following benchmarks:

- [benford.analysis](https://cran.r-project.org/package=benford.analysis) (R package version 0.1.5)
- [BenfordTests](https://cran.r-project.org/package=BenfordTests) (R package version 1.2.0)
- [BeyondBenford](https://cran.r-project.org/package=BeyondBenford) (R package version 1.4)

## 3. Intended usage

### Function: `extract_digits()`

The workhorse of the package is the `extract_digits()` function. This function takes a vector of numbers and returns the requested digits (with or without including `0`'s).

*Full function with default arguments:*

```r
extract_digits(x, check = 'first', include.zero = FALSE)
```

*Supported options for the `check` argument:*

| `check` | Returns |
| :----------- | :----------- |
| `fist` | First digit |
| `firsttwo` | First and second digit |
| `before` | All digits before the decimal separator (`.`) |
| `after` | All digits after the decimal separator (`.`) |
| `lasttwo` | Last two digits |
| `last` | Last digit |

*Example:*

```r
x <- c(0.00, 0.20, 1.23, 40.00, 54.04)
extract_digits(x, check = 'first', include.zero = FALSE)
# [1] NA  2  1  4  5
```

### Functions: `distr.test()` & `distr.btest()`

The functions `distr.test()` and `distr.btest()` take a vector of numeric values, extract the requested digits, and compares the frequencies of these digits to a reference distribution. The function `distr.test()` performs a frequentist hypothesis test of the null hypothesis that the digits are distributed according to the reference distribution and produces a *p* value. The function `distr.btest()` performs a Bayesian hypothesis test of the null hypothesis that the digits are distributed according to the reference distribution against the alternative hypothesis (using the prior parameters specified in `alpha`) that the digits are not distributed according to the reference distribution and produces a Bayes factor (Kass & Raftery, 1995). The possible options for the `check` argument are taken over from `extract_digits()`.

*Full function with default arguments:*

```r
distr.test(x, check = 'first', reference = 'benford')
distr.btest(x, check = 'first', reference = 'benford', alpha = NULL, BF10 = TRUE, log = FALSE)
```

*Supported options for the `reference` argument:*

| `check` | Returns |
| :----------- | :----------- |
| `benford` | Benford's law |
| `firsttwo` | Uniform distribution |
| Vector of probabilities | Custom distribution |

*Example:*

Benford’s law (Benford, 1938) is a principle that describes a pattern in many naturally-occurring numbers. According to Benford's law, each possible leading digit *d* in a naturally occuring, or non-manipulated, set of numbers occurs with a probability:

<img src="https://latex.codecogs.com/svg.image?p(d_i)&space;=&space;\text{log}_{10}(1&space;+&space;\frac{1}{d_i})" title="p(d_i) = \text{log}_{10}(\frac{1}{d_i})" />

The distribution of leading digits in a data set of financial transaction values (e.g., the `sinoForest` data) can be extracted and tested against the expected frequencies under Benford's law using the code below.

```r
# Frequentist hypothesis test
distr.test(sinoForest$value, check = 'first', reference = 'benford')

#
# 	Digit distribution test
#
# data:  sinoForest$value
# n = 772, X-squared = 7.6517, df = 8, p-value = 0.4682
# alternative hypothesis: leading digit(s) are not distributed according to the benford distribution.

# Bayesian hypothesis test using default prior
distr.btest(sinoForest$value, check = 'first', reference = 'benford', BF10 = FALSE)

#
# 	Digit distribution test
#
# data:  sinoForest$value
# n = 772, BF01 = 6899678
# alternative hypothesis: leading digit(s) are not distributed according to the benford distribution.
```

### Function: `rv.test()`

The function `rv.test()` analyzes the frequency with which values get repeated within a set of numbers. Unlike Benford's law, and its generalizations, this approach examines the entire number at once, not only the first or last digit. For the technical details of this procedure, see Simohnsohn (2019). The possible options for the `check` argument are taken over from `extract_digits()`.

*Full function with default arguments:*

```r
rv.test(x, check = 'last', method = 'af', B = 2000)
```

*Supported options for the `method` argument:*

| `check` | Returns |
| :----------- | :----------- |
| `af` | Average frequency |
| `entropy` | Entropy |

*Example:*

In this example we analyze a data set from a (retracted) paper that describes three experiments run in Chinese factories, where workers were nudged to use more hand-sanitizer. These data were shown to exhibited two classic markers of data tampering: impossibly similar means and the uneven distribution of last digits (Yu, Nelson, & Simohnson, 2018). We can use the `rv.test()` function to test if these data also contain a greater amount of repeated values than expected if the data were not tampered with.

```r
rv.test(sanitizer$value, check = 'lasttwo', B = 5000)

#
# 	Repeated values test
#
# data:  sanitizer$value
# n = 1600, AF = 1.5225, p-value = 0.0024
# alternative hypothesis: frequencies of repeated values are greater than for random data.
```

## 4. References

- Benford, F. (1938). The law of anomalous numbers. In *Proceedings of the American Philosophical Society*, 551-572.
- Kass, R. E., & Raftery, A. E. (1995). Bayes factors. *Journal of the American Statistical Association*, *90*(430), 773-795.
- Simohnsohn, U. (2019, May 25). *Number-Bunching: A New Tool for Forensic Data Analysis*. Retrieved from [http://datacolada.org/77](http://datacolada.org/77).
- Yo, F., Nelson, L., & Simonsohn, U. (2018, December 5). *In Press at Psychological Science: A New 'Nudge' Supported by Implausible Data*. Retrieved from [http://datacolada.org/74](http://datacolada.org/74).