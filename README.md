[![CRAN](https://img.shields.io/cran/v/digitTests?color=yellow&label=CRAN&logo=r)](https://cran.r-project.org/package=digitTests)
[![R_build_status](https://github.com/koenderks/digitTests/workflows/Build/badge.svg)](https://github.com/koenderks/digitTests/actions)
[![Codecov](https://codecov.io/gh/koenderks/digitTests/branch/master/graph/badge.svg?token=ZoxIB8p8PW)](https://codecov.io/gh/koenderks/digitTests)
[![Bugs](https://img.shields.io/github/issues/koenderks/digitTests/bug?label=Bugs&logo=github&logoColor=%23FFF&color=brightgreen)](https://github.com/koenderks/digitTests/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![Monthly](https://cranlogs.r-pkg.org/badges/digitTests?color=blue)](https://cranlogs.r-pkg.org)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/digitTests?color=blue)](https://cranlogs.r-pkg.org)

# digitTests: Tests for Detecting Irregular Digit Patterns

<img src='https://github.com/koenderks/digitTests/raw/master/man/figures/logo.png' width='149' height='173' alt='logo' align='right' margin-left='20' margin-right='20'/>

`digitTests` is an R package providing statistical tests for detecting irregular digit patterns.

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

To validate the statistical results, `digitTests`'s automated [unit tests](https://github.com/koenderks/digitTests/tree/master/tests/testthat) regularly verify the main output from the package against the following benchmark(s):

- [benford.analysis](https://cran.r-project.org/package=benford.analysis) (R package version 0.1.5)

## 3. Intended usage

### `distr.test()` & `distr.btest()`

The functions `distr.test()` and `distr.btest()` 

**Example: Assessing Benford's law**

Benford’s law is a principle that describes a pattern in many naturally-occurring numbers. The `distr.test()` function can be used to extract the first (using `check = 'first'`), last (using `check = 'last'`), or first and second (using `check = 'firsttwo'`) digits from the values in `x`.

```r
data(sinoForest)
x <- distr.test(sinoForest$value, check = 'first', reference = 'benford')

x       # Gives output of chi-squared test
plot(x) # Produces a plot of observed vs. expected distribution
```

### `rv.test()`

The function `rv.test()` analyzes the frequency with which values get repeated within a set of numbers. Unlike Benford's law, and its generalizations, this approach examines the entire number at once, not only the first or last digit. For the technical details of this procedure, see 

## References

- Benford, F. (1938). The law of anomalous numbers. In Proceedings of the American Philosophical Society, 551-572.
- Simohnsohn, U. (2019, May 25). Number-Bunching: A New Tool for Forensic Data Analysis. Retrieved from http://datacolada.org/77.