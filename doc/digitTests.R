## ---- echo = F----------------------------------------------------------------
library(digitTests)

## -----------------------------------------------------------------------------
x <- c(0.00, 0.20, 1.23, 40.00, 54.04)
extract_digits(x, check = 'first', include.zero = FALSE)

## -----------------------------------------------------------------------------
# Frequentist hypothesis test
distr.test(sinoForest$value, check = 'first', reference = 'benford')

# Bayesian hypothesis test using default prior
distr.btest(sinoForest$value, check = 'first', reference = 'benford', BF10 = FALSE)

## -----------------------------------------------------------------------------
rv.test(sanitizer$value, check = 'lasttwo', B = 2000)

