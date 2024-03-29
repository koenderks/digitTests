# Copyright (C) 2021-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Test of Repeated Values
#'
#' @description This function analyzes the frequency with which values get repeated within a set of numbers. Unlike Benford's law, and its generalizations, this approach examines the entire number at once, not only the first or last digit.
#'
#' @usage rv.test(x, check = 'last', method = 'af', B = 2000)
#' 
#' @param x             a numeric vector of values from which the digits should be analyzed.
#' @param check         which digits to shuffle during the procedure. Can be \code{last} or \code{lasttwo}.
#' @param method        which property of the data is calculated. Defaults to \code{af} for average frequency, but can also be \code{entropy} for entropy.
#' @param B             how many samples to use in the bootstraping procedure.
#'
#' @details To determine whether the data show an excessive amount of bunching, the null hypothesis that \code{x} does not contain an unexpected amount of repeated values is tested against the alternative hypothesis that \code{x} has more repeated values than expected. The statistic can either be the average frequency (\eqn{AF = sum(f_i^2)/sum(f_i))} of the data or the entropy (\eqn{E = - sum(p_i * log(p_i))}, with \eqn{p_i=f_i/n}) of the data. Average frequency and entropy are highly correlated, but the average frequency is often more interpretable. For example, an average frequency of 2.5 means that, on average, your observations contain a value that appears 2.5 times in the data set.To quantify what is expected, this test requires the assumption that the integer portions of the numbers are not associated with their decimal portions.
#'
#' @return An object of class \code{dt.rv} containing:
#'
#' \item{x}{input data.}
#' \item{frequencies}{frequencies of observations in \code{x}.}
#' \item{samples}{vector of simulated samples.}
#' \item{integers}{counts for extracted integers.}
#' \item{decimals}{counts for extracted decimals.}
#' \item{n}{the number of observations in \code{x}.}
#' \item{statistic}{the value the average frequency or entropy statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{cor.test}{correlation test for the integer portions of the number versus the decimals portions of the number.}
#' \item{method}{method used.}
#' \item{check}{checked digits.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Simohnsohn, U. (2019, May 25). Number-Bunching: A New Tool for Forensic Data Analysis. Retrieved from \url{https://datacolada.org/77}.
#' 
#' @seealso \code{\link{distr.test}} \code{\link{distr.btest}}
#' 
#' @keywords repeated values
#'
#' @examples  
#' set.seed(1)
#' x <- rnorm(50)
#'
#' # Repeated values analysis shuffling last digit
#' rv.test(x, check = 'last', method = 'af', B = 2000)
#' 
#' @export

rv.test <- function(x, check = 'last', method = 'af', B = 2000) {
  
  if (!(check %in% c("last", "lasttwo", "all")))
    stop("Specify a valid input for the check argument.")
  if (!(method %in% c("af", "entropy")))
    stop("Specify a valid input for the method argument.")
  
  dname <- deparse(substitute(x))
  
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  x <- x[x != 0] # 0.0000 crashes the analysis since it is not a valid count
  n <- length(x)
  
  check <- if (check == "all") "after" else check
  integers <- extract_digits(x, check = 'before', include.zero = TRUE)
  decimals <- extract_digits(x, check = check, include.zero = TRUE)
  frequencies <- table(x)
  
  storage <- numeric(B)
  if (method == 'af') {
    statistic <- .af(x)
  } else if (method == 'entropy') {
    statistic <- .entropy(x)
  } else {
    stop("Specify a valid input for the method argument.")
  }
  
  fraction <- switch(check, "last" = 10, "lasttwo" = 100, "after" = 1)
  
  for (i in 1:B) {
    decim_samples <- sample(decimals/fraction)
    sim <- ifelse(integers < 0, yes = (x + (decimals/fraction)) - decim_samples, no = (x - (decimals/fraction)) + decim_samples)
    storage[i] <- switch(method, "af" = .af(sim), "entropy" = .entropy(sim))
  }

  pval <- switch(method, "af" = mean(storage >= statistic), "entropy" = mean(storage <= statistic))
  cortest <- try({ stats::cor.test(integers, decimals) })
  
  names(n) <- "n"
  names(statistic) <- switch(method, "af" = "AF", "entropy" = "S")
  
  if (B < 500)
    warning("the p-value may be unreliable. It is advised to increase the number of samples.")
  
  result <- list(statistic = statistic,
                 p.value = pval,
                 x = x,
                 frequencies = frequencies,
                 samples = storage,
                 integers = table(integers),
                 decimals = table(decimals),
                 n = n,
                 cor.test = if (!inherits(cortest, "try-error")) cortest else NULL,
                 check = check,
                 method = method,
                 data.name = dname)
  class(result) <- c(class(result), 'dt.rv')
  
  return(result)
}

.entropy <- function(x) {
  frequencies <- as.numeric(table(x))
  # Probailities instead of frequencies
  prob <- frequencies / sum(frequencies)
  logProb <- log(prob)
  # Compute sum(p*log(p))
  entropy <- -sum(ifelse(!is.infinite(logProb), prob*logProb, 0))
  return(entropy) 
}

.af <- function(x) {
  tx <- table(x)
  af <- sum(tx^2)/sum(tx)
  return(af)
}