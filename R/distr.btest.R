#' Bayesian Test of Digits against a Reference Distribution
#'
#' @description This function extracts and performs a Bayesian test of the distribution of (leading) digits in a vector against a reference distribution. By default, the distribution of leading digits is checked against Benford's law.
#'
#' @usage distr.btest(x, check = 'first', reference = 'benford', 
#'             alpha = NULL, BF10 = TRUE, log = FALSE)
#' 
#' @param x              a numeric vector.
#' @param check          location of the digits to analyze. Can be \code{first}, \code{firsttwo}, or \code{last}.
#' @param reference      which character string given the reference distribution for the digits, or a vector of probabilities for each digit. Can be \code{benford} for Benford's law, \code{uniform} for the uniform distribution. An error is given if any entry of \code{reference} is negative. Probabilities that do not sum to one are normalized.
#' @param alpha          a numeric vector containing the prior parameters for the Dirichlet distribution on the digit categories.
#' @param BF10           logical. Whether to whether to compute the Bayes factor in favor of the alternative hypothesis (BF10) or the null hypothesis (BF01).
#' @param log            logical. Whether to return the logarithm of the Bayes factor.
#'
#' @details Benford's law is defined as \eqn{p(d) = log10(1/d)}. The uniform distribution is defined as \eqn{p(d) = 1/d}.
#'
#' @details The Bayes Factor \eqn{BF_{10}} quantifies how much more likely the data are to be observed under \eqn{H_{1}}: the digits are not distributed according to the reference distribution than under \eqn{H_{0}}: the digits are distributed according to the reference distribution. Therefore, \eqn{BF_{10}} can be interpreted as the relative support in the observed data for \eqn{H_{1}} versus \eqn{H_{0}}. If \eqn{BF_{10}} is 1, there is no preference for either \eqn{H_{1}} or \eqn{H_{0}}. If \eqn{BF_{10}} is larger than 1, \eqn{H_{1}} is preferred. If \eqn{BF_{10}} is between 0 and 1, \eqn{H_{0}} is preferred. The Bayes factor is calculated using the Savage-Dickey density ratio.
#'
#' @return An object of class \code{dt.distr} containing:
#' 
#' \item{observed}{the observed counts.}
#' \item{expected}{the expected counts under the null hypothesis.}
#' \item{n}{the number of observations in \code{x}.}
#' \item{statistic}{the value the chi-squared test statistic.}
#' \item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{check}{checked digits.}
#' \item{digits}{vector of digits.}
#' \item{reference}{reference distribution}
#' \item{data.name}{a character string giving the name(s) of the data.}
#' 
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Benford, F. (1938). The law of anomalous numbers. \emph{In Proceedings of the American Philosophical Society}, 551-572.
#' 
#' @seealso \code{\link{distr.test}} \code{\link{rv.test}}
#' 
#' @keywords benford distribution Bayes factor
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#'
#' # Bayesian digit analysis against Benford's law
#' distr.btest(x, check = 'first', reference = 'benford')
#'
#' # Bayesian digit analysis against Benford's law, custom prior
#' distr.btest(x, check = 'first', reference = 'benford', alpha = 9:1)
#'
#' # Bayesian digit analysis against custom distribution
#' distr.btest(x, check = 'last', reference = rep(1/9, 9))
#' 
#' @export

distr.btest <- function(x, check = 'first', reference = 'benford',
                        alpha = NULL, BF10 = TRUE, log = FALSE) {
  
  if (!(check %in% c("first", "last", "firsttwo")))
    stop("Specify a valid input for the check argument.")
  
  dname <- deparse(substitute(x))
  
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  
  d <- extract_digits(x, check = check, include.zero = FALSE)
  d <- d[!is.na(d)]
  n <- length(d)
  
  d_tab <- table(d)
  dig <- if (check == "firsttwo") 10:99 else 1:9
  
  obs <- rep(0, length(dig))
  d_included <- as.numeric(names(d_tab))
  index <- if (check == "firsttwo") d_included - 9 else d_included
  obs[index] <- as.numeric(d_tab)
  
  p_obs <- obs / n
  
  if (is.numeric(reference)) {
    if (length(reference) != length(dig))
      stop("The number of elements in reference must be equal to the number of possible digits.")
    if (any(reference < 0))
      stop("All elements in reference must be positive.")
    p_exp <- reference / sum(reference)
  } else if (reference == "benford") {
    p_exp <- log10(1 + 1 / dig) # Benfords law: log_10(1 + 1 / d)
  } else if (reference == "uniform") {
    p_exp <- rep(1 / length(dig), length(dig))
  } else {
    stop("Specify a valid input for the reference argument.")
  }
  
  exp <- n * p_exp
  
  if (is.null(alpha)) {
    alpha <- rep(1, length(dig))
  } else if (length(alpha) != length(dig)) {
    stop("The number of elements in alpha must be equal to the number of possible digits.")
  }
  
  # compute Bayes factor
  lbeta.xa <- sum(lgamma(alpha + obs)) - lgamma(sum(alpha + obs)) # Prior with alpha[i] counts for each digit
  lbeta.a  <- sum(lgamma(alpha)) - lgamma(sum(alpha))
  
  # in this case, counts*log(thetas) should be zero, omit to avoid numerical issue with log(0)
  if (any(rowSums(cbind(p_exp, obs)) == 0)) {
    log_bf10 <- (lbeta.xa-lbeta.a)
  } else {
    log_bf10 <- (lbeta.xa-lbeta.a) + (0 - sum(obs * log(p_exp)))
  }
  
  bf <- exp(log_bf10)
  if (!BF10)
    bf <- 1 / bf
  if (log)
    bf <- log(bf)
  
  bf_type <- if (BF10) "BF10" else "BF01"
  names(n) <- "n"
  names(bf) <- "bf"
  names(obs) <- dig
  names(exp) <- dig
  
  result <- list(observed = obs,
                 expected = exp,
                 n = n,
                 bf = bf,
                 bf_type = bf_type,
                 log = log,
                 check = check,
                 digits = dig,
                 reference = reference,
                 data.name = dname)
  class(result) <- c(class(result), 'dt.distr')
  
  return(result)
}
