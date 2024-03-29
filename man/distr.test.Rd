% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distr.test.R
\name{distr.test}
\alias{distr.test}
\title{Test of Digits against a Reference Distribution}
\usage{
distr.test(x, check = 'first', reference = 'benford')
}
\arguments{
\item{x}{a numeric vector.}

\item{check}{location of the digits to analyze. Can be \code{first}, \code{firsttwo}, or \code{last}.}

\item{reference}{which character string given the reference distribution for the digits, or a vector of probabilities for each digit. Can be \code{benford} for Benford's law, \code{uniform} for the uniform distribution. An error is given if any entry of \code{reference} is negative. Probabilities that do not sum to one are normalized.}
}
\value{
An object of class \code{dt.distr} containing:

\item{observed}{the observed counts.}
\item{expected}{the expected counts under the null hypothesis.}
\item{n}{the number of observations in \code{x}.}
\item{statistic}{the value the chi-squared test statistic.}
\item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value for the test.}
\item{check}{checked digits.}
\item{digits}{vector of digits.}
\item{reference}{reference distribution}
\item{data.name}{a character string giving the name(s) of the data.}
}
\description{
This function extracts and performs a test of the distribution of (leading) digits in a vector against a reference distribution. By default, the distribution of leading digits is checked against Benford's law.
}
\details{
Benford's law is defined as \eqn{p(d) = log10(1/d)}. The uniform distribution is defined as \eqn{p(d) = 1/d}.
}
\examples{
set.seed(1)
x <- rnorm(100)

# Digit analysis against Benford's law
distr.test(x, check = 'first', reference = 'benford')

# Digit analysis against custom distribution
distr.test(x, check = 'last', reference = rep(1/9, 9))

}
\references{
Benford, F. (1938). The law of anomalous numbers. \emph{In Proceedings of the American Philosophical Society}, 551-572.
}
\seealso{
\code{\link{distr.btest}} \code{\link{rv.test}}
}
\author{
Koen Derks, \email{k.derks@nyenrode.nl}
}
\keyword{benford}
\keyword{distribution}
