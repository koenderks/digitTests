\name{digitTests-package}
\alias{digitTests}
\alias{digitTests-package}
\docType{package}
\title{digitTests: Tests for Detecting Irregular Data Patterns}
\description{
  \if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}

  \code{digitTests} is an \R package providing tests for detecting irregular data patterns.
  
  The package and its analyses are also implemented with a graphical user interface in the Audit module of 
  \href{https://jasp-stats.org}{JASP}, a free and open-source statistical software program.
  
}

\author{
  \tabular{ll}{
    Koen Derks (maintainer, author)\tab <k.derks@nyenrode.nl> \cr
  }
 
  Please use the citation provided by \R when citing this package. 
  A BibTex entry is available from \code{citation("digitTests")}.
}

\seealso{
Useful links:
\itemize{
  \item The \href{https://github.com/koenderks/digitTests/issues}{issue page} to submit a bug report or feature request.
}
}

\examples{

# Load the digitTests package
library(digitTests)

############################################
### Example 1: Benford's Law ####
############################################

data('sinoForest')
distr.test(sinoForest$value, check = 'first', reference = 'benford')

###################################
### Example 2: Repeated Values ####
###################################

data('sanitizer')
rv.test(sanitizer$value, check = 'lasttwo', method = 'af', B = 1000)

}
\keyword{digitTests} 
\keyword{package}