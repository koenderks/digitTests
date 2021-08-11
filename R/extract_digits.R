#' Extraction of First or Last Digits
#'
#' @description This function extracts the first (and optionally second) or last digits in a vector.
#'
#' @usage extract_digits(x, check = 'first', include.zero = FALSE)
#' 
#' @param x       a numeric vector.
#' @param check         location of the digits to extract. Can be \code{first}, \code{firsttwo}, or \code{last}.
#' @param include.zero  logical. Whether to include the digit zero in the output.
#'
#' @return A vector of first (and optionally second) or last digits.
#' 
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @keywords extract digits
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#'
#' # Extract first digits (without zero)
#' extract_digits(x, check = 'first')
#'
#' # Extract last digits (including zero)
#' extract_digits(x, check = 'last', include.zero = TRUE)
#' 
#' @export

extract_digits <- function(x, check = 'first', include.zero = FALSE) {
  
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  
  if (check == "first") {
    if (include.zero) {
      # Simply take the first character of the string of absolute x
      digits <- as.numeric(substring(abs(x), 1, 1))
    } else {
      # Take the first character of the scientific value of x
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 1))
      digits[x == 0] <- NA
    }
  } else if (check == "firsttwo") {
    if (include.zero) {
      x <- formatC(abs(x), digits = 10, format = "f")
      x <- gsub(x = x, pattern = "[.]", replacement = "")
      digits <- as.numeric(substring(x, 1, 2))
    } else {
      # Take the first and second characters of the scientific value of x * 10
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 3)) * 10
      digits[x == 0] <- NA
    }
  } else if (check == "before") {
    if (include.zero) {
      # Take the floored/ceiled numbers
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
    } else {
      # Take the floored/ceiled numbers, but do not show the zeroes
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
      digits[digits == 0] <- NA
    }
  } else if (check == "after") {
    if (include.zero) {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x)%%1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, 3, nchar(stringedX)))
    } else {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x)%%1, drop0trailing = TRUE)
      digits <- as.numeric(substring(stringedX, 3, nchar(stringedX)))
      digits[x%%2 == 0] <- NA
    }
  } else if (check == "lasttwo") {
    if (include.zero) {
      stringedX <- format(abs(x)%%1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
    } else {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x)%%1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
      digits[x%%2 == 0] <- NA
    }
  } else if (check == "last") {
    if (include.zero) {
      x <- formatC(x, digits = 2, format = "f")
      digits <- as.numeric(substring(x, nchar(x), nchar(x)))
    } else {
      # Remove trailing zeroes from the number
      stringedX <- sub("0+$", "", as.character(abs(x)))
      digits <- as.numeric(substring(stringedX, nchar(stringedX), nchar(stringedX)))
      digits[x == 0] <- NA # Integers become NA
    }
  } else {
    stop("Specify a valid input for the check argument.")
  } 
  return(digits)
}
