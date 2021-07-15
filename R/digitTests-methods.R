#' Methods for da objects
#'
#' Methods defined for objects returned from the \code{\link{distr.test}} and \code{\link{distr.btest}} functions.
#'
#' @param x       an object of class \code{da} as returned by one of the package functions.
#' @param digits  the number of digits to round to.
#' @param ...     further arguments, currently ignored.
#'
#' @return
#' The \code{print} methods simply print and return nothing.
#'
#' @name dt-methods
NULL

# Print methods

#' @rdname dt-methods
#' @method print dt.distr
#' @export
print.dt.distr <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Digit distribution test", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  if (!is.null(x$n)) 
    out <- c(out, paste(names(x$n), "=", format(x$n, digits = max(1L, digits - 2L))))
  if (!is.null(x$statistic)) 
    out <- c(out, paste(names(x$statistic), "=", format(x$statistic, digits = max(1L, digits - 2L))))
  if (!is.null(x$parameter)) 
    out <- c(out, paste(names(x$parameter), "=", format(x$parameter, digits = max(1L, digits - 2L))))
  if (!is.null(x$bf)) {
    label <- if (x$bf_type == "BF10") "BF10" else "BF01"
    out <- c(out, paste(label, "=", format(x$bf, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
    out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  digitLabel <- switch(x$check, "first" = "leading", "last" = "last", "firsttwo" = "first two")
  distLabel <- if (is.numeric(x$reference)) "reference" else x$reference
  cat(paste0("alternative hypothesis: ", digitLabel, " digit(s) are not distributed according to the ", distLabel, " distribution."))
  cat("\n")
  invisible(x)
}

#' @rdname dt-methods
#' @method print dt.rv
#' @export
print.dt.rv <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Repeated values test", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  if (!is.null(x$n)) 
    out <- c(out, paste(names(x$n), "=", format(x$n, digits = max(1L, digits - 2L))))
  if (!is.null(x$statistic)) 
    out <- c(out, paste(names(x$statistic), "=", format(x$statistic, digits = max(1L, digits - 2L))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
    out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  altLabel <- switch(x$alternative, "two.sided" = "different", "less" = "less", "greater" = "greater")
  cat(paste0("alternative hypothesis: frequencies of repeated values are ", altLabel, " than for random data."))
  cat("\n")
  invisible(x)
}

# Plot functions

#' @rdname dt-methods
#' @method plot dt.distr
#' @export
plot.dt.distr <- function(x, ...) {
  p_exp <- x$expected / x$n
  p_obs <- x$observed / x$n
  yTicks <- pretty(c(0, p_exp, p_obs), min.n = 4)
  plot <- graphics::barplot(p_exp, las = 1, main = "Observed vs. Expected Distribution", xlab = "Digit", ylab = "Relative frequency", 
                            names.arg = x$digits, ylim = c(0, max(yTicks)), col = "gray", axes = FALSE)
  graphics::legend("topright", legend = c("Observed", "Expected"), fill = c("blue", "gray"), bty = "n")
  xloc <- as.numeric(plot)
  graphics::lines(x = xloc, y = p_obs, cex = 2, col = "blue")
  graphics::points(x = xloc, y = p_obs, cex = if (x$check == "firsttwo") 1 else 1.5, col = "blue", pch = 19)
  graphics::axis(side = 1, at = xloc, labels = rep("", length(x$digits)), pos = -0.01)
  graphics::axis(side = 2, at = yTicks, las = 1)
}

#' @rdname dt-methods
#' @method plot dt.rv
#' @export
plot.dt.rv <- function(x, ...) {
  plot <- graphics::barplot(as.numeric(x$frequencies), las = 1, main = "Histogram with Individual Bins", ylab = "Frequency", xlab = "Value", names.arg = "")
  xloc <- as.numeric(plot)
  ticks <- pretty(xloc, min.n = 4)
  graphics::axis(side = 1, at = ticks, labels = round(seq(min(x$x), max(x$x), length.out = length(ticks)), 2))
}