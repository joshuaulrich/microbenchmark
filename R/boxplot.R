#' Boxplot of \code{microbenchmark} timings.
#'
#' @param x A \code{microbenchmark} object.
#' @param unit Unit in which the results be plotted.
#' @param log Should times be plotted on log scale?
#' @param xlab X axes label.
#' @param ylab Y axes label.
#' @param ... Passed on to boxplot.formula.
#' 
#' @export
#' @method boxplot microbenchmark
#'
#' @importFrom graphics boxplot
#' 
#' @author Olaf Mersmann
boxplot.microbenchmark <- function(x, unit="t", log=TRUE, xlab, ylab, ...) {
  x$time <- convert_to_unit(x$time, unit)
  timeunits <- c("ns", "us", "ms", "s", "t")
  frequnits <- c("hz", "khz", "mhz", "eps", "f")
  
  if (missing(xlab))
    xlab <- "Expression"
  if (missing(ylab)) {
    ylab <- if (log) {
      if (unit %in% timeunits)
        paste("log(time) [", unit, "]", sep="")
      else if (unit %in% frequnits)
        paste("log(frequency) [", unit, "]", sep="")
      else
        paste("log(", unit, ")", sep="")
    } else {
      if (unit %in% timeunits)
        paste("time [", unit, "]", sep="")
      else if (unit %in% frequnits)
        paste("frequency [", unit, "]", sep="")
      else if (unit == "eps")
        "evaluations per second [Hz]"
      else
        unit
    }
  }
  ll <- if (log) "y" else ""
  
  boxplot(time ~ expr, data=x, xlab=xlab, ylab=ylab, log=ll, ...)
}
