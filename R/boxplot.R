#' Boxplot of \code{microbenchmark} timings.
#'
#' @param x A \code{microbenchmark} object.
#' @param unit Unit in which the results be plotted.
#' @param log Should times be plotted on log scale?
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param horizontal Switch X and Y axes.
#' @param ... Passed on to boxplot.formula.
#' 
#' @method boxplot microbenchmark
#' 
#' @author Olaf Mersmann
boxplot.microbenchmark <- function(x, unit="t", log=TRUE, xlab, ylab, 
                                   horizontal=FALSE, ...) {
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

  if (log) {
    # min time cannot be 0 for log y-axis
    ylim <- pmax(1, range(x$time))
  } else {
    ylim <- NULL
  }
  if (horizontal) { 
    ll <- if (log) "x" else ""
    boxplot(time ~ expr, data=x, xlab=ylab, ylab=xlab, log=ll, ylim=ylim,
            horizontal=TRUE, ...)
  } else {
    ll <- if (log) "y" else ""
    boxplot(time ~ expr, data=x, xlab=xlab, ylab=ylab, log=ll, ylim=ylim, ...)
  }
}
