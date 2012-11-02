##' Print \code{microbenchmark} timings.
##' 
##' The available units are nanoseconds (\code{"ns"}), microseconds
##' (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"})
##' and evaluations per seconds (\code{"eps"}) and relative runtime
##' compared to the best median time (\code{"relative"}).
##'
##' @param x An object of class \code{microbenchmark}.
##' @param unit What unit to print the timings in.
##' @param order If present, order results according to this column of the output.
##' @param ... Passed to \code{print.data.frame}
##'
##' @seealso \code{\link{boxplot.microbenchmark}} for a plot method.
##'
##' @S3method print microbenchmark
##' @method print microbenchmark
##' @author Olaf Mersmann \email{olafm@@p-value.net}
print.microbenchmark <- function(x, unit="t", order=NULL, ...) {
  s <- summary(x, unit=unit)
  cat("Unit: ", attr(s, "unit"), "\n", sep="")
  if (!missing(order)) {
    if (order %in% colnames(s)) {
      s <- s[order(s[[order]]), ]
    } else {
      warning("Cannot order results by", order, ".")
    }
  }
  print(s, ..., row.names=FALSE)
}
