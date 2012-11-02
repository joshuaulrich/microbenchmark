##' Summarize \code{microbenchmark} timings.
##' 
##' The available units are nanoseconds (\code{"ns"}), microseconds
##' (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"})
##' and evaluations per seconds (\code{"eps"}) and relative runtime
##' compared to the best median time (\code{"relative"}).
##'
##' @param object An object of class \code{microbenchmark}.
##' @param unit What unit to print the timings in.
##' @param ... Passed to \code{print.data.frame}
##'
##' @return A \code{data.frame} containing the aggregated results.
##' 
##' @seealso \code{\link{print.microbenchmark}}
##' @S3method summary microbenchmark
##' @method summary microbenchmark
summary.microbenchmark <- function(object, unit = "t", ...) {
  if (unit != "relative")
    object$time <- convert_to_unit(object$time, unit)

  res <- aggregate(time ~ expr, object, function(z) c(fivenum(z), length(z)))
  res <- cbind(res$expr, as.data.frame(res$time))
  colnames(res) <- c("expr", "min", "lq", "median", "uq", "max", "neval")

  if (unit == "relative") {
    min <- res[which.min(res$median), , drop = FALSE]
    min$neval <- 1 # Ugly hack: Do not rescale neval
    res[-1] <- res[-1] / as.list(min[-1])
    attr(res, "unit") <- "relative"
  } else {    
    attr(res, "unit") <- attr(object$time, "unit")
  }
  res
}
