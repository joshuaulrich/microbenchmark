#' Print \code{microbenchmark} timings.
#' 
#' @param x An object of class \code{microbenchmark}.
#' @param unit What unit to print the timings in. Default value taken
#'   from to option \code{microbenchmark.unit} (see example).
#' @param order If present, order results according to this column of the output.
#' @param signif If present, limit the number of significant digits shown.
#' @param ... Passed to \code{print.data.frame}
#'
#' @note The available units are nanoseconds (\code{"ns"}), microseconds
#' (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"})
#' and evaluations per seconds (\code{"eps"}) and relative runtime
#' compared to the best median time (\code{"relative"}).
#'
#' @note If the \code{multcomp} package is available a statistical
#' ranking is calculated and displayed in compact letter display from
#' in the \code{cld} column.
#' 
#' @seealso \code{\link{boxplot.microbenchmark}} and
#' \code{\link{autoplot.microbenchmark}} for a plot methods.
#'
#' @examples
#' a1 <- a2 <- a3 <- a4 <- numeric(0)
#' 
#' res <- microbenchmark(a1 <- c(a1, 1),
#'                       a2 <- append(a2, 1),
#'                       a3[length(a3) + 1] <- 1,
#'                       a4[[length(a4) + 1]] <- 1,
#'                       times=100L)
#' print(res)
#' ## Change default unit to relative runtime
#' options(microbenchmark.unit="relative")
#' print(res)
#' ## Change default unit to evaluations per second
#' options(microbenchmark.unit="eps")
#' print(res)
#'
#' @method print microbenchmark
#' @author Olaf Mersmann
print.microbenchmark <- function(x, unit, order, signif, ...) {
  s <- summary(x, unit=unit)
  timing_cols <- c("min", "lq", "median", "uq", "max", "mean")
  if (!missing(signif)) {
    s[timing_cols] <- lapply(s[timing_cols], base::signif, signif)
  }
  cat("Unit: ", attr(s, "unit"), "\n", sep="")
  if (!missing(order)) {
    if (order %in% colnames(s)) {
      s <- s[order(s[[order]]), ]
    } else {
      warning("Cannot order results by", order, ".")
    }
  }
  print(s, ..., row.names=FALSE)
  invisible(x)
}
