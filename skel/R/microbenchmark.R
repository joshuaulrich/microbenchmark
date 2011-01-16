##' @useDynLib microbenchmark do_microtiming
{}

##' Nanosecond accurate timing of expression evaluation.
##'
##' \code{nanobenchmark} serves as a more accurate replacement of the
##' often seen \code{system.time(replicate(1000, expr))}
##' expression. It tries hard to accurately measure only the time it
##' takes to evaluate \code{expr}. To achieved this, the
##' sub-millisecond (supposedly nanosecond) accurate timing functions
##' most modern operating systems provide are used. Additionally all
##' evaluations of the expressions are done in C code to minimze any
##' overhead.
##'
##' This function is only meant for micro-benchmarking small pieces of
##' source code and to compare their relative performance
##' characteristics. You should generally avoid benchmarking larger
##' chunks of your code using this function. Instead, try using the R
##' profiler to detect hot spots and consider rewriting them in C/C++
##' or FORTRAN.
##'
##' @note Depending on the underlying operating system, different
##' methods are used for timing. On Windows the
##' \code{QueryPerformanceCounter} interface is used to measure the
##' time passed. For Linux the \code{clock_gettime} API is used and on
##' Solaris the \code{gethrtime} function. Finally on MacOS X the,
##' undocumented, \code{mach_absolute_time} function is used to avoid
##' a dependency on the CoreServices Framework.
##'
##' Before evaluating each expression \code{times} times, the overhead
##' of calling the timing functions and the C function call overhead
##' are estimated. This estimated overhead is subtracted from each
##' measured evaluation time. Should the resulting timing be negative,
##' a warning is thrown and the respective value is replaced by
##' \code{NA}.
##' 
##' @param ... Expressions to benchmark.
##' @param list List of unevaluated expression to benchmark.
##' @param times Number of times to evaluate the expression.
##'
##' @return Object of class \sQuote{microbenchmark}, a matrix with one
##'   column per exoression. Each row contains the time it took to
##'   evaluate the respective expression one time in nanoseconds.
##'
##' @seealso \code{\link{print.microbenchmark}} to display,
##' \code{\link{boxplot.microbenchmark}} or
##' \code{\link{ggplot.microbenchmark}} to plot and
##' \code{\link{as.data.frame.microbenchmark}} or
##' \code{\link{melt.microbenchmark}} to convert \code{microbenchmark}
##' objects.
##' 
##' @examples
##' ## Measure the time it takes to dispatch a simple function call
##' ## compared to simply evaluating the constant \code{NULL}
##' f <- function() NULL
##' res <- microbenchmark(NULL, f(), times=1000L)
##'
##' ## Print results:
##' print(res)
##'
##' ## Plot results:
##' boxplot(res)
##'
##' ## Pretty plot:
##' if (require("ggplot2")) {
##'   plt <- ggplot(res)
##'   plt <- plt + geom_line() + scale_y_log10()
##'   print(plt)
##' }
##' 
##' @export
##' @author Olaf Mersmann \email{olafm@@datensplitter.ner}
microbenchmark <- function(..., list=NULL, times=100L) {
  stopifnot(times == as.integer(times))

  exprs <- as.list(match.call()[-1])
  exprs$list <- NULL
  exprs$times <- NULL
  exprs <- c(exprs, list)
  nm <- names(exprs)
  nm[nm == ""] <- as.character(exprs)[nm == ""]
  names(exprs) <- nm

  ## GC first
  gc(FALSE)

  ## Run benchmark (returns cumulative time)
  res <- matrix(0, nrow=times, ncol=length(exprs))
  for (i in 1:length(exprs)) {
    res[, i] <- .Call(do_microtiming, as.integer(times),
                      exprs[[i]], parent.frame())
  }
  colnames(res) <- names(exprs)
  structure(res, class="microbenchmark")
}

.convert_to_unit <- function(x, unit=c("ns", "ms", "eps", "slowdown", "speedup")) {
  unit <- match.arg(unit)
  if (unit == "ns") {
    x
  } else if (unit == "ms") {
    x / 1000
  } else if (unit == "eps") {
    1e9 / x
  } else if (unit == "slowdown") {
     sweep(x, 2, apply(x, 2, min), "/")
  } else if (unit == "speedup") {
    1/sweep(x, 2, apply(x, 2, max), "/")
  } else {
    stop("Unknown unit '", unit, "'.")
  }
}

##' Print nanosecound timings.
##' 
##' The available units are nanoseconds (\code{"ns"}), milliseconds
##' (\code{"ms"}), evaluations per seconds (\code{"eps"}), slowdown
##' compared to the fastest expression (\code{"slowdown"}) and speedup
##' compared to slowest expression (\code{"speedup"}).
##'
##' @param x An object of class \code{microbenchmark}.
##' @param unit What unit to print the timings in.
##' @param ... Ignored.
##'
##' @S3method print microbenchmark
##' @method print microbenchmark
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
print.microbenchmark <- function(x, unit=c("ns", "ms", "eps", "slowdown", "speedup"), ...) {
  unit <- match.arg(unit)
  res <- t(apply(x, 2, fivenum))
  colnames(res) <- c("min", "lq", "median", "uq", "max")
  if (unit == "ns") {
    cat("Unit: nanoeconds\n")
  } else if (unit == "ms") {
    cat("Unit: milliseconds\n")
    res <- res / 1000
  } else if (unit == "eps") {
    cat("Unit: evaluations per second\n")
    res <- 1e9 / res
  } else if (unit == "slowdown") {
    cat("Unit: relative slowdown\n")
    res <- sweep(res, 2, apply(res, 2, min), "/")
  } else if (unit == "speedup") {
    cat("Unit: relative speedup\n")
    res <- 1/sweep(res, 2, apply(res, 2, max), "/")
  }
  print(res)
}

##' Convert / melt a \code{microbenchmark} object into a
##' \code{data.frame} ready to be \code{\link{cast}}.
##'
##' @param data A \code{microbenchmark} object.
##' @param x A \code{microbenchmark} object.
##' @param unit Unit in which the results be plotted.
##' @param ... Ignored.
##' @return A \code{data.frame} with columns \sQuote{run},
##'   \sQuote{expr} and \sQuote{value}, containing the run number,
##'   expression (as a string) and the time in nanoseconds.
##'
##' @S3method melt microbenchmark
##' @method melt microbenchmark
##' @importFrom reshape melt
##'
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
melt.microbenchmark <- function(data,
                                unit=c("ns", "ms", "eps", "slowdown", "speedup"),
                                ...) {
  m <- melt(unclass(.convert_to_unit(data, unit)))
  colnames(m) <- c("run", "expr", "value")
  m
}

##' @return The same \code{data.frame} returned by \code{melt}.
##' 
##' @S3method as.data.frame microbenchmark
##' @method as.data.frame microbenchmark
##' @importFrom reshape melt
##' @rdname melt.microbenchmark
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
as.data.frame.microbenchmark <- function(x, ...)
  melt(x, ...)

##' Boxplot of \code{microbenchmark} timings.
##'
##' @param x A \code{microbenchmark} object.
##' @param unit Unit in which the results be plotted.
##' @param log Should times be plotted on log scale?
##' @param xlab X axes label.
##' @param ylab Y axes label.
##' @param ... Passed on to boxplot.formula.
##' 
##' @S3method boxplot microbenchmark
##' @method boxplot microbenchmark
##'
##' @importFrom graphics boxplot
##' 
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
boxplot.microbenchmark <- function(x,
                                   unit=c("ns", "ms", "eps", "slowdown", "speedup"),
                                   log=TRUE, xlab, ylab, ...) {
  unit <- match.arg(unit)
  data <- as.data.frame(.convert_to_unit(x, unit))
  if (missing(xlab))
    xlab <- "Expression"
  if (missing(ylab)) {
    ylab <- if (log) {
      if (unit %in% c("ns", "ms"))
        paste("log(time) [", unit, "]", sep="")
      else
        paste("log(", unit, ")", sep="")
    } else {
      if (unit == "ns")
        "time [ns]"
      else if (unit == "ms")
        "time [ms]"
      else if (unit == "eps")
        "Evaluations per second"
      else if (unit == "slowdown")
        "Relative slowdown"
      else if (unit == "speedup")
        "Relative speedup"
    }
  }
  ll <- if (log) "y" else ""
  boxplot(value ~ expr, data=data, xlab=xlab, ylab=ylab, log=ll, ...)
}

##' ggplot method for \code{microbenchmark} objects.
##'
##' @param data A \code{microbenchmark} object.
##' @param mapping Default list of aesthetic mappings.
##' @param ... Passed to \code{\link{ggplot.data.frame}}.
##' @S3method ggplot microbenchmark
##' @method ggplot microbenchmark
##'
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 ggplot
##' 
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
ggplot.microbenchmark <- function(data,
                                  mapping=aes_string(x="run", y="value", colour="expr"),
                                  ...) {
  ndata <- as.data.frame(data)
  ggplot(ndata, mapping, ...)
}
