##' @useDynLib microbenchmark do_microtiming
{}

##' Sub-millisecond accurate timing of expression evaluation.
##'
##' \code{microbenchmark} serves as a more accurate replacement of the
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
##' @param control List of control arguments. See Details.
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
##' objects and \code{\link{print.microbenchmark}},
##' \code{\link{relative_slowdown}} and \code{\link{relative_speedup}}
##' to print the results in various formats.
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
microbenchmark <- function(..., list=NULL, times=100L, control=list()) {
  stopifnot(times == as.integer(times))

  control[["warmup"]] <- coalesce(control[["warmup"]], 2^18)

  stopifnot(as.integer(control$warmup) == control$warmup)
  
  exprs <- as.list(match.call()[-1])
  exprs$list <- NULL
  exprs$times <- NULL
  exprs$control <- NULL
  
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
                      exprs[[i]], parent.frame(),
                      as.integer(control$warmup))
  }

  ## Sanity check. Fail as early as possible if the results are
  ## rubbish.
  if (all(is.na(res)))
    .all_na_stop()
  
  colnames(res) <- names(exprs)
  structure(res, class="microbenchmark")
}

##' Print \code{microbenchmark} timings.
##' 
##' The available units are nanoseconds (\code{"ns"}), microseconds
##' (\code{"us"}), milliseconds (\code{"ms"}), seconds (\code{"s"})
##' and evaluations per seconds (\code{"eps"}).
##'
##' @param x An object of class \code{microbenchmark}.
##' @param unit What unit to print the timings in.
##' @param ... Ignored.
##'
##' @seealso \code{\link{relative_slowdown}} and
##' \code{\link{relative_speedup}} to convert the absolute timings of
##' a microbenchmark into relative performance values.
##' 
##' @S3method print microbenchmark
##' @method print microbenchmark
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
print.microbenchmark <- function(x, unit="ns", ...) {
  x <- convert_to_unit(x, unit)
  res <- t(apply(x, 2, fivenum))
  colnames(res) <- c("min", "lq", "median", "uq", "max")
  cat("Unit: ", attr(x, "unit"), "\n", sep="")
  print(res)
}

##' Relative speedup / slowdown of one expression compared to another.
##'
##' @param x An object of class \code{microbenchmark}.
##' @param ... Currently ignored.
##'
##' @rdname relspeed
##' @export
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
relative_slowdown <- function(x, ...) {
  res <- t(apply(x, 2, fivenum))
  colnames(res) <- c("min", "lq", "median", "uq", "max")
  sweep(res, 2, apply(res, 2, min), "/")
}

##' @rdname relspeed
##' @export
relative_speedup <- function(x, ...) {
  res <- t(apply(x, 2, fivenum))
  colnames(res) <- c("min", "lq", "median", "uq", "max")
  1/sweep(res, 2, apply(res, 2, max), "/")
}

##' Convert / melt a \code{microbenchmark} object into a
##' \code{data.frame} ready to be \code{\link{cast}}.
##'
##' @param data A \code{microbenchmark} object.
##' @param x A \code{microbenchmark} object.
##' @param unit Unit in which the results should be plotted.
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
melt.microbenchmark <- function(data, unit="ns", ...) {
  m <- melt(convert_to_unit(data, unit))
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
boxplot.microbenchmark <- function(x, unit="ns", log=TRUE, xlab, ylab, ...) {
  data <- melt(x, unit)
  timeunits <- c("ns", "us", "ms", "s")
  frequnits <- c("hz", "khz", "mhz")
  
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

##' Internal helper functions that returns a generic error if timings fail.
.all_na_stop <- function() {
  msg <- "All measured timings are NA. This is bad!

There are several causes for this. The most likely are

 * You are running under a hypervisor. This can do all sorts of things
   to the timing functions of your operating system.

 * You have frequency scaling turned on. Most modern CPUs can reduce
   their core frequency if they are not busy. microbenchmark tries
   hard to spin up the CPU before the actual timing, but there is no
   guarantee this work. You might want to disable this feature. Under
   Linux this can be done using the 'cpufreq' utilities.

 * You have a machine with many CPU cores and the timers provided by
   your operating system are not synchronized across cores. Best bet
   it to peg your R package to a single core. On Linux systems, this
   can be achieved using 'taskset'.

 * Your machine is super fast. If the difference between the estimated
   overhead and the actual execution time is zero (or possibly even
   negative), you will get this error. Sorry, your out of luck,
   benchmark complexer code.

If this problem persists for you, please contact me and I will try to
resolve the issue with you."

  stop(msg, call.=FALSE)
}

##' Convert timings to different units.
##'
##' The following units of time are supported \describe{
##' \item{\dQuote{ns}}{Nanoseconds.}
##' \item{\dQuote{us}}{Microseconds.}
##' \item{\dQuote{ms}}{Milliseconds.}
##' \item{\dQuote{s}}{Seconds.}
##' \item{\dQuote{hz}}{Hertz / evaluations per second.}
##' \item{\dQuote{eps}}{Evaluations per second / Hertz.}
##' \item{\dQuote{khz}}{Kilohertz / 1000s of evaluations per second.}
##' \item{\dQuote{mhz}}{Megahertz / 1000000s of evaluations per second.}
##' }
##' 
##' @param x An \code{microbenchmark} object.
##' @param unit A unit of time. See details.
##' @return A matrix containing the converted time values with an
##'   attribute \code{unit} which is a printable name of the unit of
##'   time.
##'
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
convert_to_unit <- function(x,
                             unit=c("ns", "us", "ms", "s", "hz", "khz", "mhz",
                               "eps", "slowdown", "speedup")) {
  unit <- match.arg(unit)
  if (unit == "ns") {
    attr(x, "unit") <- "nanoseconds"
    unclass(x)
  } else if (unit == "us") {
    attr(x, "unit") <- "microseconds"
    unclass(x / 1e3)
  } else if (unit == "ms") {
    attr(x, "unit") <- "milliseconds"
    unclass(x / 1e6)    
  } else if (unit == "s") {
    attr(x, "unit") <- "seconds"
    unclass(x / 1e9)
  } else if (unit == "eps") {
    attr(x, "unit") <- "evaluations per second"
    unclass(1e9 / x)
  } else if (unit == "hz") {
    attr(x, "unit") <- "hertz"
    unclass(1e9 / x)
  } else if (unit == "khz") {
    attr(x, "unit") <- "kilohertz"
    unclass(1e6 / x)
  } else if (unit == "mhz") {
    attr(x, "unit") <- "megahertz"
    unclass(1e3 / x)
} else {
    stop("Unknown unit '", unit, "'.")
  }
}

##' Return first non null argument.
##'
##' This function is useful when processing complex arguments with multiple
##' possible defaults based on other arguments that may or may not have been
##' provided.
##'
##' @param ... List of values.
##' @return First non null element in \code{...}.
##'
##' @author Olaf Mersmann \email{olafm@@statistik.tu-dortmund.de}
##' @export
coalesce <- function(...) {
  isnotnull <- function(x) !is.null(x)
  Find(isnotnull, list(...))
}
