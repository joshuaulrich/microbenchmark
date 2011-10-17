##' @useDynLib microbenchmark do_microtiming do_microtiming_precision
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
##' The \code{control} list can contain the following entries:
##' \describe{
##' \item{order}{the order in which the expressions are evaluated.
##'   \dQuote{random} (the default) randomizes the execution order,
##'   \dQuote{inorder} executes each expression in order and
##'   \dQuote{block} executes all repetitions of each expression
##'     as one block.}
##' \item{warmup}{the number of warm-up iterations performed before
##'   the actual benchmark. These are used to estimate the timing
##'   overhead as well as spinning up the processor from any sleep
##'   or idle states it might be in. The default value is 2^}
##' }
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
##' @seealso \code{\link{print.microbenchmark}} to display and
##' \code{\link{boxplot.microbenchmark}} to plot the results.
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
##'   plt <- ggplot2::qplot(y=time, data=res, colour=expr)
##'   plt <- plt + ggplot2::scale_y_log10()
##'   print(plt)
##' }
##' 
##' @export
##' @author Olaf Mersmann \email{olafm@@datensplitter.ner}
microbenchmark <- function(..., list=NULL, times=100L, control=list()) {
  stopifnot(times == as.integer(times))

  control[["warmup"]] <- coalesce(control[["warmup"]], 2^18L)
  control[["order"]] <- coalesce(control[["order"]], "random")

  stopifnot(as.integer(control$warmup) == control$warmup)
  
  exprs <- as.list(match.call()[-1])
  exprs$list <- NULL
  exprs$times <- NULL
  exprs$control <- NULL
  
  exprs <- c(exprs, list)
  nm <- names(exprs)
  exprnm <- sapply(exprs, deparse)
  if (is.null(nm))
    nm <- exprnm
  else
    nm[nm == ""] <- exprnm[nm == ""]
  names(exprs) <- nm

  ## GC first
  gc(FALSE)

  o <- if (control$order == "random")
    sample(rep(seq_along(exprs), times=times))
  else if (control$order == "inorder")
    rep(seq_along(exprs), times=times)
  else if (control$order == "block")
    rep(seq_along(exprs), each=times)
  else
    stop("Unknown ordering. Must be one of 'random', 'inorder' or 'block'.")
  exprs <- exprs[o]

  res <- .Call(do_microtiming, exprs, parent.frame(), as.integer(control$warmup))
  
  ## Sanity check. Fail as early as possible if the results are
  ## rubbish.
  if (all(is.na(res)))
    .all_na_stop()

  res <- data.frame(expr=nm[o], time=res)
  class(res) <- c("microbenchmark", class(res))
  res
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
##' @seealso \code{\link{boxplot.microbenchmark}} for a plot method.
##'
##' @S3method print microbenchmark
##' @method print microbenchmark
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
print.microbenchmark <- function(x, unit="t", ...) {
  x$time <- convert_to_unit(x$time, unit)
  res <- aggregate(time ~ expr, x, fivenum)
  res <- cbind(res$expr, as.data.frame(res$time))
  colnames(res) <- c("expr", "min", "lq", "median", "uq", "max")
  cat("Unit: ", attr(x$time, "unit"), "\n", sep="")
  print(res, unit="t", ...)
}

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

##' Internal helper functions that returns a generic error if timings fail.
.all_na_stop <- function() {
  msg <- "All measured timings are NA. This is bad!

There are several causes for this. The most likely are

 * You are running under a hypervisor. This can do all sorts of things
   to the timing functions of your operating system.

 * You have frequency scaling turned on. Most modern CPUs can reduce
   their core frequency if they are not busy. microbenchmark tries
   hard to spin up the CPU before the actual timing, but there is no
   guarantee this works, so you are advised to disable this
   feature. Under Linux this can be done using the 'cpufreq'
   utilities.

 * You have a machine with many CPU cores and the timers provided by
   your operating system are not synchronized across cores. Your best
   bet is to peg your R process to a single core. On Linux systems,
   this can be achieved using the 'taskset' utility.

 * Your machine is super fast. If the difference between the estimated
   overhead and the actual execution time is zero (or possibly even
   negative), you will get this error. Sorry,
   you're out of luck, benchmark complexer code.

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
##' \item{\dQuote{t}}{Appropriately prefixed time unit.}
##' \item{\dQuote{hz}}{Hertz / evaluations per second.}
##' \item{\dQuote{eps}}{Evaluations per second / Hertz.}
##' \item{\dQuote{khz}}{Kilohertz / 1000s of evaluations per second.}
##' \item{\dQuote{mhz}}{Megahertz / 1000000s of evaluations per second.}
##' \item{\dQuote{f}}{Appropriately prefixed frequency unit.}
##' }
##' 
##' @param x An \code{microbenchmark} object.
##' @param unit A unit of time. See details.
##'
##' @return A matrix containing the converted time values with an
##'   attribute \code{unit} which is a printable name of the unit of
##'   time.
##'
##' @author Olaf Mersmann \email{olafm@@datensplitter.net}
convert_to_unit <- function(x, unit=c("ns", "us", "ms", "s", "t", "hz", "khz", "mhz", "eps", "f")) {
  unit <- match.arg (unit)

  switch (unit,
          t = unit <- sprintf ("%ss", find_prefix (x * 1e-9, minexp = -9, maxexp = 0, mu = FALSE)),
          f = unit <- sprintf ("%shz", find_prefix (1e9 / x, minexp =  0, maxexp = 6, mu = FALSE))
          )
  
  switch (unit,
          ns  = {attr(x, "unit") <- "nanoseconds"           ; unclass(x      )},
          us  = {attr(x, "unit") <- "microseconds"          ; unclass(x / 1e3)},
          ms  = {attr(x, "unit") <- "milliseconds"          ; unclass(x / 1e6)}, 
          s   = {attr(x, "unit") <- "seconds"               ; unclass(x / 1e9)},
          eps = {attr(x, "unit") <- "evaluations per second"; unclass(1e9 / x)},
          hz  = {attr(x, "unit") <- "hertz"                 ; unclass(1e9 / x)},
          khz = {attr(x, "unit") <- "kilohertz"             ; unclass(1e6 / x)},
          mhz = {attr(x, "unit") <- "megahertz"             ; unclass(1e3 / x)},
          stop("Unknown unit '", unit, "'.")
          )
}

##' Find SI prefix for unit
##'
##' 
##' @param x a numeric
##' @param f function that produces the number from \code{x} that is used to determine the
##' prefix, e.g. \code{\link[base]{min}} or \code{\link[stats]{median}}.
##' @param minexp minimum (decimal) exponent to consider, 
##'   e.g. -3 to suppress prefixes smaller than milli (m).
##' @param maxexp maximum (decimal) exponent to consider, 
##'   e.g. 3 to suppress prefixes larger than kilo (k).
##' @param mu if \code{TRUE}, should a proper mu is used for micro, otherwise use
##'  u as ASCII-compatible replacement
##'
##' @return character with the SI prefix
##' @author Claudia Beleites 
find_prefix <- function (x, f = min, minexp = -Inf, maxexp = Inf, mu = TRUE){
 
  prefixes <- c ("y", "z", "a", "f", "p", "n", "u", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y")
  if (mu) prefixes [7] <- "\u03bc"

  if (is.numeric (minexp)) minexp <- floor (minexp / 3) 
  if (is.numeric (minexp)) maxexp <- floor (maxexp / 3) 
  
  e3 <- floor (log10 (f (x)) / 3)
  e3 <- max (e3, minexp, -8) # prefixes go from 10^-24 = 10^(3 * -8)
  e3 <- min (e3, maxexp,  8) # to 10^24 = 10^(3 * 8)

  prefixes [e3 + 9] # e3 of -8 => index 1
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
coalesce <- function(...) {
  isnotnull <- function(x) !is.null(x)
  Find(isnotnull, list(...))
}

##' Estimate precision of timing routines.
##'
##' This function is currently experimental. Its main use is to judge
##' the quality of the underlying timer implementation of the
##' operating system. The function measures the overhead of timing a C
##' function call \code{rounds} times and returns all non-zero timings
##' observed. This can be used to judge the granularity and resolution
##' of the timing subsystem.
##' 
##' @param rounds Number of measurements used to estimate the precision.
##' @param warmup Number of iterations used to warmup the CPU.
##' @return A vector of observed non-zero timings.
##'
##' @author Olaf Mersmann \email{olafm@@statistik.tu-dortmund.de}
##' @export
microtiming_precision <- function(rounds=100L, warmup=2^18) {
  .Call(do_microtiming_precision, parent.frame(),
        as.integer(rounds),
        as.integer(warmup))
}
