## Internal utility functions

# Internal helper functions that returns a generic error if timings fail.
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
   you're out of luck, benchmark more complex code.

If this problem persists for you, please contact me and I will try to
resolve the issue with you."

  stop(msg, call.=FALSE)
}

#' Convert timings to different units.
#'
#' The following units of time are supported \describe{
#' \item{\dQuote{ns}}{Nanoseconds.}
#' \item{\dQuote{us}}{Microseconds.}
#' \item{\dQuote{ms}}{Milliseconds.}
#' \item{\dQuote{s}}{Seconds.}
#' \item{\dQuote{t}}{Appropriately prefixed time unit.}
#' \item{\dQuote{hz}}{Hertz / evaluations per second.}
#' \item{\dQuote{eps}}{Evaluations per second / Hertz.}
#' \item{\dQuote{khz}}{Kilohertz / 1000s of evaluations per second.}
#' \item{\dQuote{mhz}}{Megahertz / 1000000s of evaluations per second.}
#' \item{\dQuote{f}}{Appropriately prefixed frequency unit.}
#' }
#' 
#' @param x An \code{microbenchmark} object.
#' @param unit A unit of time. See details.
#'
#' @return A matrix containing the converted time values with an
#'   attribute \code{unit} which is a printable name of the unit of
#'   time.
#'
#' @author Olaf Mersmann
#'
#' @keywords internal
convert_to_unit <- function(x,
                            unit=c("ns", "us", "ms", "s", "t",
                                   "hz", "khz", "mhz", "eps", "f")) {
  unit <- match.arg(unit)

  switch (unit,
          t=unit <- sprintf ("%ss", find_prefix(x * 1e-9,
            minexp = -9, maxexp = 0, mu = FALSE)),
          f=unit <- sprintf ("%shz", find_prefix(1e9 / x,
            minexp =  0, maxexp = 6, mu = FALSE))
          )
  unit <- tolower(unit)
  switch (unit,
          ns  ={attr(x, "unit") <- "nanoseconds"           ; unclass(x      )},
          us  ={attr(x, "unit") <- "microseconds"          ; unclass(x / 1e3)},
          ms  ={attr(x, "unit") <- "milliseconds"          ; unclass(x / 1e6)}, 
          s   ={attr(x, "unit") <- "seconds"               ; unclass(x / 1e9)},
          eps ={attr(x, "unit") <- "evaluations per second"; unclass(1e9 / x)},
          hz  ={attr(x, "unit") <- "hertz"                 ; unclass(1e9 / x)},
          khz ={attr(x, "unit") <- "kilohertz"             ; unclass(1e6 / x)},
          mhz ={attr(x, "unit") <- "megahertz"             ; unclass(1e3 / x)},
          stop("Unknown unit '", unit, "'.")
          )
}

#' Find SI prefix for unit
#' 
#' @param x a numeric
#' @param f function that produces the number from \code{x} that is used to
#'   determine the prefix, e.g. \code{\link[base]{min}} or
#'   \code{\link[stats]{median}}.
#' @param minexp minimum (decimal) exponent to consider, 
#'   e.g. -3 to suppress prefixes smaller than milli (m).
#' @param maxexp maximum (decimal) exponent to consider, 
#'   e.g. 3 to suppress prefixes larger than kilo (k).
#' @param mu if \code{TRUE}, should a proper mu be used for micro, otherwise use
#'  u as ASCII-compatible replacement
#'
#' @return character with the SI prefix
#' @author Claudia Beleites 
#'
#' @keywords internal
find_prefix <- function (x, f=min, minexp=-Inf, maxexp=Inf, mu=TRUE) {
  prefixes <- c ("y", "z", "a", "f", "p", "n", "u", "m", "",
                 "k", "M", "G", "T", "P", "E", "Z", "Y")
  if (mu) prefixes [7] <- "\u03bc"

  if (is.numeric (minexp)) minexp <- floor (minexp / 3) 
  if (is.numeric (minexp)) maxexp <- floor (maxexp / 3) 

  e3 <- floor (log10 (f (x)) / 3)
  e3 <- max (e3, minexp, -8) # prefixes go from 10^-24 = 10^(3 * -8)
  e3 <- min (e3, maxexp,  8) # to 10^24 = 10^(3 * 8)

  prefixes [e3 + 9] # e3 of -8 => index 1
 }

#' Return first non null argument.
#'
#' This function is useful when processing complex arguments with multiple
#' possible defaults based on other arguments that may or may not have been
#' provided.
#'
#' @param ... List of values.
#' @return First non null element in \code{...}.
#'
#' @author Olaf Mersmann
#'
#' @keywords internal
coalesce <- function(...) {
  isnotnull <- function(x) !is.null(x)
  Find(isnotnull, list(...))
}
