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
##'   or idle states it might be in. The default value is 2.}
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
##' @param unit Default unit used in \code{summary} and \code{print}.
##' 
##' @return Object of class \sQuote{microbenchmark}, a matrix with one
##'   column per expression. Each row contains the time it took to
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
##' @author Olaf Mersmann \email{olafm@@p-value.net}
microbenchmark <- function(..., list=NULL,
                           times=100L,
                           unit,
                           control=list()) {
  stopifnot(times == as.integer(times))
  if (!missing(unit))
    stopifnot(is.character("unit"), length(unit) == 1L)
  
  control[["warmup"]] <- coalesce(control[["warmup"]], 2^18L)
  control[["order"]] <- coalesce(control[["order"]], "random")

  stopifnot(as.integer(control$warmup) == control$warmup)

  exprs <- c(as.list(match.call(expand.dots = FALSE)$`...`), list)
  nm <- names(exprs)
  exprnm <- sapply(exprs, function(e) paste(deparse(e), collapse=" "))
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

  res <- data.frame(expr = factor(nm[o], levels = nm), time=res)
  class(res) <- c("microbenchmark", class(res))
  if (!missing(unit))
    attr(res, "unit") <- unit
  res
}
