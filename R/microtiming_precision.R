#' Estimate precision of timing routines.
#'
#' This function is currently experimental. Its main use is to judge
#' the quality of the underlying timer implementation of the
#' operating system. The function measures the overhead of timing a C
#' function call \code{rounds} times and returns all non-zero timings
#' observed. This can be used to judge the granularity and resolution
#' of the timing subsystem.
#' 
#' @param rounds Number of measurements used to estimate the precision.
#' @param warmup Number of iterations used to warm up the CPU.
#' @return A vector of observed non-zero timings.
#'
#' @author Olaf Mersmann
#' @export
microtiming_precision <- function(rounds=100L, warmup=2^18) {
  .Call(do_microtiming_precision, parent.frame(),
        as.integer(rounds),
        as.integer(warmup))
}
