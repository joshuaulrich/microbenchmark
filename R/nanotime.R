#' Return the current value of the platform timer.
#'
#' The current value of the most accurate timer of the platform is
#' returned. This can be used as a time stamp for logging or similar
#' purposes. Please note that there is no common reference, that is,
#' the timer value cannot be converted to a date and time value.
#' 
#' @useDynLib microbenchmark do_get_nanotime
#' @author Olaf Mersmann
#' @export
get_nanotime <- function() {
  .Call(do_get_nanotime, PACKAGE="microbenchmark")
}
