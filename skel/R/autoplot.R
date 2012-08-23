##' Autoplot method for microbenchmark objects: Prettier graphs for
##' microbenchmark using ggplot2
##'
##' Uses ggplot2 to produce a more legible graph of microbenchmark timings
##'
##' @param object A microbenchmark object
##' @param \dots Ignored
##' @param log If \code{TRUE} the time axis will be on log scale.
##' @param y_max The upper limit of the y axis (defaults to 5 percent more than
##'   the maximum value)
##' @return A ggplot2 plot
##' @export autoplot.microbenchmark
##' @method autoplot microbenchmark
##' @examples
##' library("ggplot2")
##' 
##' tm <- microbenchmark(rchisq(100, 0),
##'                      rchisq(100, 1),
##'                      rchisq(100, 2),
##'                      rchisq(100, 3),
##'                      rchisq(100, 5), times=1000L)
##' autoplot(tm)
##' @author Ari Friedman, Olaf Mersmann
autoplot.microbenchmark <- function(object, ...,
                                    log=TRUE,
                                    y_max=1.05 * max(object$time)) {
  y_min <- 0
  object$ntime <- convert_to_unit(object$time, "t")
  plt <- ggplot(object, aes_string(x="expr", y="ntime"))
  plt <- plt + coord_cartesian(ylim=c(y_min , y_max))
  plt <- plt + stat_ydensity()
  plt <- plt + scale_x_discrete(name="")
  plt <- if (log) {
    plt + scale_y_log10(name=sprintf("Time [%s]", attr(object$ntime, "unit")))
  } else {
    plt + scale_y_continuous(name=sprintf("Time [%s]", attr(object$ntime, "unit")))
  }
  plt <- plt + coord_flip()  
  plt
}
