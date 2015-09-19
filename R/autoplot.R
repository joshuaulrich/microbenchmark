#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings
#'
#' @param object A microbenchmark object
#' @param \dots Ignored
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param y_max The upper limit of the y axis (defaults to 5 percent more than
#'   the maximum value)
#' @return A ggplot2 plot
#'
#' @importFrom ggplot2 autoplot
#' @export
#' @method autoplot microbenchmark
#' @examples
#' library("ggplot2")
#'
#' tm <- microbenchmark(rchisq(100, 0),
#'                      rchisq(100, 1),
#'                      rchisq(100, 2),
#'                      rchisq(100, 3),
#'                      rchisq(100, 5), times=1000L)
#' autoplot(tm)
#' @author Ari Friedman, Olaf Mersmann
autoplot.microbenchmark <- function(object, ...,
                                    log=TRUE,
                                    y_max=1.05 * max(object$time)) {
  if (!requireNamespace("ggplot2"))
    stop("Missing package 'ggplot2'.")
  y_min <- 0
  object$ntime <- convert_to_unit(object$time, "t")
  plt <- ggplot2::ggplot(object, ggplot2::aes_string(x="expr", y="ntime"))
  plt <- plt + ggplot2::coord_cartesian(ylim=c(y_min , y_max))
  plt <- plt + ggplot2::stat_ydensity()
  plt <- plt + ggplot2::scale_x_discrete(name="")
  y_label <- sprintf("Time [%s]", attr(object$ntime, "unit"))
  plt <- if (log) {
    plt + ggplot2::scale_y_log10(name=y_label)
  } else {
    plt + ggplot2::scale_y_continuous(name=y_label)
  }
  plt <- plt + ggplot2::coord_flip()
  plt
}
