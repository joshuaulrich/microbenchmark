#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings.
#'
#' @param object A microbenchmark object.
#' @param \dots Ignored.
#' @param order Names of output column(s) to order the results.
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param unit The unit to use for graph labels.
#' @param y_max The upper limit of the y axis, in the unit automatically
#'   chosen for the time axis (defaults to the maximum value).
#' @return A ggplot2 object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     tm <- microbenchmark(rchisq(100, 0),
#'                          rchisq(100, 1),
#'                          rchisq(100, 2),
#'                          rchisq(100, 3),
#'                          rchisq(100, 5), times=1000L)
#'     ggplot2::autoplot(tm)
#' }
#' @author Ari Friedman, Olaf Mersmann
autoplot.microbenchmark <- function(object, ...,
                                    order=NULL,
                                    log=TRUE,
                                    unit=NULL,
                                    y_max=NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Missing package 'ggplot2'.")
  y_min <- 0
  object_unit <- attr(object, "unit")
  if (!is.null(object_unit) && is.null(unit)) {
      unit <- object_unit
  }
  unit <- normalize_unit(unit)
  object$ntime <- convert_to_unit(object$time, unit)
  if (is.null(y_max)) {
    y_max <- max(object$ntime)
  }
  if (!is.null(order)) {
    s <- summary(object)
    object_colnames <- colnames(s)
    order <- match.arg(order, object_colnames, several.ok=TRUE)
    new_order <- do.call("order", c(s[, order, drop=FALSE], decreasing=TRUE))
    object$expr <- factor(object$expr, levels = levels(object$expr)[new_order])
  }
  plt <- ggplot2::ggplot(object, ggplot2::aes_string(x="expr", y="ntime"))
  plt <- plt + ggplot2::stat_ydensity()
  plt <- plt + ggplot2::scale_x_discrete(name="")
  y_label <- sprintf("Time [%s]", attr(object$ntime, "unit"))
  if (log) {
    y_min <- if (min(object$time) == 0) 1 else min(object$ntime)
    plt <- plt + ggplot2::scale_y_log10(name=y_label)
  } else {
    plt <- plt + ggplot2::scale_y_continuous(name=y_label)
  }
  plt <- plt + ggplot2::coord_flip(ylim=c(y_min , y_max))
  plt
}
