#' Tinyplot method for microbenchmark objects
#'
#' @description Uses the `tinyplot` package to produce prettier base graphics
#'   for microbenchmark timings. Note that `tinyplot` needs to be installed and
#'   loaded separately. 
#'
#' @param x A microbenchmark object.
#' @param type String giving the type of plot representation. One of `"violin"`
#'   (the default), `"boxplot"`, or `"jitter"`.
#' @param unit Unit in which the results be plotted.
#' @param log Should times be plotted on a log scale? Default is \code{TRUE}.
#' @param order Names of output column(s) to order the results.
#' @param main,xlab,ylab Plot and axes titles.
#' @param flip Switch the X and Y axes? Default is \code{TRUE}.
#' @param trim Trim violin plots to data extent? Default is \code{TRUE}.
#' @param joint.bw Which (if any) joint smoothing bandwidth to use on violin
#'   plots? Default is \code{"none"} to match
#'   \code{\link[microbenchmark]{autoplot.microbenchmark}}.
#' @param ... Additional arguments passed to [`tinyplot`].
#' @return No return value. Called for side effect of producing a plot.
#'
#' @examples
#' if (requireNamespace("tinyplot", quietly = TRUE)) {
#'     library(tinyplot)
#'     
#'     tm <- microbenchmark(rchisq(100, 0),
#'                          rchisq(100, 1),
#'                          rchisq(100, 2),
#'                          rchisq(100, 3),
#'                          times=100L)
#'     
#'     # default plot
#'     tinyplot(tm)
#'     
#'     # same, but with aesthetic tweaks
#'     tinyplot(tm,
#'              fill  = "transparent",
#'              theme = "classic",
#'              main  = "Impressive benchmarks",
#'              sub   = "Brought to you by tinyplot")
#' 
#'     # we can use tinyplot scaffolding to add layers to our plot
#'     tinyplot_add(type = "jitter", cex = 0.5, alpha = 0.3)
#' }
#' @author Grant McDermott
#' @export
tinyplot.microbenchmark = function(
   x,
   type = c("violin", "boxplot", "jitter"),
   log = TRUE,
   unit = NULL,
   order = NULL,
   main = "microbenchmark timings",
   xlab = NA,
   ylab = NULL,
   flip = TRUE,
   trim = TRUE,
   joint.bw = c("none", "mean", "full"),
   ...
) {

  dots <- list(...)

  type <- match.arg(type)
  joint.bw <- match.arg(joint.bw)
  
  unit <- determine_unit(x, unit)
  x$ntime <- convert_to_unit(x, unit)
  if (!is.null(order)) {
    s <- summary(x)
    x_colnames <- colnames(s)
    order <- match.arg(order, x_colnames, several.ok=TRUE)
    new_order <- do.call("order", c(s[, order, drop=FALSE], decreasing=TRUE))
    x$expr <- factor(x$expr, levels = levels(x$expr)[new_order])
  }
  
  if (is.null(ylab)) ylab <- sprintf(
    "Time (%s) for neval = %d",
    attr(x$ntime, "unit"),
    nrow(x) / length(levels(x$expr))
  )

  if (isTRUE(log)) {
    log <- "y"
  } else {
    log <- NULL
  }

  if (is.null(dots$ylim)) {
    if (log == "y") {
      y_min <- if (min(x$time) == 0) 1 else min(x$ntime)
    } else {
      y_min <- 0
    }
    y_max <- max(x$ntime)
    dots$ylim <- c(y_min, y_max)
  }
  
  do.call(
    tinyplot::tinyplot,
    utils::modifyList(
      list(
        x = ntime ~ expr,
        data = x,
        type = type,
        main = main, 
        ylab = ylab,
        xlab = xlab,
        log = log,
        trim = trim,
        flip = flip,
        joint.bw = joint.bw
      ),
      dots
    )
  )
  
}
