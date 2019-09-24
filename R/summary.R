#' Summarize \code{microbenchmark} timings.
#'
#' @param object An object of class \code{microbenchmark}.
#'
#' @param unit What unit to print the timings in. If none is given,
#' either the \code{unit} attribute of \code{object} or the option
#' \code{microbenchmark.unit} is used and if neither is set
#' \dQuote{t} is used.
#'
#' @param ... Passed to \code{print.data.frame}
#'
#' @note The available units are nanoseconds (\code{"ns"}),
#' microseconds (\code{"us"}), milliseconds (\code{"ms"}), seconds
#' (\code{"s"}) and evaluations per seconds (\code{"eps"}) and
#' relative runtime compared to the best median time
#' (\code{"relative"}).
#'
#' @return A \code{data.frame} containing the aggregated results.
#'
#' @seealso \code{\link{print.microbenchmark}}
#' @method summary microbenchmark
summary.microbenchmark <- function(object, unit, ...) {
  ## Choose unit if not given based on unit attribute of object or
  ## global option. Default to 't' if none is set.
  if (missing(unit)) {
    unit <- if (!is.null(attr(object, "unit")))
      attr(object, "unit")
    else
      getOption("microbenchmark.unit", "t")
  }
  if (unit != "relative")
    object$time <- convert_to_unit(object$time, unit)

  res <- aggregate(time ~ expr, object,
                   function(z) {
                       tmp <- c(fivenum(z), mean(z), length(z))
                       tmp[c(1, 2, 6, 3, 4, 5, 7)]
                   })
  res <- cbind(res$expr, as.data.frame(res$time))
  colnames(res) <- c("expr", "min", "lq", "mean", "median", "uq", "max", "neval")

  if (unit == "relative") {
    min <- res[which.min(res$median), , drop = FALSE]
    min$neval <- 1 # Ugly hack: Do not rescale neval
    res[-1] <- res[-1] / as.list(min[-1])
    attr(res, "unit") <- "relative"
  } else {
    attr(res, "unit") <- attr(object$time, "unit")
  }

  if (requireNamespace("multcomp", quietly = TRUE)
      && nrow(res) > 1 && all(res["neval"] > 1)) {
    ## Try to calculate a statistically meaningful comparison. If it fails for
    ## any reason (f.e. the data might be constant), ignore the error.
    tryCatch({
      ops <- options(warn=-1)
      mdl <- lm(time ~ expr, object)
      comp <- multcomp::glht(mdl, multcomp::mcp(expr = "Tukey"))
      res$cld <- multcomp::cld(comp)$mcletters$monospacedLetters
    }, error=function(e) FALSE, finally=options(ops))
  }
  res
}
