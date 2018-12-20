library(microbenchmark)
library(methods)
options(error = traceback)

test_unit_argument <- function()
{
  out <- try(microbenchmark(NULL, unit=a), silent = TRUE)
  stopifnot(inherits(out, "try-error"))
}
test_unit_argument()

test_unit_f <- function()
{
  out <- try(print(microbenchmark(NULL, unit="f")), silent = TRUE)
  stopifnot(!inherits(out, "try-error"))
}
test_unit_f()

test_unit_int <- function()
{
  out <- try(print(microbenchmark(NULL, unit=4)), silent = TRUE)
  stopifnot(!inherits(out, "try-error"))
}
test_unit_int()

test_simple_timing <- function()
{
  set.seed(21)
  out <- microbenchmark(rnorm(1e4))
  stopifnot(all(out$time > 0))
}
test_simple_timing()

test_get_nanotime <- function()
{
  nt <- get_nanotime()
  stopifnot(nt > 0)
}
test_get_nanotime()

test_microtiming_precision <- function()
{
  mtp <- tryCatch(microtiming_precision(),
                  warning = function(w) w,
                  error = function(e) e)

  if (is(mtp, "warning") || is(mtp, "error")) {
    stop(mtp$message)
  }
}
test_microtiming_precision()

test_setup_expression <- function()
{
  set.seed(21)
  x <- rnorm(10)
  microbenchmark(y <- rnorm(10), x, setup = set.seed(21))
  stopifnot(identical(x, y))
}
test_setup_expression()

test_setup_expression_check <- function()
{
  my_check <- function(values) {
    v1 <- values[[1]]
    all(sapply(values[-1], function(x) identical(v1, x)))
  }
  set.seed(21)
  x <- rnorm(10)
  microbenchmark(rnorm(10), x, check = my_check, setup = set.seed(21))
}
test_setup_expression_check()

test_setup_expression_eval_env_check <- function()
{
  my_check <- function(values) {
    v1 <- values[[1]]
    all(sapply(values[-1], function(x) identical(v1, x)))
  }
  set.seed(21)
  x <- rnorm(10)
  microbenchmark(rnorm(n), x, check = my_check,
                 setup = {set.seed(21); n <- 10})
}
test_setup_expression_eval_env_check()

test_setup_expression_eval_env <- function()
{
  x <- rnorm(10)
  microbenchmark(y <- rnorm(n), x, setup = {n <- 10})
  stopifnot(length(y) == 10L)
}
test_setup_expression_eval_env()

test_expression_eval_parent_frame <- function()
{
  fx <- function() { 1:10 }
  fy <- function() { 1:10 }
  microbenchmark(x <- fx(), y <- fy())
  stopifnot(identical(x, y))
}
test_expression_eval_parent_frame()

test_setup_expression_check_equal <- function()
{
  set.seed(21)
  x <- rnorm(1e5)
  microbenchmark(rnorm(1e5), x, check = 'equal', setup = set.seed(21))
}
test_setup_expression_check_equal()

test_setup_expression_check_equal_failure <- function()
{
  set.seed(21)
  x <- rnorm(1e5)
  attr(x = x, 'abc') <- 123 # add attribute
  out <- try(microbenchmark(rnorm(1e5), x, check = 'equal', setup = set.seed(21)), silent = T)
  stopifnot(inherits(out, "try-error"))
}
test_setup_expression_check_equal_failure()

test_setup_expression_check_equivalent <- function()
{
  set.seed(21)
  x <- rnorm(1e5)
  attr(x = x, 'abc') <- 123 # add attribute
  microbenchmark(rnorm(1e5), x, check = 'equivalent', setup = set.seed(21))
}
test_setup_expression_check_equivalent()

test_setup_expression_check_identical <- function()
{
  set.seed(21)
  x <- rnorm(1e5)
  microbenchmark(rnorm(1e5), x, check = 'identical', setup = set.seed(21))
}
test_setup_expression_check_identical()

test_setup_expression_check_identical_failure <- function()
{
  set.seed(21)
  x <- rnorm(1e5)
  attr(x = x, 'abc') <- 123 # add attribute
  out <- try(microbenchmark(rnorm(1e5), x, check = 'equal', setup = set.seed(21)), silent = T)
  stopifnot(inherits(out, "try-error"))
}
test_setup_expression_check_identical_failure()
