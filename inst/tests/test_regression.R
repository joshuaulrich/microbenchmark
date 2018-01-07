library(microbenchmark)

test_units_argument <- function()
{
  out <- try(microbenchmark(NULL, units=a), silent = TRUE)
  stopifnot(inherits(out, "try-error"))
}
test_units_argument()
