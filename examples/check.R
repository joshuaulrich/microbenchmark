library("microbenchmark")

my_check <- function(values) {
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

f <- function(a, b)
  2 + 2

a <- 2
microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)

a <- 3
microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)
