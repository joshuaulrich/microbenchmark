library("microbenchmark")

f0 <- function() NULL

f1 <- function(a) NULL

f2 <- function(a, b) NULL

f3 <- function(a, b, c) NULL

f10 <- function(a, b, c, d, e, f, g, h, i, j) NULL

n <- 10000L

res <- microbenchmark(f0(), f1(1), f2(1, 1), f3(1, 1, 1),
                      f10(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                      times=n)
print(res, unit="relative")
