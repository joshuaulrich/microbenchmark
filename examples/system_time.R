library("microbenchmark")

n <- 100000
x <- runif(n)
y <- runif(n)

k <- 10000L
s <- 1:k

## Measure overhead:
t0 <- system.time(for (i in 1:k) NULL)
t1 <- system.time(for (i in 1:k) crossprod(x, y))
tt <- (t1 - t0) / k

t2 <- microbenchmark(crossprod(x, y), times=100L)
