library("microbenchmark")

m <- microbenchmark(a=runif(100), b=runif(100), times=100L)

relative <- function(x, order = TRUE) {
  sum <- summary(x)
  min <- sum[which.min(sum$median), , drop = FALSE]
  min$neval <- 1 # Ugly hack: Do not rescale neval
  sum[-1] <- sum[-1] / as.list(min[-1])

  if (order) {
    sum <- sum[order(sum$median), , drop = FALSE]
  }

  sum
}
print(m)
print(m, order="min")
print(m, unit="relative")
