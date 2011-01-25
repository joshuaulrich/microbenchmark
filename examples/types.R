require("microbenchmark")

zeromatrix <- function(nrow, ncol) {
  x <- numeric(nrow * ncol)
  dim(x) <- c(nrow, ncol)
  x
}


n <- 1000
res1 <- microbenchmark(matrix(numeric(n*n), nrow=n, ncol=n),
                      matrix(0, nrow=n, ncol=n),
                      zeromatrix(n, n),
                      times=100L)
## print(res)
to_matrix <- function(x, nrow, ncol) {
  dim(x) <- c(nrow, ncol)
  x
}

x <- runif(n*n)
res2 <- microbenchmark(matrix(x, nrow=n, ncol=n),
                       to_matrix(x, n, n),
                       times=100L)
print(res2)
                       
