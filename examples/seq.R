library("reshape")
library("ggplot2")
library("microbenchmark")

n <- 1000L
x <- 1:n

res <- microbenchmark(seq_along(x),
                      seq_len(n),
                      1:n,
                      1:length(x),
                      seq(1L, n),
                      seq.int(1L, n),
                      times=1000L)
print(res, unit="ns")
