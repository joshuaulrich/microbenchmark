library("reshape")
library("ggplot2")
library("microbenchmark")

n <- 1000L
x <- 1:n

res <- microbenchmark(1:n,
                      1:length(x),
                      seq(1L, n),
                      seq.int(1L, n),
                      seq_len(n),
                      seq_along(x),
                      times=500L)
print(res, unit="slowdown")

r <- melt(unclass(res))
plt <- qplot(X1, value, data=r, geom="line")
plt <- plt + facet_grid(X2 ~ .)
plt <- plt + scale_y_log10()
print(plt)
