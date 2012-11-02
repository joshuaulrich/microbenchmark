library("microbenchmark")

b <- rbind(microbenchmark(1, -1, runif(10), times=200L),
           microbenchmark(2, -2, runif(20), times=100L))
print(b)
print(b, unit="eps")
print(b, unit="t", order="neval")
