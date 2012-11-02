library("microbenchmark")

b <- microbenchmark(b=1, c=2, a=3, times=1000L)
print(b)

b1 <- microbenchmark(b=1, a=2, times=1000L)
b2 <- microbenchmark(d=3, c=4, times=250L)
print(rbind(b1, b2))
