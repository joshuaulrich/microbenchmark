library("microbenchmark")

res <- microbenchmark(NULL, times=100000L)
times <- res$time
long <- which(times > 2.5 * median(times))

print(median(times[long]) - median(times[-long]))
