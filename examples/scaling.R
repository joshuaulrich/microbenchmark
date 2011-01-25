library("microbenchmark")

exprs <- list()
for (n in 10^(3:7)) {
  name <- sprintf("n%i", n)
  assign(name, as.numeric(1:n))
  exprs[[name]] <- bquote(f(.(as.name(name))))
}

f <- sum
res <- microbenchmark(list=exprs, times=100L)
print(relative_slowdown(res))
