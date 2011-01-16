library("microbenchmark")
library("methods")

messagef <- function(msg, ...)
  message(sprintf(msg, ...))

## Plain old R style function call:
plain_object <- function()
  list(a=1, b="bam")

plain_fun <- function(x, ...)
  NULL

## S3 style method call:
s3_object <- function() 
  structure(list(a=1, b="bam"),
            class="s3_object")

s3_fun <- function(x, ...)
  UseMethod("s3_fun")

s3_fun.s3_object <- function(x, ...)
  NULL

## S4 style method call:
setClass("s4_object", representation(a="numeric", b="character"))


s4_object <- function()
  new("s4_object", a=1, b="bam")

setGeneric("s4_fun", function(x, ...) standardGeneric("s4_fun"))

setMethod("s4_fun", "s4_object", function(x, ...) NULL)
for (i in 1:200) {
  n <- sprintf("a4_object_%i", i)
  setClass(n, representation(a="numeric", b="character"))
  setMethod("s4_fun", n, function(x, ...) NULL)
}

## Micro benchmark of call speed:
n <- 1000x00L
op <- plain_object()
o3 <- s3_object()
o4 <- s4_object()

speed <- microbenchmark(plain_fun(op), s3_fun(o3), s4_fun(o4), times=n)
print(speed)

