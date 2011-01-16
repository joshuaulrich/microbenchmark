library("microbenchmark")
library("methods")

## Class definitions:
setClass("s4_class", representation(id="character", num="numeric"))
setClass("s4_subclass", contains="s4_class")
setClass("s4_subsubclass", contains="s4_subclass")

## Constructors:
s4_class <- function(id, num) new("s4_class", id=id, num=num)
s4_subclass <- function(id, num) new("s4_subclass", id=id, num=num)
s4_subsubclass <- function(id, num) new("s4_subsubclass", id=id, num=num)

## [ - Generic field getter using subset operator.
setMethod(f="[", signature = signature("s4_class"),
          def=function(x, i, j, ..., drop) {
            if (i %in% slotNames(x))
              return (slot(x, i))
            return (NULL)
          })

setMethod(f="[", signature = signature("s4_subclass"),
          def=function(x, i, j, ..., drop) {
            if (i == "bam")
              return ("ham")
            if (i == "ham")
              return ("bam")
            callNextMethod()
          })

setMethod(f="[", signature = signature("s4_subsubclass"),
          def=function(x, i, j, ..., drop) {
            if (i == "foo")
              return ("bar")
            if (i == "bar")
              return ("foo")
            callNextMethod()
          })

## get_id - Simple generic dispatch.
setGeneric(name="get_id",
           def=function(x) standardGeneric("get_id"))

setMethod("get_id", "s4_class", function(x) x@id)

## getId - implements a mix of get_id (explicit getter) but with an
##   explicit method for each subclass that calls callNextMethod().
setGeneric(name="getId",
           def=function(x) standardGeneric("getId"))

setMethod("getId", "s4_class", function(x) x@id)
setMethod("getId", "s4_subclass", function(x) callNextMethod())
setMethod("getId", "s4_subsubclass", function(x) callNextMethod())

## Micro benchmark of call speed:
n <- 1000L
c4 <- s4_class(id="foo", num=2)
sc4 <- s4_subclass(id="foo", num=2)
ssc4 <- s4_subsubclass(id="foo", num=2)

speed <- microbenchmark(c4@id, sc4@id, ssc4@id,
                        get_id(c4), get_id(sc4), get_id(ssc4),
                        c4["id"], sc4["id"], ssc4["id"],
                        getId(c4), getId(sc4), getId(ssc4),
                        ## as(c4, "s4_class")["id"], as(sc4, "s4_class")["id"], as(ssc4, "s4_class")["id"],
                        times=n)
print(speed, "eps")

## Sennheiser MM 550
