library("microbenchmark")
library("multcomp")

ssqdif <- function(X, Y=X) {                                                         
  #From 'outer' without modification
  Y <- rep(Y, rep.int(length(X), length(Y)))                                         
  X <- rep(X, times = ceiling(length(Y)/length(X)))                                  
  #For this case:
  sum((X-Y)^2) #SLIGHTLY quicker than d<-X-Y; sum(d*d)                               
}
                                                                                     
outerdif <- function(X, Y = X) {                                                     
  gg <- outer(X, Y, FUN="-")                                                         
  sum(gg*gg)                                                                         
}

X <- runif(1000)
m <- microbenchmark(                                                                 
  ssqdif(X),                                                                         
  outerdif(X)                                                                        
)                                                                                    
                                                                                     
library(multcomp)                                                                    
mod <- lm(time ~ expr, m)                                                            
comp <- glht(mod, mcp(expr = "Tukey"))                                               
cld(comp)
sm <- summary(m)                                                                     
sm$ratio <- sm$median / min(sm$median)                                               
sm$cld <- cld(comp)$mcletters$monospacedLetters             
