#Simulate a testing data of mixture distribution for Algorithm Testing
#Author: Jinchun Zhang
#Created: 2016-12-05
#Last Updated:2016-12-05
#Contact information: jz2516@nyu.edu

simX <- function(n,p,seed){
  set.seed(seed)
  bound1 <- runif(p,-100, 100)
  bound2 <- runif(p, -100, 100)
  x <- matrix(nrow=n, ncol=p)
  for (i in 1:p){
    x[,i]= runif(n,min(bound1,bound2),max(bound1,bound2))    
  }
   x
}

simY <- function(){

}
