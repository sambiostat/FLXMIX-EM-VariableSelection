simX <- function(n,p){
  bound1 <- runif(p,-100, 100)
  bound2 <- runif(p, -100, 100)
  x <- matrix(nrow=n, ncol=p)
  for (i in 1:p){
    x[,i] <- runif(n,min(bound1,bound2),max(bound1,bound2))    
  }
  x
}

simBeta<-function(p){
  beta<-matrix(nrow=1, ncol=p)
  for (i in 1:p){
    beta[1,i] <- runif(1,-20,20)
  }
  beta
}

simY <- function(x,beta,family=c("gaussian","binomial")){
  family <- match.arg(family)
  unlinkedY <- x %*% t(beta)
  if (family == "guassian") {
    y <- unlinkedY + rnorm(nrow(x),0,runif(1,1,2))
  }
  if (family =="binomial") {
    prob <- 1/(1+exp(-unlinkedY))
    y <- rbinom(length(prob),1,prob)
  }
  y
}

#nparam and family can only take a list with 2 elements and one is corresponding to another one
#k is in default as 2
simData <- function(n, nparam=list(),family=list(),mprop,covariate=c(TRUE,FALSE)){
  prop <- c(mprop,1-mprop)
  if (covariate) {
    Ncol=nparam[1]+2
    colname = array(dim=Ncol)
    colname[1] = "y1"
    colname[2] = "y2"
    for (i in 1:Ncol-2) {
      xname = paste( "x", toString(i) ,sep="_")
      append(colname,xname)
    }
  }else {
    Ncol=nparam[1]+nparam[2]+2
    colname=array(dim=Ncol)
    colname[1] = "y1"
    colname[2] = "y2"
    for (i in 1:nparam[1]) {
      xname = paste( "x1" ,toString(i) ,sep="_")
      colname[2+i]= xname
    }
    for (i in 1:nparam[2]) {
      xname = paste( "x2" ,toString(i) ,sep="_")
      colname[2+nparam[1]+i]= xname
    }
  }  
  final=matrix()
  for (k in 1:2){
    p1 = nparam[1]
    x1 = simX(n,p1)
    betaa1 = simBeta(p1)
    y1 = simY(x1,beta1,family=family[1])
    p2 = nparam[2]
    beta2 = simBeta(p2)
    if (covariate) {
      x2 = x1
      y2 = simY(x2,beta2,family=family[2])
      temp=cbind(y1,y2,x1)
    } else {
      x2 = simX(n,p2)
      y2 = simY(x2,beta2,family=family[2])
      temp = cbind(y1,y2,x1,x2)
    }
    if (k==1){final = temp[sample(1:n,n*prop[1]),]
      }else{final <- rbind(final,temp)}
  }
  colnames(final)=colname
  final
  }








