#Simulation Test / Model Usage
#K=2, D=2
#yl~ linear, 3 covariate, including variable selection
#yb ~ binomial, 5 covariate, including variable selection
#for each S, simulate 500 times, output the k, coefficient
##now running (50,0.5,2)
simulate <- function(ite, Scoef, buff) {
  n = ite # iteration number for simulation.
  k.out = matrix("NA", n, 6 )
  colnames(k.out) = c("npk1", "npk2", "lsk1", "lsk2", "sdk1", "sdk2")
  converge.out = matrix("NA", n, 3 )
  colnames(converge.out) = c("npconverge","lsconverge", "sdconverge")
  for (i in 1:n){
    s = Scoef
    beta.l1=as.matrix(c(1,1,0,-1))*s
    beta.l2=as.matrix(c(1,0,-1,-2))*s
    beta.b1=as.matrix(c(1,0,2,-1,-0.5,-2))*s
    beta.b2=as.matrix(c(1,2,3,-1,0,-3))*s
    Xl1 = matrix(rnorm(200*3),200,3)
    Xl2 = matrix(rnorm(100*3,1,1),100,3)
    Xb1 = matrix(rnorm(200*5),200,5)
    Xb2 = matrix(rnorm(100*5,1,1),100,5)
    Yl1 = cbind(rep(1,200),Xl1)%*%beta.l1+rnorm(200,0,1)
    Yl2 = cbind(rep(1,100),Xl2)%*%beta.l2+rnorm(100,0,1)
    Yb1 = rbinom(200,1,1/(1+exp(-(cbind(rep(1,200),Xb1)%*%beta.b1))))
    Yb2 = rbinom(100,1,1/(1+exp(-(cbind(rep(1,100),Xb2)%*%beta.b2))))
    temp1<-cbind(Yl1,Yb1,Xl1,Xb1)
    temp2<-cbind(Yl2,Yb2,Xl2,Xb2)
    test.data<-rbind(temp1,temp2)
    Ncol=10
    colname=array(dim=Ncol)
    colname[1] = "yl"
    colname[2] = "yb"
    for (j in 1:3) {
      xname = paste( "xl" ,toString(j) ,sep="_")
      colname[2+j]= xname
    }
    for (j in 1:5) {
    xname = paste( "xb" ,toString(j) ,sep="_")
    colname[5+j]= xname
    }
    colnames(test.data)=colname

    # code for the final report
    # SCAD penalty
    Model_l <- FLXMRncvreg(yl ~ xl_1+xl_2+xl_3, penalty="SCAD")
    Model_b <- FLXMRncvreg(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4+xb_5, penalty="SCAD", family = "binomial")
    m_scad<- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
                control = list(verbose = 50, iter.max =100))
    
    # Adaptive LASSO penalty
    Model_l <- FLXMRglmnet(yl ~ xl_1+xl_2+xl_3, adaptive = TRUE)
    Model_b <- FLXMRglmnet(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4+xb_5, adaptive = TRUE, family = "binomial")
    m_lasso <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
                   control = list(verbose = 50, iter.max =100))
    # without penalty
    Model_l <- FLXMRglm(yl ~ xl_1+xl_2+xl_3)
    Model_b <- FLXMRglm(cbind(yb,1-yb)~xb_1+xb_2+xb_3+xb_4+xb_5, family = "binomial")
    m_nopenalty <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
                control = list(verbose = 50,iter.max =100))
 
     parameters(m_nopenalty)
    parameters(m_scad)
    parameters(m_lasso)
    k.out[i,1] = (as.data.frame(m_nopenalty@size))[1,]
    k.out[i,2] = (as.data.frame(m_nopenalty@size))[2,]
    k.out[i,3] = (as.data.frame(m_lasso@size))[1,]
    k.out[i,4] = (as.data.frame(m_lasso@size))[2,]
    k.out[i,5] = (as.data.frame(m_scad@size))[1,]
    k.out[i,6] = (as.data.frame(m_scad@size))[2,]
    converge.out[i,1] =m_nopenalty@converged
    converge.out[i,2] =m_lasso@converged
    converge.out[i,3] =m_scad@converged
  }
  k.out.name= paste("/Users/JinchunZhang/k.out.s", toString(s), "_", toString(buff),".txt",sep="")
  c.out.name= paste("/Users/JinchunZhang/converge.out.s", toString(s), "_", toString(buff),".txt",sep="")
  write.table(k.out, file = k.out.name, quote=FALSE, sep=",")
  write.table(converge.out, file =c.out.name, quote=FALSE, sep=",")
}
