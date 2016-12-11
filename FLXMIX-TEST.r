#Simulation Test / Model Usage
#K=2, D=2
#yl~ linear, 8 covariate, including variable selection
#yb ~ binomial, 12 covariate, including variable selection

n=1000
beta.l1=as.matrix(sample(-10:10, 5))
#beta.l1[2]=0
#beta.l1[3]=0.9

beta.l2=as.matrix(sample(-10:10, 5))
#beta.l2[2]=-0.1
#beta.l2[3]=0

beta.b1=as.matrix(sample(-20:20, 5))
beta.b1[3]=3
#beta.b1[5]=-1


beta.b2=as.matrix(sample(-20:20, 5))
#beta.b2[3]= -1.7
#beta.b2[5]= 0

Xl1 = matrix(rnorm(200*4),200,4)
Xl2 = matrix(rnorm(200*4),200,4)
Xb1 = matrix(rnorm(200*4),200,4)
Xb2 = matrix(rnorm(200*4),200,4)

Yl1 = cbind(rep(1,200),Xl1)%*%beta.l1+rnorm(200,0,1)
Yl2 = cbind(rep(1,200),Xl2)%*%beta.l2+rnorm(200,0,1)

Yb1 = rbinom(200,1,1/(1+exp(-(cbind(rep(1,200),Xb1)%*%beta.b1))))
Yb2 = rbinom(200,1,1/(1+exp(-(cbind(rep(1,200),Xb2)%*%beta.b2))))

temp1<-cbind(Yl1,Yb1,Xl1,Xb1)
temp2<-cbind(Yl2,Yb2,Xl2,Xb2)

test.data<-rbind(temp1,temp2)

Ncol=10
colname=array(dim=Ncol)
colname[1] = "yl"
colname[2] = "yb"
for (i in 1:4) {
  xname = paste( "xl" ,toString(i) ,sep="_")
  colname[2+i]= xname
}
for (i in 1:4) {
  xname = paste( "xb" ,toString(i) ,sep="_")
  colname[6+i]= xname
}

colnames(test.data)=colname

Model_l <- FLXMRncvreg(yl ~ xl_1+xl_2+xl_3+xl_4+xl_5+xl_6+xl_7, adaptive = TRUE)
Model_b <- FLXMRncvreg(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4+xb_5+xb_6+xb_7+xb_8+xb_9+xb_10+xb_11, adaptive = TRUE, family = "binomial")
m1 <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
	control = list(verbose = 10))

Model_l <- FLXMRglm(yl ~ xl_1+xl_2+xl_3+xl_4+xl_5+xl_6+xl_7)
Model_b <- FLXMRglm(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4+xb_5+xb_6+xb_7+xb_8+xb_9+xb_10+xb_11, family = "binomial")
m1 <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
              control = list(verbose = 10))

Model_l <- FLXMRncvreg(yl ~ xl_1+xl_2+xl_3+xl_4, penalty="SCAD")
Model_b <- FLXMRncvreg(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4, penalty="SCAD", family = "binomial")
m1 <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
              control = list(verbose = 10))

Model_l <- FLXMRglm(yl ~ xl_1+xl_2+xl_3+xl_4)
Model_b <- FLXMRglm(cbind(yb, 1-yb)~xb_1+xb_2+xb_3+xb_4, family = "binomial")
m1 <- flexmix(.~., data = as.data.frame(test.data), k = 2, model = list(Model_l, Model_b),
              control = list(verbose = 10))

