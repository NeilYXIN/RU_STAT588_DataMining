library(glmnet)
library(elasticnet)
library(randomForest)
library(DNAMR)
library(erf)
library(ada)
library(penalizedSVM)

xx = cbind(x,array(rnorm(50000),dim=c(50,1000)))
table(y <- xx[,1] > 0)

#Random forest is affected by large number of noise vars

randomForest(xx[,1:2],factor(y),ntree=10000)
randomForest(xx[,1:3],factor(y),ntree=10000)

predict(glmnet(xx[,1:1000],factor(y),family="binomial"))

frate2 = function(x,y) {
i = sort.list(x)
n = length(x)
y = y[i]; x= x[i] 
t2 = cumsum(y)/sum(y)
t1 = cumsum(1-y)/sum(1-y)
m1=which.min(1-t1+t2)
c( if (m1<n)mean(x[m1+0:1]) else x[m1], (1-t1+t2)[m1])
}

# Ten fold cross validation with glmnet

yy = y*0
for(i in 1:10) {
jj=5*(i-1)+1:5
hh=predict(glmnet(xx[-jj,],alpha=0.5,factor(y)[-jj],family="binomial"),newx=xx[,]) 
uu=t(apply(hh[-jj,],2,frate2,y[-jj]))
kk=which.min(uu[,2])
yy[jj]=hh[jj,kk]>=uu[kk,1]
}
table(y,yy)


# Leave on out cross validation with glmnet

xx =t(rma0)
y = rma0res-1
yy = y*0
n = length(y)
for(i in 1:n) { 
jj=i
hh=predict(glmnet(xx[-jj,],alpha=0.75,factor(y)[-jj],family="binomial"),newx=xx[,]) 
uu=t(apply(hh[-jj,],2,frate2,y[-jj]))
kk=which.min(uu[,2])
yy[jj]=hh[jj,kk]>=uu[kk,1]
}
table(y,yy)

# Predicting on a testing set
# For this data I am provided with two testing sets
#  that are called ram10 and rma18

lam=0.75
xx = t(rma0)
y = rma0res -1 
yy = y*0

uu=glmnet(xx,factor(y), family="binomial",alpha=lam)
hh=predict(uu,newx=xx) 
hh1=predict(uu,newx=t(rma18)) 
hh2=predict(uu,newx=t(rma10)) 
uu1=t(apply(hh,2,frate2,y))
kk=which.min(uu1[,2])

yy1=hh1[,kk]>=uu1[kk,1]
yy2=hh2[,kk]>=uu1[kk,1]
table(rma18res,yy1)
table(rma10res,yy2)

### Example with  SVM 
uu <- svm(t(rma0),2*(rma0res-1)-1)
predict(uu, newdata=t(rma10)) 
plot(predict(uu, newdata=t(rma10)) , rma10res)
plot(predict(uu, newdata=t(rma18)) , rma18res)

### Example with Penalized SVM 
uu <- svm.fs(t(rma0),2*(rma0res-1)-1)
table(predict(uu, newdata=rma10)$pred.class,rma10res) 
table(predict(uu, newdata=rma18)$pred.class,rma18res) 





