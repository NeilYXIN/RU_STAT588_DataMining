##### Code from Class Oct 4
##### Linear classifiers and Iris data
#### 
#### 
####   Example LDA
library(MASS)
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
set.seed(23561)
train <- sample(1:150, 50)
table(Iris$Sp[train])
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
PTest = predict(z, Iris[-train, ])$class
table(PTest, Iris$Sp[-train])
z1 <- update(z, . ~ . - Petal.W.)
PTest1 = predict(z1, Iris[-train, ])$class
table(PTest1, Iris$Sp[-train])

###  Now QDA
z2 <- qda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
PTest2 = predict(z2, Iris[-train, ])$class
table(PTest2, Iris$Sp[-train])
## Nonlinear
z3 <- lda(Sp ~ . +I(Sepal.L.^2)+I(Sepal.W.^2)+I(Petal.L.^2)+I(Petal.W.^2), Iris, prior = c(1,1,1)/3, subset = train)
PTest3 = predict(z3, Iris[-train, ])$class
table(PTest2, Iris$Sp[-train])

###  LOGISTIC REGRESSION
###  
library(glmnet)
Irr = Iris[train,]
lam = unlist(cv.glmnet(as.matrix(Irr[,-5]), factor(Irr[,5]),family="multinomial", alpha=1)[10])
mglm =glmnet(as.matrix(Irr[,-5]), factor(Irr[,5]), family="multinomial", lambda=lam,alpha=1)
pairs(predict(mglm,newx =as.matrix(Iris[-train,-5]) ),col=factor(Irr[-train,5]))
table( apply(pg,1,which.max),Iris$Sp[-train])
