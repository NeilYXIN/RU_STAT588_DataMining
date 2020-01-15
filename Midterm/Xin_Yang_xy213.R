#Question 1
data=load(file = '~/Desktop/Midterm/geno.R', envir = parent.frame(), verbose = FALSE)
set.seed(182006660)
x0=t(x0)
index <-  sort(sample(nrow(x0), nrow(x0)*.8))
train_x0 <- x0[index,]
test_x0 <-  x0[-index,]
train_y <- y[index]
test_y <-  y[-index]

# Question 2
design_matrix=cbind(x0==0, x0==1, x0==2)*1
train_design_x0 <- design_matrix[index,]
test_design_x0 <- design_matrix[-index,]

# Question 3
# SVM
library(e1071)
#svmfit <- svm(x=train_design_x0, y=as.factor(train_y), kernel='linear')
svm_model <- svm(x=train_design_x0, y=as.factor(train_y))
print(svm_model)
summary(svm_model)

svm_predict <- predict(svm_model, test_design_x0)
table(svm_predict,as.factor(test_y))

# Random Forest
library(randomForest)
rf_model <- randomForest(x=train_design_x0, y=as.factor(train_y))
rf_predict <- predict(rf_model, test_design_x0)
table(rf_predict,as.factor(test_y))

# Naive Bayes
nb_model <- naiveBayes(x=train_design_x0, y=as.factor(train_y))
nb_predict <- predict(nb_model, test_design_x0)
table(nb_predict, as.factor(test_y))

# Question 4
# GLMNET
library(glmnet)
glm_model <- glmnet(x=train_design_x0, y=as.factor(train_y), family='multinomial')
glm_predict <- predict(glm_model, test_design_x0, type="response")[,,1]
glm_labels <- colnames(glm_predict)[apply(glm_predict,1,which.max)]
glm_factors<-factor(c(glm_labels), levels = colnames(glm_predict))
table(glm_factors, as.factor(test_y))

# PenalizedSVM using elastic net

#install.packages("penalizedSVM", version='1.0')

#library(penalizedSVM)
#y_0_train = sign(train_y - 0.5)
#y_0_test = sign(test_y - 0.5)
#svmfs_model = svmfs(x = train_design_x0, y=y_0_train, fs.method = c("DrHSVM"),maxIter = 10,verbose = FALSE)
#svmfs_predict = predict.penSVM(svmfs_model, test_design_x0, as.factor(y_0_test))

#print(pl_predict$tab)


library(sparseSVM)
# 0 as class 1, 1,2 as class 0
y_0_train <- train_y
for(i in 1:length(train_y)){
  y_0_train[i]<-0
  if(train_y[i] == 0){
    y_0_train[i]<-1
  }
}
y_0_test <- test_y
for(i in 1:length(test_y)){
  y_0_test[i]<-0
  if(test_y[i] == 0){
    y_0_test[i]<-1
  }
}
pen_svm_model = sparseSVM(X=train_design_x0, y = y_0_train, alpha = 0.5)
pen_svm_pred = predict(pen_svm_model, test_design_x0)
table(pen_svm_pred[,1], y_0_test)

# 1 as class 1, 0,2 as class 0
y_1_train <- train_y
for(i in 1:length(train_y)){
  y_1_train[i]<-0
  if(train_y[i] == 1){
    y_1_train[i]<-1
  }
}
y_1_test <- test_y
for(i in 1:length(test_y)){
  y_1_test[i]<-0
  if(test_y[i] == 1){
    y_1_test[i]<-1
  }
}
pen_svm_model = sparseSVM(X=train_design_x0, y = y_1_train, alpha = 0.5)
pen_svm_pred = predict(pen_svm_model, test_design_x0)
table(pen_svm_pred[,1], y_1_test)


# 2 as class 1, 0,1 as class 0
y_2_train <- train_y
for(i in 1:length(train_y)){
  y_2_train[i]<-0
  if(train_y[i] == 2){
    y_2_train[i]<-1
  }
}
y_2_test <- test_y
for(i in 1:length(test_y)){
  y_2_test[i]<-0
  if(test_y[i] == 2){
    y_2_test[i]<-1
  }
}
pen_svm_model = sparseSVM(X=train_design_x0, y = y_2_train, alpha = 0.5)
pen_svm_pred = predict(pen_svm_model, test_design_x0)
table(pen_svm_pred[,1], y_2_test)

