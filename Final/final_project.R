#Data Mininng Final Project
library(e1071)
library(pROC)
library(randomForest)
library(adabag)

data<-read.table('/Users/wjhmike/Desktop/Data\ Minning/Project/pex23.txt')
features <- data[1:23]
responses <- data[24]

pex23_predict<-function(features,responses,persent_testing=0.2){
  set.seed(101)
  smp_siz <- floor((1-persent_testing)*nrow(features))
  train_ind <- sample(seq_len(nrow(features)),size = smp_siz)
  train_x <- features[train_ind,]
  train_y <- responses[train_ind,]
  test_x <- features[-train_ind,]
  test_y <- responses[-train_ind,]
  
  svm_model <- svm(x = train_x, y = train_y,type="C-classification", kernel = "radial")
  #tune.out=tune(svm, train.x = train_x, y = train_y,type="C-classification", kernel = "radial", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
  #svm_model <- tune.out$best.model
  svm_trainPre <- predict(svm_model,newdata = train_x)
  svm_testPre <- predict(svm_model,newdata = test_x)
  svm_trainError <- mean(train_y != svm_trainPre)
  svm_testError <- mean(test_y != svm_testPre)
  svm_auc <- auc(test_y, as.numeric(svm_testPre))
  
  RF_model <- randomForest(as.factor(train_y) ~ ., data=train_x)
  RF_trainPre <- predict(RF_model,newdata = train_x)
  RF_testPre <- predict(RF_model,newdata = test_x)
  RF_trainError <- mean(train_y != RF_trainPre)
  RF_testError <- mean(test_y != RF_testPre)
  RF_auc <- auc(test_y, as.numeric(RF_testPre))
  
  train_data <- cbind(train_x,train_y)
  ada_boosting <- boosting(train_y~., data=train_data)
  ada_trainPre <- predict.boosting(ada_boosting,newdata = train_x)
  ada_testPre <- predict.boosting(ada_boosting,newdata = test_x)
  ada_trainError <- mean(train_y != ada_trainPre$class)
  ada_testError <- mean(test_y != ada_testPre$class)
  ada_auc <- auc(test_y, as.numeric(ada_testPre$class))
  
  
  
  summary <- matrix(c(svm_trainError,svm_testError,svm_auc, RF_trainError,RF_testError,RF_auc,ada_trainError,ada_testError,ada_auc), ncol=3, byrow = TRUE)
  colnames(summary) <- c('train error', "test error", "AUC")
  rownames(summary) <- c('SVM', "Random Forest", "Ada Boosting")
  summary <- as.table(summary)
  summary
}

pex23_predict(features,responses)


