#Question 1
data=load(file = '', envir = parent.frame(), verbose = FALSE)
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



# Logistic Regression
lr_model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
fitted.results <- predict(lr_model, test_design_x0, type='response')


# GLMNET
library(glmnet)
glm_model <- glmnet(x=train_design_x0, y=as.factor(train_y), family='multinomial') # binomial
glm_predict <- predict(glm_model, test_design_x0, type="response")[,,1]
glm_labels <- colnames(glm_predict)[apply(glm_predict,1,which.max)]
glm_factors<-factor(c(glm_labels), levels = colnames(glm_predict))
table(glm_factors, as.factor(test_y))




