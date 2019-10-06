library(magrittr)
library(dplyr)
library(caret)
library(shiny)
library(glmnet)
# Upload the dataset

diamonds <-read.csv('/Users/yi/Desktop/diamonds.csv', header = TRUE, sep = ',')
head(diamonds)

# Drop the index column
diamonds<- diamonds[ , -which(names(diamonds)=='X')]
head(diamonds)

# Print unique values of text features
print(levels(diamonds$cut))
print(levels(diamonds$clarity))
print(levels(diamonds$color))

diamonds$cut <- as.integer(diamonds$cut)
diamonds$color <-as.integer(diamonds$color)
diamonds$clarity <- as.integer(diamonds$clarity)
head(diamonds)

# Create features and target matrixes
X <- diamonds %>% 
  select(carat, depth, table, x, y, z, clarity, cut, color)
y <- diamonds$price

# Scale data
preprocessParams<-preProcess(X, method = c("center", "scale"))
X <- predict(preprocessParams, X)

# Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(y, p=0.75, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

# Create and fit Lasso and elastic net
lasso<-train(y= y_train,
             x = X_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 1, lambda = 1)
             ) 

elnet<-train(y= y_train,
             x = X_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0.5, lambda = 1)
             ) 

# Make the predictions
predictions_lasso <- lasso %>% predict(X_test)
predictions_elnet <- elnet %>% predict(X_test)
#data.frame( predictions_lasso, 
#            predictions_elnet)


# Print R squared scores
data.frame(
  Lasso_R2 = R2(predictions_lasso, y_test),
  Elnet_R2 = R2(predictions_elnet, y_test))

# Print RMSE
data.frame(
  Lasso_RMSE = RMSE(predictions_lasso, y_test),
  Elnet_RMSE = RMSE(predictions_elnet, y_test)
)

# Print coeficients
data.frame(
  lasso_ = as.data.frame.matrix(coef(lasso$finalModel, lasso$bestTune$lambda)),
  elnet_ = as.data.frame.matrix(coef(elnet$finalModel, elnet$bestTune$lambda))
) %>%     rename(Lasso_coef = X1, elnet_coef = X1.1)



#ui <- fluidPage('Hello World!')
#server <- function(input, output){}
#shinyApp(ui=ui, server=server)
