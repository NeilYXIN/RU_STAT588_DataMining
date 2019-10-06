library(magrittr)
library(dplyr)
library(caret)
library(shiny)
library(glmnet)

split = function(filepath){
  set.seed(2016)
  
  diamonds <-read.csv(filepath, header = TRUE, sep = ',')
  # Drop the index column
  diamonds<- diamonds[ , -which(names(diamonds)=='X')]
  
  #transfer these categorical features to numeric
  diamonds$cut <- as.integer(diamonds$cut)
  diamonds$color <-as.integer(diamonds$color)
  diamonds$clarity <- as.integer(diamonds$clarity)
  head(diamonds)
  
  # Create features and target matrixes
  X <- diamonds %>% 
    select(carat, depth, table, x, y, z, clarity, cut, color)
  y <- diamonds$price
  
  y <- log(y)
  
  # Scale data
  preprocessParams<-preProcess(X, method = c("center", "scale"))
  X <- predict(preprocessParams, X)
  
  # Spliting training set into two parts based on outcome: 75% and 25%
  index <- createDataPartition(y, p=0.9, list=FALSE)
  X_train <- X[ index, ]
  X_test <- X[-index, ]
  y_train <- y[index]
  y_test<-y[-index]
  X_train <- data.matrix(X_train)
  X_test <- data.matrix(X_test)
  return(c(X_train,X_test,y_train,y_test))
}

plot = function(X_train,y_train,a){
  par(mfrow=c(1,3))
  Plot <<- plot(u<-cv.glmnet(X_train,y_train,alpha=a))
  # Plot full solution path
  Plot <<- plot(glmnet(X_train,y_train,alpha=a ))
  lam=c(u$lambda.1se,u$lambda.min)
  v <- glmnet(X_train,y_train,alpha=a,lambda=lam)
  # Plot Lambda path
  Plot <<- plot(v)
}

predict = function(X_train,X_test,y_train,y_test,a){
  u<-cv.glmnet(X_train,y_train,alpha=a)
  best_lambda = u$lambda.min
  #train again
  lasso_best <- glmnet(X_train, y_train, alpha = a, lambda = best_lambda)
  pred <- predict(lasso_best, s = best_lambda, newx = X_test)
  result <- cbind(y_test, pred)
  colnames(result)[2] <- "predictor"
  return (result)
}

ui <- fluidPage(
  fluidRow(
    column(3, wellPanel(
      fileInput("file", "Choose Datasest File", multiple = FALSE, accept = NULL,
                width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected"),
      textInput("alph", "alph", value = ""),
      actionButton("submit","Run Regression")
    ))
  ),
  mainPanel(
    plotOutput("plots"),
    tableOutput("tables")
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    pere = split(input$file$datapath)
    output$plots <- renderPlot({
      plot(pere[1],pere[2], input$alph)
    })
    output$tables <- renderTable({
      predict(pere[1],pere[2],pere[3],pere[4], input$alph)
    })
  })
}


shinyApp(ui = ui, server = server)