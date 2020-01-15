require(stats)
require(splines)
require(DAAG)
require(ISLR)
attach(Wage)
head(Wage)

# Wage dataset
X=Wage$age
y=Wage$wage

# Generate a list of knots based on the quantity of data points
get_knots<-function(X, num_knots) {
  sorted_X=sort(X)
  knots_list=vector()
  for (i in 1:num_knots) {
    knots_list <- c(knots_list, sorted_X[length(X) / (num_knots+1) * i])
  }
  return(knots_list)
}

# Cubic spline for LOOCV
cubic_spline<-function(train_X, train_y, test_X, test_y, knots_list) {
  fit <- lm(train_y ~ bs(train_X,knots = knots_list))
  residuals=residuals(fit)
  summary=summary(fit)
  df<-summary$df[2]
  prediction<-predict(fit, newdata=list(train_X = test_X))
  rss=(test_y-prediction)^2
  return(list(rss=rss,residuals=residuals, df=df, model=fit, knots_list=knots_list))
}

# Leave-one-out-cross-validation
cross_validation<-function(X,y,num_knots, sorted_X) {
  knots_list<-get_knots(X, num_knots)
  min_rss<-.Machine$integer.max
  optimal_model<-list()
  rsses<-vector()
  for(test_index in 1:length(X)) {
    train_X <- X[-test_index]
    train_y <- y[-test_index]
    test_X <-X [test_index]
    test_y <-y [test_index]
    res=cubic_spline(train_X = train_X, train_y = train_y, test_X = test_X, test_y = test_y, knots_list = knots_list)
    
    rsses<-c(rsses, res$rss)
    if(res$rss < min_rss) {
      optimal_model<-res
      min_rss<-res$rss
    }
  }
  cv_rss=mean(rsses)
  #Plotting the Cubic Spline Regression to the Scatterplot   
  plot(X,y,col="grey",xlab="Age",ylab="Wages")
  points(unique(sorted_X),predict(optimal_model$model, newdata = list(train_X=unique(sorted_X))),col="blue",lwd=2,type="l")
  abline(v=knots_list,lty=2,col="darkgreen")
  return(list(residuals=optimal_model$residuals, df=optimal_model$df, model=optimal_model$model, cv_rss=cv_rss, knots_list=optimal_model$knots_list))
}

# Find the optimal number of knots
find_optimal<-function(X, y, max_knots) {
  optimal_knots<-0
  optimal_knot_model<-list()
  RSS<-vector()
  DFs<-vector()
  minRSS<-.Machine$integer.max
  sorted_X<-sort(X)
  for(num_knots in 1:max_knots) {
    cv_res<-cross_validation(X,y,num_knots,sorted_X)
    RSS<-c(RSS, cv_res$cv_rss)
    if (RSS[length(RSS)] < minRSS) {
      optimal_res<-cv_res
      optimal_knots<-num_knots
      minRSS<-RSS[length(RSS)]
    }
    DFs<-c(DFs, cv_res$df)
  }
  
  # Plot optimal fitted data and data points
  plot(X,y,col="grey",xlab="Age",ylab="Wages")
  fitted<-predict(optimal_res$model, newdata = list(train_X=unique(sorted_X)))
  points(unique(sorted_X), fitted,col="red",lwd=2,type="l")
  abline(v=optimal_res$knots_list,lty=2,col="darkgreen")
  result_obj=list(residuals=optimal_res$residuals,df=optimal_res$df, fitted=fitted, RSS=RSS)
  
 return(list(result_obj=result_obj, RSS=RSS, DFs=DFs, optimal_knots=optimal_knots))
}

optimal_cv_res<-list()
optimal_cv_res<-find_optimal(X, y, 20)
result_object<-optimal_cv_res$result_obj # Object expected
optimal_knots<-optimal_cv_res$optimal_knots # Optimal number of knots
optimal_RSS<-optimal_cv_res$RSS # List of CV'd RSS
optimal_DFs<-optimal_cv_res$DFs # List of DFs

plot(optimal_DFs, optimal_RSS) # plot the relationship between DFs and RSS