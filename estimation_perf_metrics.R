## Functions for performance metrics of estimation models

## a <- actual, p <- predicted
#mean absolute error
mae <- function(actual, predicted){
  mae <- mean(abs(actual - predicted))
  return (mae)
}

#mean absolute percentage error
mape <- function(actual, predicted){
  mape <- mean(abs((actual - predicted)/actual))*100
  return (mape)
}

#set the tolerance level to whichever you want
tolerance <- function(actual, predicted, tol){
       actual <- as.data.frame(actual)
       predicted <- as.data.frame(predicted)
       count <- 0
       for (i in 1:nrow(actual)) {
          
         if (abs(actual[i,] - predicted[i,]) < abs(tol*actual[i,])) count <- count + 1
         
       }
      tolf <- count/nrow(actual)
      return(tolf)
}

#mean error
me <- function(actual, predicted){
  me <- mean(actual - predicted)
  return (me)
}

#mean positive error
mpe <- function(actual, predicted){
    actual <- as.data.frame(actual)
    predicted <- as.data.frame(predicted)
    sum <- 0
    count <- 0
    for (i in 1:nrow(actual)) {
      
      if (actual[i,] - predicted[i,] > 0) 
      {
        sum <- sum + (actual[i,] - predicted[i,])
        count <- count + 1 
      }
      
    }
    mnp <- sum/count
    return(mnp)
}

#mean negative error
mne <- function(actual, predicted){
  actual <- as.data.frame(actual)
  predicted <- as.data.frame(predicted)
  sum <- 0
  count <- 0
  for (i in 1:nrow(actual)) {
    
    if (actual[i,] - predicted[i,] < 0) 
    {
      sum <- sum + (actual[i,] - predicted[i,])
      count <- count + 1 
    }
    
  }
  mnn <- sum/count
  return(mnn)
}

#x <- regressor: object in which your model is built
#Root mean squared error
mse <- function(x){
  mse <- mean(residuals(x)^2)
  return(mse)
}

#Residual sum of squares
rss <- function(x){
  rss <- sum(residuals(x)^2)
  return(rss)
}

#Residual standard error
rse <- function(x){
  rse <- sqrt(sum(residuals(regressor1)^2) / regressor1$df.residual)
  return(rse)
}
