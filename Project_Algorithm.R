library(TSA)
library(tseries)

#FUNCTIONS
#reads in csv, returns 3 data frames, full stock, training set, and test set
#to get train set run x <- data_clean(filename)$train
#to get test set run x <- data_clean(filename)$test
#to get full set run x <- data_clean(filename)$base
data_clean <- function(filename) {
  stock <- read.csv(filename)
  stock[1,8] <- 0
  stock[,8] <- as.numeric(stock[,8])
  stock_train <- stock[1:(length(stock[,1])-5),]
  stock_test <- stock[(length(stock[,1])-4):length(stock[,1]),]
  return(list(train = stock_train, test = stock_test, base = stock))
}

#displays both acf and eacf
choose_model <- function(train) {
  acf(train[,8])
  eacf(train[,8])
}

#Input 2 sets of ARMA models to test
#ex: if testing ARMA(1,2) and ARMA(3,4), with data "train" and "test" input:
#test_arimas(train, 1,2,3,4, test)
#output is the msfe of the best model after trying each ARIMA with d=0 and d=1
#the function will concatenate the best model to you
test_arimas <- function(train,p1,q1,p2,q2,test) {
  m10 <- arima(train[,8], order = c(p1,0,q1))
  m11 <- arima(train[,8], order = c(p1,1,q1))
  m20 <- arima(train[,8], order = c(p2,0,q2))
  m21 <- arima(train[,8], order = c(p2,1,q2))
  
  f10 <- predict(m10, n.ahead = 5)$pred
  f11 <- predict(m11, n.ahead = 5)$pred
  f20 <- predict(m20, n.ahead = 5)$pred
  f21 <- predict(m21, n.ahead = 5)$pred
  
  for10 <- data.frame(actual = test[,8], forecast = f10, error = f10 - test[,8])
  for11 <- data.frame(actual = test[,8], forecast = f11, error = f11 - test[,8])
  for20 <- data.frame(actual = test[,8], forecast = f20, error = f20 - test[,8])
  for21 <- data.frame(actual = test[,8], forecast = f21, error = f21 - test[,8])
  
  msfe10 <- sum((for10$forecast - for10$actual)^2)/5
  msfe11 <- sum((for11$forecast - for11$actual)^2)/5
  msfe20 <- sum((for20$forecast - for20$actual)^2)/5
  msfe21 <- sum((for21$forecast - for21$actual)^2)/5
  
  best <- which.max(c(msfe10,msfe11,msfe20,msfe21))
  
  if (best == 1) {
    cat("Use ARIMA(",p1,",0,",q1,") \n MSFE =", msfe10)
    return(msfe10)
  }
  else if (best == 2) {
    cat("Use ARIMA(",p1,",1,",q1,") \n MSFE =", msfe11)
    return(msfe11)
  }
  else if (best == 3) {
    cat("Use ARIMA(",p2,",0,",q2,") \n MSFE =", msfe20)
    return(msfe20)
  }
  else if (best == 4) {
    cat("Use ARIMA(",p2,",1,",q2,") \n MSFE =", msfe21)
    return(msfe21)
  }
}

#splits data at point spl and returns train and test data sets for that
#to get train set run x <- split_data(df,spl)$train
#to get test set run x <- split_data(df,spl)$test
split_data <- function(df, spl) {
  data_train <- df[0:(spl-5),]
  data_test <- df[(spl-4):spl,]
  return(list(train = data_train, test = data_test))
}

iff <- data_clean("IFF.csv")$base
iff_train <- data_clean("IFF.csv")$train
iff_test <- data_clean("IFF.csv")$test

choose_model(iff_train)

msfe <- test_arimas(iff_train, 2, 1, 4, 4, iff_test)

iff_model <- arima(iff_train[,8], order = c(4,1,4))
msfe

plot(iff$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(iff$Close),10))

iff_split_train <- split_data(iff,2650)$train
iff_split_test <- split_data(iff,2650)$test

choose_model(iff_split_train)

msfe <- test_arimas(iff_split_train, 1,1,0,1, iff_split_test)
iff_split_model <- arima(iff_split_train[,8], order = c(1,0,1))
msfe

#GARCH selection
summary(garch(iff_model$residuals, order = c(1,1)))

summary(garch(iff_model$residuals, order = c(1,2)))

#summary(garch(iff_model$residuals, order = c(2,1)))

#summary(garch(iff_model$residuals, order = c(2,2)))

final_iff_model <- garch(iff_model$residuals, order = c(1,1))

#Split GARCH Selection
summary(garch(iff_split_model$residuals, order = c(1,1)))

summary(garch(iff_split_model$residuals, order = c(1,2)))

#summary(garch(iff_split_model$residuals, order = c(2,1)))

#summary(garch(iff_split_model$residuals, order = c(2,2)))

final_iff_split_model <- garch(iff_split_model$residuals, order = c(1,2))
