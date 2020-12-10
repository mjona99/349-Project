library(TSA)
library(tseries)
dgx <- read.csv("DGX.csv")

dgx[1,8] <- 0
dgx[,8] <- as.numeric(dgx[,8])
dgx_neg.log.return <- dgx[,8]
dgx_last.five <- dgx[(length(dgx[,1])-4):length(dgx[,1]),]
dgx_train <- dgx[1:(length(dgx[,1])-5),]

plot(c(1:length(dgx_train[,1])), dgx_train$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(dgx_train$Close),10))

spl = 3000

plot(c(1:spl), dgx_train$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(dgx_train$Close),10))

dgx_split = dgx[c(1:spl),]

acf(dgx_train$Neg..log.return)
eacf(dgx_train$Neg..log.return)
#AR/MA
#  0 1 2 3 4 5 6 7 8 9 10 11 12 13
#0 x o o o o o o x x o o  o  o  o 
#1 o o o o o o o o x o o  o  o  o 
#2 x o o o o o o o x o o  o  o  o 
#3 x x x o o o o o x o o  o  o  o 
#4 x x x x o o o o x o o  o  o  o 
#5 x x x x o o o o x o o  o  o  o 
#6 x x x x x x o o o o o  o  o  o 
#7 x x o x o x o o o o o  o  o  o 

 #fit ARMA(1,0)
model_dgx <- arima(dgx_train$Neg..log.return, order = c(1,0,0))
plot(model_dgx$residuals)

#fit ARMA(2,1)
model_dgx_2 <- arima(dgx_train$Neg..log.return, order = c(2,0,1))
plot(model_dgx_2$residuals)

#predict the last five
forecast_1 = predict(model_dgx, n.ahead = 5)$pred
forecast_1

#predict the last five
forecast_2 = predict(model_dgx_2, n.ahead = 5)$pred
forecast_2

#See the forecasting
fore_1 = data.frame(actual = dgx_last.five$Neg..log.return, 
                    forecast = forecast_1, error = forecast_1 - dgx_last.five$Neg..log.return)
fore_1
mean(fore_1$error)
sum(fore_1$error)

#See the forecasting
fore_2 = data.frame(actual = dgx_last.five$Neg..log.return, 
                    forecast = forecast_2, error = forecast_2 - dgx_last.five$Neg..log.return)
fore_2
mean(fore_2$error)
sum(fore_2$error)

#MSFE
sum((fore_1$forecast - fore_1$actual)^2)/5

#MSFE
sum((fore_2$forecast - fore_2$actual)^2)/5

#We will use the ARMA(1,0) since it has a smaller MSFE

#GARCH selection
summary(garch(model_dgx$residuals, order = c(1,1)))
#possible Box-Ljung test: p-value = 0.5912
#coeffs are significant
summary(garch(model_dgx$residuals, order = c(1,2)))
##possible Box-Ljung test: p-value = 0.637
#coeffs are significant
#summary(garch(model_dgx$residuals, order = c(2,1)))
#singular information
#summary(garch(model_dgx$residuals, order = c(2,2)))
#singular information

m11 <- garch(model_dgx$residuals, order = c(1,1))

shapiro.test(na.omit(residuals(m11))) #p-value < 2.2e-16

acf(na.omit(residuals(m11)^2)) #none significant
qqnorm(na.omit(residuals(m11)))
qqline(na.omit(residuals(m11))) 

#############################################################################

#Split Data
dgx_split_train <- dgx_split[c(1:(length(dgx_split[,1]) - 5)),]
dgx_split_last_five <- dgx_split[c((length(dgx_split[,1])-4):length(dgx_split[,1])),]

acf(dgx_split_train$Neg..log.return)
eacf(dgx_split_train$Neg..log.return)
AR/MA
#  0 1 2 3 4 5 6 7 8 9 10 11 12 13
#0 o x o o o o o o o o x  o  o  o 
#1 x x o o o o o o o o o  o  o  o 
#2 x x o o o o o o o o o  o  o  o 
#3 x x o o o o o o o o o  o  o  o 
#4 x x x x o o o o o o o  o  o  o 
#5 x x x x x o o o o o o  o  o  o 
#6 x x x x x o o o o o o  o  o  o 
#7 x x x x x o o o o o o  o  o  o 

#ARMA(2,2)
model_split <- arima(dgx_split_train$Neg..log.return, order = c(2,0,2))
plot(model_split$residuals)

#ARMA(3,2)
model_split_2 <- arima(dgx_split_train$Neg..log.return, order = c(3,0,2))
plot(model_split_2$residuals)

#Forecasting
forecast_split = predict(model_split, n.ahead = 5)$pred
forecast_split

#Forecasting
forecast_split_2 = predict(model_split_2, n.ahead = 5)$pred
forecast_split_2

#See Forecasting
fore_split = data.frame(actual = dgx_split_last_five$Neg..log.return, 
                    forecast = forecast_split, error = forecast_split - dgx_split_last_five$Neg..log.return)
fore_split

#See Forecasting
fore_split_2 = data.frame(actual = dgx_split_last_five$Neg..log.return, 
                          forecast = forecast_split_2, error = forecast_split - dgx_split_last_five$Neg..log.return)
fore_split_2

#MSFE
sum((fore_split$forecast - fore_split$actual)^2)/5

#MSFE
sum((fore_split_2$forecast - fore_split_2$actual)^2)/5

#We will use ARMA(2,2)

#GARCH Selection
summary(garch(model_split$residuals, order = c(1,1)))
#possible Box-Ljung test: p-value = 0.6126
#coeffs are significant
summary(garch(model_split$residuals, order = c(1,2)))
##possible Box-Ljung test: p-value = 0.6662
#coeffs are significant
#summary(garch(model_dgx$residuals, order = c(2,1)))
#singular information
#summary(garch(model_dgx$residuals, order = c(2,2)))
#singular information

m22 <- garch(model_split$residuals, order = c(1,1))
