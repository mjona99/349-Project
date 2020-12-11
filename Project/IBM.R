library(readxl)
library(tseries)
library(forecast)
library(TSA)

ibm <- as.data.frame(read_ibmel("Project.xlsx", sheet = 8))
ibm[1,8] <- 0
ibm[,8] <- as.numeric(ibm[,8])
ibm_neg.log.return <- ibm[,8]
ibm_last.five <- ibm_neg.log.return[(length(ibm_neg.log.return)-4):length(ibm_neg.log.return)]
ibm_without.last.five <- ibm_neg.log.return[1:(length(ibm_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(ibm_without.last.five) # significant at a handful of lags
eacf(ibm_without.last.five) # maybe ARMA(0, 1) or ARMA(1, 1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x o o x o x x x x x o  o  o  x 
# 1 x o o x o x o o x x o  o  o  o 
# 2 x x o o o o o o x x o  o  o  o 
# 3 x x x o o o o o x x o  o  o  o 
# 4 x x x o o o o x x x x  o  o  o 
# 5 x x x x o o o x x x o  o  o  x 
# 6 x x x x x x o o x x o  o  o  o 
# 7 x x x x x x x o x x o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(ibm_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(1, 1) with d = 0 
model_101 = arima(ibm_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(ibm_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(ibm_without.last.five, order = c(1,1,1))
model_111$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_011 = predict(model_011, n.ahead = 5)$pred
forecast_011

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_001=data.frame(actual=ibm_last.five, forecast = forecast_001, error=forecast_001 - ibm_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=ibm_last.five, forecast = forecast_101, error=forecast_101 - ibm_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=ibm_last.five, forecast = forecast_011, error=forecast_011 - ibm_last.five)
fore_011

#See the forecasting
fore_111 = data.frame(actual=ibm_last.five, forecast = forecast_111, error=forecast_111 - ibm_last.five)
fore_111

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5 

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(0,1) for the garch

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5 

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1))) #possible Box-Ljung test: p-value =  0.5325 
summary(garch(model_101$residuals, order = c(1,2))) #possible Box-Ljung test: p-value =  0.493 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_101$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_101$residuals, order = c(2,2))) #possible Box-Ljung test: p-value = 0.8534

m11 <- garch(model_101$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant no lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(ibm[,1])), ibm$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(ibm$Close),10))

plot(c(1000:3000), ibm$Close[c(1000:3000)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(ibm$Close),10))


ibm_split = ibm_neg.log.return[c(1000:3000)]
ibm_split_last.five <- ibm_split[(length(ibm_split)-4):length(ibm_split)]
ibm_split_without.last.five <- ibm_split[1:(length(ibm_split) - 5)]

acf(ibm_split_without.last.five) # significant at two lags
eacf(ibm_split_without.last.five) # ARMA(0, 1), ARMA (1,1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 o o o o x o o o o o o  o  o  o 
# 1 x o o o o o o o o o o  o  o  o 
# 2 x x o o o o o o o o o  o  o  o 
# 3 x x x o o o o o o o o  o  o  o 
# 4 x x x x o o o o o o o  o  o  o 
# 5 x x x x x o o o o o o  o  o  o 
# 6 x x x x o x o o o o o  o  o  o 
# 7 x x x x o x o o o o o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(ibm_split_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(1, 1) with d = 0 
model_101 = arima(ibm_split_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(ibm_split_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(ibm_split_without.last.five, order = c(1,1,1))
model_111$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_011 = predict(model_011, n.ahead = 5)$pred
forecast_011

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_001=data.frame(actual=ibm_split_last.five, forecast = forecast_001, error=forecast_001 - ibm_split_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=ibm_split_last.five, forecast = forecast_101, error=forecast_101 - ibm_split_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=ibm_split_last.five, forecast = forecast_011, error=forecast_011 - ibm_split_last.five)
fore_011

#See the forecasting
fore_111 = data.frame(actual=ibm_split_last.five, forecast = forecast_111, error=forecast_111 - ibm_split_last.five)
fore_111

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(0,1) for the garch

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5 

#GARCH selection
summary(garch(model_001$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.5479 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_001$residuals, order = c(1,2))) #possible Box-Ljung test: p-value =  0.7281 
summary(garch(model_001$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_001$residuals, order = c(2,2))) #possible Box-Ljung test: p-value = 0.9628

m11 <- garch(model_001$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant no lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 



