library(readxl)
library(tseries)
library(forecast)
library(TSA)

duk <- as.data.frame(read_excel("Project.xlsx", sheet = 3))
duk[1,8] <- 0
duk[,8] <- as.numeric(duk[,8])
duk_neg.log.return <- duk[,8]
duk_last.five <- duk_neg.log.return[(length(duk_neg.log.return)-4):length(duk_neg.log.return)]
duk_without.last.five <- duk_neg.log.return[1:(length(duk_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(duk_without.last.five)
eacf(duk_without.last.five) 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(duk_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(0, 2) with d = 0 
model_002 = arima(duk_without.last.five, order = c(0,0,2))
model_002$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(duk_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(0, 2) with d = 1 
model_012 = arima(duk_without.last.five, order = c(0,1,2))
model_012$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_002 = predict(model_002, n.ahead = 5)$pred
forecast_002

#predict the last five
forecast_011 = predict(model_011, n.ahead = 5)$pred
forecast_011

#predict the last five
forecast_012 = predict(model_012, n.ahead = 5)$pred
forecast_012

#See the forecasting
fore_001=data.frame(actual=duk_last.five, forecast = forecast_001, error=forecast_001 - duk_last.five)
fore_001

#See the forecasting
fore_002=data.frame(actual=duk_last.five, forecast = forecast_002, error=forecast_002 - duk_last.five)
fore_002

#See the forecasting
fore_011=data.frame(actual=duk_last.five, forecast = forecast_011, error=forecast_011 - duk_last.five)
fore_011

#See the forecasting
fore_012=data.frame(actual=duk_last.five, forecast = forecast_012, error=forecast_012 - duk_last.five)
fore_012

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_002$forecast - fore_002$actual)^2) / 5 

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_012$forecast - fore_012$actual)^2) / 5 

#GARCH selection
summary(garch(model_002$residuals, order = c(1,1))) 
summary(garch(model_002$residuals, order = c(1,2))) 
summary(garch(model_002$residuals, order = c(2,1)))
summary(garch(model_002$residuals, order = c(2,2))) 

AIC(garch(model_002$residuals, order = c(1,1))) 
AIC(garch(model_002$residuals, order = c(1,2)))

m11 <- garch(model_002$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) 

sum((garch(model_002$residuals, order = c(1,2))$residuals)^2, na.rm=T) 

plot(c(1:length(duk[,1])), duk$`Neg. log return`, type = 'l',  xlab = "Day", ylab = "Neg. log return", xaxp = c(0,length(duk$`Neg. log return`),10))
plot(m11$residuals)


shapiro.test(na.omit(residuals(m11)))

jarque.bera.test(na.omit(residuals(m11)))
skewness(na.omit(residuals(m11)))
kurtosis(na.omit(residuals(m11)))
acf(na.omit(residuals(m11)^2)) 
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11)))
################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(duk[,1])), duk$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(duk$Close),10))

spl = 3100

plot(c(1:spl), duk$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(duk$Close),10))


duk_split = duk_neg.log.return[c(1:spl)]
duk_split_last.five <- duk_split[(length(duk_split)-4):length(duk_split)]
duk_split_without.last.five <- duk_split[1:(length(duk_split) - 5)]

acf(duk_split_without.last.five) 
eacf(duk_split_without.last.five) 

#fit ARMA(0, 3) with d = 0 
model_003 = arima(duk_split_without.last.five, order = c(0,0,3))
model_003$residuals

#fit ARMA(0, 3) with d = 1 
model_013 = arima(duk_split_without.last.five, order = c(0,1,3))
model_013$residuals

#fit ARMA(1, 3) with d = 0
model_103 = arima(duk_split_without.last.five, order = c(1,0,3))
model_103$residuals

#fit ARMA(1, 3) with d = 1
model_113 = arima(duk_split_without.last.five, order = c(1,1,3))
model_113$residuals

#predict the last five
forecast_003 = predict(model_003, n.ahead = 5)$pred
forecast_003

#predict the last five
forecast_013 = predict(model_013, n.ahead = 5)$pred
forecast_013

#predict the last five
forecast_103 = predict(model_103, n.ahead = 5)$pred
forecast_103

#predict the last five
forecast_113 = predict(model_113, n.ahead = 5)$pred
forecast_113

#See the forecasting
fore_003=data.frame(actual=duk_split_last.five, forecast = forecast_003, error=forecast_003 - duk_split_last.five)
fore_003

#See the forecasting
fore_013=data.frame(actual=duk_split_last.five, forecast = forecast_013, error=forecast_013 - duk_split_last.five)
fore_013

#See the forecasting
fore_103=data.frame(actual=duk_split_last.five, forecast = forecast_103, error=forecast_103 - duk_split_last.five)
fore_103

#See the forecasting
fore_113=data.frame(actual=duk_split_last.five, forecast = forecast_113, error=forecast_113 - duk_split_last.five)
fore_113

#MSFE
sum((fore_003$forecast - fore_003$actual)^2) / 5

#MSFE
sum((fore_013$forecast - fore_013$actual)^2) / 5 

#MSFE
sum((fore_103$forecast - fore_103$actual)^2) / 5

#MSFE
sum((fore_113$forecast - fore_113$actual)^2) / 5

#GARCH selection
summary(garch(model_103$residuals, order = c(1,1))) 
summary(garch(model_103$residuals, order = c(1,2)))
summary(garch(model_103$residuals, order = c(2,1)))
summary(garch(model_103$residuals, order = c(2,2)))

m11 <- garch(model_103$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model


shapiro.test(na.omit(residuals(m11))) 

jarque.bera.test(na.omit(residuals(m11)))
skewness(na.omit(residuals(m11)))
kurtosis(na.omit(residuals(m11)))
acf(na.omit(residuals(m11)^2)) 
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11)))

