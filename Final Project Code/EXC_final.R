library(readxl)
library(tseries)
library(forecast)
library(TSA)

exc <- as.data.frame(read_excel("Project.xlsx", sheet = 5))
exc[1,8] <- 0
exc[,8] <- as.numeric(exc[,8])
exc_neg.log.return <- exc[,8]
exc_last.five <- exc_neg.log.return[(length(exc_neg.log.return)-4):length(exc_neg.log.return)]
exc_without.last.five <- exc_neg.log.return[1:(length(exc_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(exc_without.last.five) 
eacf(exc_without.last.five) 

#fit ARMA(1, 1) with d = 0 
model_101 = arima(exc_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(exc_without.last.five, order = c(1,1,1))
model_111$residuals

model_102 = arima(exc_without.last.five, order = c(1,0,2))
model_102$residuals

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

forecast_102 = predict(model_102, n.ahead = 5)$pred
forecast_102

#See the forecasting
fore_101=data.frame(actual=exc_last.five, forecast = forecast_101, error=forecast_101 - exc_last.five)
fore_101

#See the forecasting
fore_111=data.frame(actual=exc_last.five, forecast = forecast_111, error=forecast_111 - exc_last.five)
fore_111

fore_102=data.frame(actual=exc_last.five, forecast = forecast_102, error=forecast_102 - exc_last.five)
fore_102

sum((fore_102$forecast - fore_102$actual)^2) / 5 

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1))) 
summary(garch(model_101$residuals, order = c(1,2))) 
summary(garch(model_101$residuals, order = c(2,1)))
summary(garch(model_101$residuals, order = c(2,2))) 

m11 <- garch(model_101$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

plot(c(1:length(exc[,1])), exc$`Neg. log return`, type = 'l',  xlab = "Day", ylab = "Neg. log return", xaxp = c(0,length(exc$`Neg. log return`),10))
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
plot(c(1:length(exc[,1])), exc$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(exc$Close),10))

plot(c(1000:3500), exc$Close[c(1000:3500)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(exc$Close),10))


exc_split = exc_neg.log.return[c(1000:3500)]
exc_split_last.five <- exc_split[(length(exc_split)-4):length(exc_split)]
exc_split_without.last.five <- exc_split[1:(length(exc_split) - 5)]

acf(exc_split_without.last.five) 
eacf(exc_split_without.last.five) 

#fit ARMA(1, 1) with d = 0 
model_000 = arima(exc_split_without.last.five, order = c(1,0,1))
model_000$residuals

#fit ARMA(1, 1) with d = 1 
model_010 = arima(exc_split_without.last.five, order = c(1,1,1))
model_010$residuals

#predict the last five
forecast_000 = predict(model_000, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_010 = predict(model_010, n.ahead = 5)$pred
forecast_010

#See the forecasting
fore_000=data.frame(actual=exc_split_last.five, forecast = forecast_000, error=forecast_000 - exc_split_last.five)
fore_000

#See the forecasting
fore_010=data.frame(actual=exc_split_last.five, forecast = forecast_010, error=forecast_010 - exc_split_last.five)
fore_010

#MSFE
sum((fore_000$forecast - fore_000$actual)^2) / 5

#MSFE
sum((fore_010$forecast - fore_010$actual)^2) / 5 

#GARCH selection
summary(garch(model_010$residuals, order = c(1,1)))
summary(garch(model_010$residuals, order = c(1,2))) 
summary(garch(model_010$residuals, order = c(2,1)))
summary(garch(model_010$residuals, order = c(2,2)))

sum((garch(model_002$residuals, order = c(1,1))$residuals)^2, na.rm=T)
sum((garch(model_002$residuals, order = c(1,2))$residuals)^2, na.rm=T)


m11 <- garch(model_010$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

shapiro.test(na.omit(residuals(m11)))

jarque.bera.test(na.omit(residuals(m11)))
skewness(na.omit(residuals(m11)))
kurtosis(na.omit(residuals(m11)))
acf(na.omit(residuals(m11)^2))
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) 

