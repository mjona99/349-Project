library(readxl)
library(tseries)
library(forecast)
library(TSA)

chtr <- as.data.frame(read_excel("Project.xlsx", sheet = 1))
chtr[1,8] <- 0
chtr[,8] <- as.numeric(chtr[,8])
chtr_neg.log.return <- chtr[,8]
chtr_last.five <- chtr_neg.log.return[(length(chtr_neg.log.return)-4):length(chtr_neg.log.return)]
chtr_without.last.five <- chtr_neg.log.return[1:(length(chtr_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(chtr_without.last.five)
eacf(chtr_without.last.five) 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(chtr_without.last.five, order = c(0,0,1))
model_001$residuals
#fit ARMA(1, 1) with d = 0 
model_101 = arima(chtr_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(chtr_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(chtr_without.last.five, order = c(1,1,1))
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
fore_001=data.frame(actual=chtr_last.five, forecast = forecast_001, error=forecast_001 - chtr_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=chtr_last.five, forecast = forecast_101, error=forecast_101 - chtr_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=chtr_last.five, forecast = forecast_011, error=forecast_011 - chtr_last.five)
fore_011

#See the forecasting
fore_111=data.frame(actual=chtr_last.five, forecast = forecast_111, error=forecast_111 - chtr_last.five)
fore_111

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1)))
summary(garch(model_101$residuals, order = c(1,2))) 
summary(garch(model_101$residuals, order = c(2,1))) 
summary(garch(model_101$residuals, order = c(2,2))) 

AIC(garch(model_101$residuals, order = c(2,2)))
AIC(garch(model_101$residuals, order = c(1,2)))
AIC(garch(model_101$residuals, order = c(1,1)))

m11 <- garch(model_101$residuals, order = c(1,1))

plot(c(1:length(chtr[,1])), chtr$`Neg. log return`, type = 'l',
     xlab = "Day", ylab = "Neg. log return", xaxp = c(0,length(chtr$`Neg. log return`),10))
plot(m11$residuals)

summary(m11)
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

shapiro.test(na.omit(residuals(m11))) 

jarque.bera.test(na.omit(residuals(m11)))
skewness(na.omit(residuals(m11)))
kurtosis(na.omit(residuals(m11)))
acf(na.omit(residuals(m11)^2))
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) 

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(chtr[,1])), chtr$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(chtr$Close),10))

plot(c(1:length(chtr[,1])), chtr$`Neg. log return`, type = 'l',
     xlab = "Day", ylab = "Neg. log return", xaxp = c(0,length(chtr$`Neg. log return`),10))

spl = 2400

plot(c(1:spl), chtr$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(chtr$Close),10))


chtr_split = chtr_neg.log.return[c(1:spl)]
chtr_split_last.five <- chtr_split[(length(chtr_split)-4):length(chtr_split)]
chtr_split_without.last.five <- chtr_split[1:(length(chtr_split) - 5)]

acf(chtr_split_without.last.five)
eacf(chtr_split_without.last.five)

#fit ARMA(0, 1) with d = 0 
model_001 = arima(chtr_split_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(chtr_split_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 0
model_101 = arima(chtr_split_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(1, 1) with d = 1
model_111 = arima(chtr_split_without.last.five, order = c(1,1,1))
model_111$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_011 = predict(model_001, n.ahead = 5)$pred
forecast_011

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_001=data.frame(actual=chtr_split_last.five, forecast = forecast_001, error=forecast_001 - chtr_split_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=chtr_split_last.five, forecast = forecast_101, error=forecast_101 - chtr_split_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=chtr_split_last.five, forecast = forecast_011, error=forecast_011 - chtr_split_last.five)
fore_011

#See the forecasting
fore_111=data.frame(actual=chtr_split_last.five, forecast = forecast_111, error=forecast_111 - chtr_split_last.five)
fore_111

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5 

#GARCH selection
summary(garch(model_111$residuals, order = c(1,1)))
summary(garch(model_111$residuals, order = c(1,2))) 
summary(garch(model_111$residuals, order = c(2,1))) 
summary(garch(model_111$residuals, order = c(2,2))) 

AIC(garch(model_111$residuals, order = c(1,1)))
AIC(garch(model_111$residuals, order = c(1,2))) 

m11 <- garch(model_111$residuals, order = c(2,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11)))

acf(na.omit(residuals(m11)^2)) 
shapiro.test(na.omit(residuals(m11))) 
jarque.bera.test(na.omit(residuals(m11)))

skewness(na.omit(residuals(m11)))
kurtosis(na.omit(residuals(m11)))
