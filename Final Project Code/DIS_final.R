library(readxl)
library(tseries)
library(forecast)
library(TSA)

dis <- as.data.frame(read_excel("Project.xlsx", sheet = 4))
dis[1,8] <- 0
dis[,8] <- as.numeric(dis[,8])
dis_neg.log.return <- dis[,8]
dis_last.five <- dis_neg.log.return[(length(dis_neg.log.return)-4):length(dis_neg.log.return)]
dis_without.last.five <- dis_neg.log.return[1:(length(dis_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(dis_without.last.five)
eacf(dis_without.last.five) 

#fit ARMA(1, 2) with d = 0 
model_102 = arima(dis_without.last.five, order = c(1,0,2))
model_102$residuals

#fit ARMA(2, 2) with d = 0 
model_202 = arima(dis_without.last.five, order = c(2,0,2))
model_202$residuals

#fit ARMA(1, 2) with d = 1 
model_112 = arima(dis_without.last.five, order = c(1,1,2))
model_112$residuals

#fit ARMA(2, 2) with d = 1 
model_212 = arima(dis_without.last.five, order = c(2,1,2))
model_212$residuals

#predict the last five
forecast_102 = predict(model_102, n.ahead = 5)$pred
forecast_102

#predict the last five
forecast_202 = predict(model_202, n.ahead = 5)$pred
forecast_202

#predict the last five
forecast_112 = predict(model_112, n.ahead = 5)$pred
forecast_112

#predict the last five
forecast_212 = predict(model_212, n.ahead = 5)$pred
forecast_212

#See the forecasting
fore_102=data.frame(actual=dis_last.five, forecast = forecast_102, error=forecast_102 - dis_last.five)
fore_102

#See the forecasting
fore_202=data.frame(actual=dis_last.five, forecast = forecast_202, error=forecast_202 - dis_last.five)
fore_202

#See the forecasting
fore_112=data.frame(actual=dis_last.five, forecast = forecast_112, error=forecast_112 - dis_last.five)
fore_112

#See the forecasting
fore_212=data.frame(actual=dis_last.five, forecast = forecast_212, error=forecast_212 - dis_last.five)
fore_212

#MSFE
sum((fore_102$forecast - fore_102$actual)^2) / 5 

#MSFE
sum((fore_202$forecast - fore_202$actual)^2) / 5 

#MSFE
sum((fore_112$forecast - fore_112$actual)^2) / 5 

#MSFE
sum((fore_212$forecast - fore_212$actual)^2) / 5

#GARCH selection
summary(garch(model_102$residuals, order = c(1,1))) 
summary(garch(model_102$residuals, order = c(1,2)))
summary(garch(model_102$residuals, order = c(2,1))) 
summary(garch(model_102$residuals, order = c(2,2))) 

m11 <- garch(model_102$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

plot(c(1:length(dis[,1])), dis$`Neg. log return`, type = 'l',  xlab = "Day", ylab = "Neg. log return", xaxp = c(0,length(dis$`Neg. log return`),10))
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
plot(c(1:length(dis[,1])), dis$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(dis$Close),10))

spl = 3200

plot(c(1:spl), dis$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(dis$Close),10))


dis_split = dis_neg.log.return[c(1:spl)]
dis_split_last.five <- dis_split[(length(dis_split)-4):length(dis_split)]
dis_split_without.last.five <- dis_split[1:(length(dis_split) - 5)]

acf(dis_split_without.last.five) 
eacf(dis_split_without.last.five) 

#fit ARMA(1, 2) with d = 0 
model_102 = arima(dis_split_without.last.five, order = c(1,0,2))
model_102$residuals

#fit ARMA(1, 2) with d = 1 
model_112 = arima(dis_split_without.last.five, order = c(1,1,2))
model_112$residuals

#fit ARMA(1, 3) with d = 0
model_103 = arima(dis_split_without.last.five, order = c(1,0,3))
model_103$residuals

#fit ARMA(1, 3) with d = 1
model_113 = arima(dis_split_without.last.five, order = c(1,1,3))
model_113$residuals

#predict the last five
forecast_102 = predict(model_102, n.ahead = 5)$pred
forecast_102

#predict the last five
forecast_112 = predict(model_112, n.ahead = 5)$pred
forecast_112

#predict the last five
forecast_103 = predict(model_103, n.ahead = 5)$pred
forecast_103

#predict the last five
forecast_113 = predict(model_113, n.ahead = 5)$pred
forecast_113

#See the forecasting
fore_102=data.frame(actual=dis_split_last.five, forecast = forecast_102, error=forecast_102 - dis_split_last.five)
fore_102

#See the forecasting
fore_112=data.frame(actual=dis_split_last.five, forecast = forecast_112, error=forecast_112 - dis_split_last.five)
fore_112

#See the forecasting
fore_103=data.frame(actual=dis_split_last.five, forecast = forecast_103, error=forecast_103 - dis_split_last.five)
fore_103

#See the forecasting
fore_113=data.frame(actual=dis_split_last.five, forecast = forecast_113, error=forecast_113 - dis_split_last.five)
fore_113

#MSFE
sum((fore_102$forecast - fore_102$actual)^2) / 5

#MSFE
sum((fore_112$forecast - fore_112$actual)^2) / 5 

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
