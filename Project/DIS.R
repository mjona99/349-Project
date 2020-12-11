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

acf(dis_without.last.five) # significant at many lags
eacf(dis_without.last.five) # ARMA(1, 2) or ARMA(2, 2)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x x x o x o x o o o o  o  o  o 
# 1 x x o o o o o o o o o  o  o  o 
# 2 x x o o o x x o o o o  o  o  o 
# 3 x x x x o o o o o o o  o  o  o 
# 4 x x x x o o x o o o o  o  o  o 
# 5 x x x x x x x o o o o  o  o  o 
# 6 x x x x x x o o o o o  o  o  o 
# 7 x x o x o x x x o o o  o  o  o 

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
sum((fore_112$forecast - fore_112$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(1, 2) for the garch

#MSFE
sum((fore_212$forecast - fore_212$actual)^2) / 5 

#GARCH selection
summary(garch(model_112$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.756 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_112$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.9095
summary(garch(model_112$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_112$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_112$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at no lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 

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

acf(dis_split_without.last.five) # many lags are significant
eacf(dis_split_without.last.five) # ARMA(1, 2), ARMA (1,3)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x x x o x o o o o o o  o  o  x 
# 1 x x o o x o o o o o o  o  o  o 
# 2 x x o x x o o o o o o  o  o  o 
# 3 x x x o x o o o o o o  o  o  o 
# 4 x x x x x o o o o o o  o  o  o 
# 5 x x x x x o o o o o o  o  o  o 
# 6 x x x x o x o o o o o  o  o  o 
# 7 x o x x x x x o o o o  o  o  o 

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
sum((fore_103$forecast - fore_103$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(1,3) for the garch

#MSFE
sum((fore_113$forecast - fore_113$actual)^2) / 5

#GARCH selection
summary(garch(model_103$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.7756 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_103$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.9328
summary(garch(model_103$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_103$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_103$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at one lag 
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 


