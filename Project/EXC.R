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

acf(exc_without.last.five) # significant at many lags
eacf(exc_without.last.five) # ARMA(1, 1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x x o x o x o x o o o  x  o  o 
# 1 x o o x o o o x o o o  o  o  o 
# 2 x o x x o o o x o o o  o  o  o 
# 3 x x x x o o o o o o o  o  o  o 
# 4 x x x x o o o o o o o  o  o  o 
# 5 x x x x x o o o o o o  o  o  o 
# 6 x x x x o x o o o o o  o  o  o 
# 7 x x x x x x o o o o o  o  o  o 

#fit ARMA(1, 1) with d = 0 
model_101 = arima(exc_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(exc_without.last.five, order = c(1,1,1))
model_111$residuals

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_101=data.frame(actual=exc_last.five, forecast = forecast_101, error=forecast_101 - exc_last.five)
fore_101

#See the forecasting
fore_111=data.frame(actual=exc_last.five, forecast = forecast_111, error=forecast_111 - exc_last.five)
fore_111

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(1,1) for the garch

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.5092 - all coeffs are significant and p-value diff isnt big. going with 1,1
summary(garch(model_101$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.4962 a2 isnt significant.
summary(garch(model_101$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_101$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_101$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # not significant at any lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(exc[,1])), exc$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(exc$Close),10))

plot(c(1000:3500), exc$Close[c(1000:3500)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(exc$Close),10))


exc_split = exc_neg.log.return[c(1000:3500)]
exc_split_last.five <- exc_split[(length(exc_split)-4):length(exc_split)]
exc_split_without.last.five <- exc_split[1:(length(exc_split) - 5)]

acf(exc_split_without.last.five) # not significant at any lags
eacf(exc_split_without.last.five) # weird output,ARMA(0,0)?
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 o o o o o o o o o o o  o  o  o 
# 1 x o o o o o o o o o o  o  o  o 
# 2 x x o o o o o o o o o  o  o  o 
# 3 x x x o o o o o o o o  o  o  o 
# 4 x x x o o o o o o o o  o  o  o 
# 5 x x x o x o o o o o o  o  o  x 
# 6 x x o x x x o o o o o  o  o  o 
# 7 x x x x x x o o o o o  o  o  o 

#fit ARMA(0, 0) with d = 0 
model_000 = arima(exc_split_without.last.five, order = c(0,0,0))
model_000$residuals

#fit ARMA(0, 0) with d = 1 
model_010 = arima(exc_split_without.last.five, order = c(0,1,0))
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
sum((fore_010$forecast - fore_010$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(0,0) for the garch

#GARCH selection
summary(garch(model_010$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.1398
summary(garch(model_010$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 5.388e-11  - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_010$residuals, order = c(2,1))) #possible Box-Ljung test: p-value = 0.08637
summary(garch(model_010$residuals, order = c(2,2))) ##singular information - so no go

m11 <- garch(model_010$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value = 3.969e-07 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at many lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # looks kinda normal

