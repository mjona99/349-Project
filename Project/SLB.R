library(readxl)
library(tseries)
library(forecast)
library(TSA)

slb <- as.data.frame(read_excel("Project.xlsx", sheet = 2))
slb[1,8] <- 0
slb[,8] <- as.numeric(slb[,8])
slb_neg.log.return <- slb[,8]
slb_last.five <- slb_neg.log.return[(length(slb_neg.log.return)-4):length(slb_neg.log.return)]
slb_without.last.five <- slb_neg.log.return[1:(length(slb_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(slb_without.last.five) # lag 1, 5, 7, 17, 22 significant
eacf(slb_without.last.five) # ARMA(0, 1), ARMA (1,1)
# AR/MA
#   0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x o o o x o x o o x o  o  o  o 
# 1 x o o o o o x o o o o  o  o  o 
# 2 x x o o o o x o o o o  o  o  o 
# 3 x x x o o o x x o o o  o  o  o 
# 4 x x x o o o x x o o x  o  o  o 
# 5 x x x x o o x x o o o  o  o  o 
# 6 x x x o x x o o o o o  o  o  o 
# 7 o x x x x x x x o o o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(slb_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(1, 1) with d = 0 
model_101 = arima(slb_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(slb_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(slb_without.last.five, order = c(1,1,1))
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
fore_001=data.frame(actual=slb_last.five, forecast = forecast_001, error=forecast_001 - slb_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=slb_last.five, forecast = forecast_101, error=forecast_101 - slb_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=slb_last.five, forecast = forecast_011, error=forecast_011 - slb_last.five)
fore_011

#See the forecasting
fore_111=data.frame(actual=slb_last.five, forecast = forecast_111, error=forecast_111 - slb_last.five)
fore_111

#MSFE
msfe_001 <- sum((fore_001$forecast - fore_001$actual)^2) / 5#smaller forecast error so we are going to use ARMA(0,1) for the garch

#MSFE
msfe_101 <- sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
msfe_011 <- sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
msfe_111 <- sum((fore_111$forecast - fore_111$actual)^2) / 5

min(msfe_001, msfe_011, msfe_101, msfe_111)

#GARCH selection
summary(garch(model_001$residuals, order = c(0,1))) #possible Box-Ljung test: p-value = 0.08714 - smallest p-value indicating that the model has the smallest significant lack of fit
summary(garch(model_001$residuals, order = c(1,1))) #possible Box-Ljung test: p-value =  0.6031 
summary(garch(model_001$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_001$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_001$residuals, order = c(0,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at lag 10, 29, 33
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(slb[,1])), slb$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(slb$Close),10))
spl = 1900

plot(c(1:spl), slb$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(slb$Close),10))


slb_split = slb_neg.log.return[c(1:spl)]
slb_split_last.five <- slb_split[(length(slb_split)-4):length(slb_split)]
slb_split_without.last.five <- slb_split[1:(length(slb_split) - 5)]

acf(slb_split_without.last.five) # lag 1, 5, 6, 11 significant
eacf(slb_split_without.last.five) # ARMA(0, 1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x o o o x x o o o o x  o  o  o 
# 1 x x o o o x o o o o x  o  o  o 
# 2 x x o o o x o o o o x  o  o  o 
# 3 x o x o o o o o o o x  o  o  o 
# 4 x x x x o o o o o o x  o  o  o 
# 5 x x x x o o o o x o o  o  o  o 
# 6 x x x x o x o o x o o  o  o  o 
# 7 x o x x x x x o o o o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(slb_split_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(slb_split_without.last.five, order = c(0,1,1))
model_011$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_011 = predict(model_011, n.ahead = 5)$pred
forecast_011

#See the forecasting
fore_001=data.frame(actual=slb_split_last.five, forecast = forecast_001, error=forecast_001 - slb_split_last.five)
fore_001

#See the forecasting
fore_011=data.frame(actual=slb_split_last.five, forecast = forecast_011, error=forecast_011 - slb_split_last.five)
fore_011

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(0,1) for the garch

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5 

#GARCH selection
summary(garch(model_001$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.6031
summary(garch(model_001$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.4342 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_001$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_001$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_001$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # no lags are significant
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal, but not as bad as previous ones

