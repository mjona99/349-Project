library(readxl)
library(tseries)
library(forecast)
library(TSA)

cvs <- as.data.frame(read_excel("Project.xlsx", sheet = 7))
cvs[1,8] <- 0
cvs[,8] <- as.numeric(cvs[,8])
cvs_neg.log.return <- cvs[,8]
cvs_last.five <- cvs_neg.log.return[(length(cvs_neg.log.return)-4):length(cvs_neg.log.return)]
cvs_without.last.five <- cvs_neg.log.return[1:(length(cvs_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(cvs_without.last.five) # significant at a handful of lags
eacf(cvs_without.last.five) # ARMA(0, 1)
#AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x o o x o o o o o x o  o  x  o 
# 1 x x o x o o o o o x o  o  x  o 
# 2 x o o x o o o o x x o  o  o  o 
# 3 x x x x o o o o o x o  o  x  o 
# 4 x x x o x o o o o x o  o  o  o 
# 5 x x x x o o o o o x o  o  o  o 
# 6 x x x x o x o o o x o  o  o  o 
# 7 x x x x x x x o o x o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(cvs_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(cvs_without.last.five, order = c(0,1,1))
model_011$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_011 = predict(model_011, n.ahead = 5)$pred
forecast_011

#See the forecasting
fore_001=data.frame(actual=cvs_last.five, forecast = forecast_001, error=forecast_001 - cvs_last.five)
fore_001

#See the forecasting
fore_011=data.frame(actual=cvs_last.five, forecast = forecast_011, error=forecast_011 - cvs_last.five)
fore_011

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(0,1) for the garch

#GARCH selection
summary(garch(model_011$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.7491 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_011$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.4193 
summary(garch(model_101$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_011$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_011$residuals, order = c(1,2))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # not significant at any lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal at all

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(cvs[,1])), cvs$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(cvs$Close),10))

spl = 2400

plot(c(1:spl), cvs$Close[c(1:spl)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(cvs$Close),10))


cvs_split = cvs_neg.log.return[c(1:spl)]
cvs_split_last.five <- cvs_split[(length(cvs_split)-4):length(cvs_split)]
cvs_split_without.last.five <- cvs_split[1:(length(cvs_split) - 5)]

acf(cvs_split_without.last.five) # a handful of lags are significant
eacf(cvs_split_without.last.five) # ARMA(0, 1) or ARMA(1, 1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x o o x x o o x o o o  o  o  o 
# 1 x o o o x o o x x o o  o  x  o 
# 2 x x o o x o o x o o o  o  o  o 
# 3 x x x o o o o x o o o  o  o  o 
# 4 x x x x x o o x x o o  o  o  o 
# 5 o o x x x o o o x o o  o  o  o 
# 6 x o x x x o x o o o o  o  o  o 
# 7 o o x x x x o o o o o  o  o  o 

#fit ARMA(0, 1) with d = 0 
model_001 = arima(cvs_split_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(0, 1) with d = 1 
model_011 = arima(cvs_split_without.last.five, order = c(0,1,1))
model_011$residuals

#fit ARMA(1, 1) with d = 0
model_101 = arima(cvs_split_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(1, 1) with d = 1
model_111 = arima(cvs_split_without.last.five, order = c(1,1,1))
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
forecast_111 = predict(model_101, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_001=data.frame(actual=cvs_split_last.five, forecast = forecast_001, error=forecast_001 - cvs_split_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=cvs_split_last.five, forecast = forecast_101, error=forecast_101 - cvs_split_last.five)
fore_101

#See the forecasting
fore_011=data.frame(actual=cvs_split_last.five, forecast = forecast_011, error=forecast_011 - cvs_split_last.five)
fore_011

#See the forecasting
fore_111=data.frame(actual=cvs_split_last.five, forecast = forecast_111, error=forecast_111 - cvs_split_last.five)
fore_111

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(1,1) for the garch

#MSFE
sum((fore_011$forecast - fore_011$actual)^2) / 5

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.8562 - smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_101$residuals, order = c(1,2))) #NaNs produced - so no go
summary(garch(model_101$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_101$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_101$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # not significant at any lags
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal at all


