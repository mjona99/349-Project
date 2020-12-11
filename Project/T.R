library(readxl)
library(tseries)
library(forecast)
library(TSA)

t <- as.data.frame(read_excel("Project.xlsx", sheet = 6))
t[1,8] <- 0
t[,8] <- as.numeric(t[,8])
t_neg.log.return <- t[,8]
t_last.five <- t_neg.log.return[(length(t_neg.log.return)-4):length(t_neg.log.return)]
t_without.last.five <- t_neg.log.return[1:(length(t_neg.log.return) - 5)]
#### PART ONE - LEAVE OUT LAST 5 ####

acf(t_without.last.five) #  significant at many lags
eacf(t_without.last.five) # ARMA(0, 2), ARMA (1,2)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 x x o o o x x o x o o  o  x  x 
# 1 x x o o o x x o x o o  o  x  o 
# 2 x x o o o x x x x o o  o  x  o 
# 3 x x o o o x x o x o o  o  o  x 
# 4 x x x x o x x o x o o  o  x  o 
# 5 x x x x o x x x o o o  o  x  o 
# 6 x x x x x x o x o o o  o  o  o 
# 7 x x x x x x x o o o o  o  o  o 

#fit ARMA(0, 2) with d = 0 
model_002 = arima(t_without.last.five, order = c(0,0,2))
model_002$residuals

#fit ARMA(1, 2) with d = 0 
model_102 = arima(t_without.last.five, order = c(1,0,2))
model_102$residuals

#fit ARMA(0, 2) with d = 1 
model_012 = arima(t_without.last.five, order = c(0,1,2))
model_012$residuals

#fit ARMA(1, 2) with d = 1 
model_112 = arima(t_without.last.five, order = c(1,1,2))
model_112$residuals

#predict the last five
forecast_002 = predict(model_002, n.ahead = 5)$pred
forecast_002

#predict the last five
forecast_102 = predict(model_102, n.ahead = 5)$pred
forecast_102

#predict the last five
forecast_012 = predict(model_012, n.ahead = 5)$pred
forecast_012

#predict the last five
forecast_112 = predict(model_112, n.ahead = 5)$pred
forecast_112

#See the forecasting
fore_002=data.frame(actual=t_last.five, forecast = forecast_002, error=forecast_002 - t_last.five)
fore_002

#See the forecasting
fore_102=data.frame(actual=t_last.five, forecast = forecast_102, error=forecast_102 - t_last.five)
fore_102

#See the forecasting
fore_012 =data.frame(actual=t_last.five, forecast = forecast_012, error=forecast_012 - t_last.five)
fore_012

#See the forecasting
fore_112 =data.frame(actual=t_last.five, forecast = forecast_112, error=forecast_112 - t_last.five)
fore_112

#MSFE
sum((fore_002$forecast - fore_002$actual)^2) / 5

#MSFE
sum((fore_102$forecast - fore_102$actual)^2) / 5 #smallest forecast error so we are going to use ARMA(1,2) for the garch

#MSFE
sum((fore_012$forecast - fore_012$actual)^2) / 5

#MSFE
sum((fore_112$forecast - fore_112$actual)^2) / 5


#GARCH selection
summary(garch(model_102$residuals, order = c(0,1))) #possible Box-Ljung test: p-value = 0.1235 - smallest p-value indicating that the model has the smallest significant lack of fit
summary(garch(model_102$residuals, order = c(1,1))) #possible Box-Ljung test: p-value =  0.8649
summary(garch(model_102$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_102$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_102$residuals, order = c(0,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at all but lag 1
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 

################################################################################################################################################################################
#### PART TWO - SPLIT ####
plot(c(1:length(t[,1])), t$Close, type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(t$Close),10))

plot(c(1000:3000), t$Close[c(1000:3000)], type = 'l',
     xlab = "Day", ylab = "Close Value", xaxp = c(0,length(t$Close),10))


t_split = t_neg.log.return[c(1000:3000)]
t_split_last.five <- t_split[(length(t_split)-4):length(t_split)]
t_split_without.last.five <- t_split[1:(length(t_split) - 5)]

acf(t_split_without.last.five) # no lags are significant
eacf(t_split_without.last.five) # weird output ARMA(1, 1)
# AR/MA
  # 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 o o o o o o o o o o o  o  o  o 
# 1 x o o o o o o o o o o  o  o  o 
# 2 x x o o o o o o o o o  o  o  o 
# 3 x x x o o o o o o o o  o  o  o 
# 4 x x x x o o o o o o o  o  o  o 
# 5 x x x x x o o o o o o  o  o  o 
# 6 x x x o x x o o o o o  o  o  o 
# 7 x x x x x o x o o o o  o  o  o 

#fit ARMA(1, 1) with d = 0 
model_101 = arima(t_split_without.last.five, order = c(1,0,1))
model_101$residuals

#fit ARMA(1, 1) with d = 1 
model_111 = arima(t_split_without.last.five, order = c(1,1,1))
model_111$residuals

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#predict the last five
forecast_111 = predict(model_111, n.ahead = 5)$pred
forecast_111

#See the forecasting
fore_101=data.frame(actual=t_split_last.five, forecast = forecast_101, error=forecast_101 - t_split_last.five)
fore_101

#See the forecasting
fore_111=data.frame(actual=t_split_last.five, forecast = forecast_111, error=forecast_111 - t_split_last.five)
fore_111

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 

#MSFE
sum((fore_111$forecast - fore_111$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(1,1) for the garch

#GARCH selection
summary(garch(model_111$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.548 2- smallest p-value indicating that the model has the smallest significant lack of fit 
summary(garch(model_111$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.8377 
summary(garch(model_111$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_111$residuals, order = c(2,2))) #singular information - so no go

m11 <- garch(model_111$residuals, order = c(1,1))
sum((m11$residuals)^2, na.rm=T) #sum of squared error of final model

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at lags 10, 17
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal

