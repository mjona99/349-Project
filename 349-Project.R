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

acf(chtr_without.last.five) # lag 6 significant
eacf(chtr_without.last.five) # weird output, maybe ARMA(0, 1) or ARMA(1, 1)
# AR/MA
#   0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 o o o o o x o o o o o  o  o  o 
# 1 x o x o o x o o o o o  o  o  o 
# 2 x x o o o x o o o o o  o  o  o 
# 3 x x x o o x x o o o o  o  o  o 
# 4 x x x x o x o o o o o  o  o  o 
# 5 x o x x x x o o o o o  o  o  o 
# 6 x x x x x x o o o o o  o  o  o 
# 7 x x x o x x x o o o o  o  o  o 

#fit ARMA(0, 1)
model_001 = arima(chtr_without.last.five, order = c(0,0,1))
model_001$residuals

#fit ARMA(1, 1)
model_101 = arima(chtr_without.last.five, order = c(1,0,1))
model_101$residuals

#predict the last five
forecast_001 = predict(model_001, n.ahead = 5)$pred
forecast_001

#predict the last five
forecast_101 = predict(model_101, n.ahead = 5)$pred
forecast_101

#See the forecasting
fore_001=data.frame(actual=chtr_last.five, forecast = forecast_001, error=forecast_001 - chtr_last.five)
fore_001

#See the forecasting
fore_101=data.frame(actual=chtr_last.five, forecast = forecast_101, error=forecast_101 - chtr_last.five)
fore_101

#MSFE
sum((fore_001$forecast - fore_001$actual)^2) / 5

#MSFE
sum((fore_101$forecast - fore_101$actual)^2) / 5 #smaller forecast error so we are going to use ARMA(1,1) for the garch

#GARCH selection
summary(garch(model_101$residuals, order = c(1,1))) #possible Box-Ljung test: p-value = 0.3017 - smallest p-value indicating that the model has the smallest significant lack of fit 
#also all coeffs are significant
summary(garch(model_101$residuals, order = c(1,2))) #possible Box-Ljung test: p-value = 0.886 
#summary(garch(model_101$residuals, order = c(2,1))) #singular information - so no go
summary(garch(model_101$residuals, order = c(2,2))) #possible Box-Ljung test: p-value = 0.8842

m11 <- garch(model_101$residuals, order = c(1,1))

#can only do a max of 5000 samples
shapiro.test(na.omit(residuals(m11))) # p-value < 2.2e-16 - very very not normal

acf(na.omit(residuals(m11)^2)) # significant at lag 28
qqnorm(na.omit(residuals(m11))) 
qqline(na.omit(residuals(m11))) # does not look normal 



# final garch model SSE
#predict the last five
forecast_garch = predict(m11, n.ahead = 5)$pred
forecast_garch

#See the forecasting
fore_garch=data.frame(actual=chtr_last.five, forecast = forecast_garch, error=forecast_garch - chtr_last.five)
fore_garch

#MSE
sum((fore_garch$forecast - fore_garch$actual)^2) / 5



