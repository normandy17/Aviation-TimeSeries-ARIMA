##Time Series

library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(readxl)

setwd("C:/ML/aviation")
data <- read.csv("aviation.csv", header = TRUE)

dt_ts <- ts(data$Sales, frequency = 4, start=c(86))
View(dt_ts)
plot(dt_ts)

train <- dt_ts[1:38]
test <- dt_ts[39:42]

train <- ts(train,frequency = 4)
test <- ts(test,frequency = 4)

### level Alpha
### trend Beta
### seasonality Gamma

##holtwinters
#only level, alpha
hw1 <- HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
hw1
hw1_pred <- data.frame(predict(hw1, n.ahead=4))
plot(forecast(hw1,h=4))
hw1_mape <- MAPE(hw1_pred$fit,test)*100

##holtwinters
#level and trend, alpha & beta
hw2 <- HoltWinters(train, alpha = 0.2, beta = .1, gamma = F)
hw2
hw2_pred <- data.frame(predict(hw2, n.ahead=4))
plot(forecast(hw2,h=4))
hw2_mape <- MAPE(hw2_pred$fit,test)*100

##holtwinters
#level, seasonality and trend, alpha, gamma & beta
hw3 <- HoltWinters(train, alpha = 0.2, beta = .15, gamma = .15)
hw3
hw3_pred <- data.frame(predict(hw3, n.ahead=4))
plot(forecast(hw3,h=4))
hw3_mape <- MAPE(hw3_pred$fit,test)*100

##holtwinters
#level, NO alpha input, gamma & beta =F , optimum alpha is chosen by system
hw4 <- HoltWinters(train, beta = F, gamma = F)
hw4
hw4_pred <- data.frame(predict(hw4, n.ahead=4))
hw4_pred
plot(forecast(hw4,h=4))
hw4_mape <- MAPE(hw4_pred$fit,test)*100

##holtwinters
#level and seasonality, NO alpha & NO beta input, gamma=F , optimum alpha and Beta is chosen by system
hw5 <- HoltWinters(train, gamma = F)
hw5
hw5_pred <- data.frame(predict(hw5, n.ahead=4))
hw5_pred
plot(forecast(hw5,h=4))
hw5_mape <- MAPE(hw5_pred$fit,test)*100

##holtwinters
#level and seasonality, NO alpha, beta or gamma input, optimum alpha and Beta and gamma is chosen by system
hw6 <- HoltWinters(train)
hw6
hw6_pred <- data.frame(predict(hw6, n.ahead=4))
hw6_pred
plot(forecast(hw6,h=4))
hw6_mape <- MAPE(hw6_pred$fit,test)*100

df_mape <- data.frame(c("hw1_mape","hw2_mape","hw3_mape","hw4_mape","hw5_mape","hw6_mape"),c(hw1_mape,hw2_mape,hw3_mape,hw4_mape,hw5_mape,hw6_mape))
colnames(df_mape) <- c("MAPE","Values")

newmodel <- HoltWinters(dt_ts)
plot(forecast(newmodel,n.ahead=4))
forecast_new <- data.frame(predict(newmodel,n.ahead=4))



##########ARIMA
plot(train)
acf(train)
pacf(train)  
arima <-arima(train, order = c(3,1,0), method = "ML")

## Automatic Arima
model_AA <-auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))

acf(model_AA$residuals)
plot(forecast(model_AA, h=12),xaxt="n")
