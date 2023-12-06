#begin
suppressPackageStartupMessages(library(IntroCompFinR))#The package is for the introduction to computational finance in R
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(methods))
suppressMessages(library(forecast))
suppressMessages(library(tseries))
suppressMessages(library(quantmod))
suppressMessages(library(caret))
suppressMessages(library(nnet))
library(PerformanceAnalytics)
library(tseries)
library(forecast)
data(msftDailyPrices, sbuxDailyPrices)

msft_data<-ts(msftDailyPrices,start="2004",end="2014",frequency = 365)
sbux_data<-ts(sbuxDailyPrices,start = "2004", end= "2014", frequency = 365)

# Stationarity Checking
#plot the non-modified data
plot(msft_data,main="MSFT DATA PLOT")
plot(sbux_data,main="SBUX DATA PLOT")
#adf test
adf.test(msft_data)
adf.test(sbux_data)

#difference
modified_msft_data<- diff(msft_data)
modified_sbux_data<- diff(sbux_data)
#plot the modified data
plot(modified_msft_data,main="modified msft data")
plot(modified_sbux_data,main="modified sbux data")
#adf test of modified data
adf.test(modified_msft_data)# p-value <0.05, series is stationary.
adf.test(modified_sbux_data)#p value is < 0.05 , series is stationary.
#acf and pacf of modified data
acf(modified_msft_data,lag.max = 10,main= "acf of modified_msft_data")
acf(modified_sbux_data,lag.max = 10,main="acf of modified_sbux_data")
pacf(modified_msft_data,lag.max=10,main = "pacf of modified_msft_data")
pacf(modified_sbux_data,lag.max = 10,main="pacf of modified_sbux_data")
# Delimit training range
msft_data.train <- window(msft_data, end = c(2012, 12))
sbux_data.train <- window(sbux_data, end = c(2012, 12))
# Delimit testing range
msft_data.test <- window(msft_data,start = c(2012, 12), end = c(2014, 12))
sbux_data.test <- window(sbux_data,start = c(2012, 12), end = c(2014, 12))
# Training and testing ranges chart
plot(msft_data,main="Train and test range of msft Daily Prices 2004-2014",ylab="Price",xlab="Days")
lines(msft_data.train,col="blue")
lines(msft_data.test,col="green")
legend("topleft",col=c("blue","green"),lty=1,legend=c("Training","Testing"))
plot(sbux_data,main="Train and test range of sbux Daily Prices 2004-2014",ylab="Price",xlab="Days")
lines(sbux_data.train,col="blue")
lines(sbux_data.test,col="green")
legend("topleft",col=c("blue","green"),lty=1,legend=c("Training","Testing"))

#to check the order of MA and AR for ARMA
auto.arima(modified_msft_data)
auto.arima(modified_sbux_data)
# ARIMA Fitting
model_msft<-Arima(modified_msft_data,order=c(0,0,0))
print(summary(model_msft))
plot(model_msft)
model_sbux<-Arima(modified_sbux_data,order=c(2,0,0))
print(summary(model_sbux))
plot(model_sbux)

#display residuals
tsdisplay(residuals(model_msft), lag.max=5, main='(0,0,0) Model Residuals')
tsdisplay(residuals(model_sbux), lag.max=5, main='(2,0,0) Model Residuals')
#checking residual
checkresiduals(model_msft)
checkresiduals(model_sbux)

#holtwinters
model1 = HoltWinters(modified_msft_data, gamma = FALSE)
pred1 = predict(model1, n.ahead = 365)
plot(model1)
f1<-forecast(model1, level=c(95), h=365)
plot(f1,main="Forecasted Value of 365 steps ahead msft price")
model2 = HoltWinters(modified_sbux_data, gamma = FALSE)
pred2 = predict(model2, n.ahead = 365)
plot(model2)
f2<-forecast(model2, level=c(95), h=365)
plot(f2,main="Forecasted Value of 365 steps ahead sbux price")
##Forecast using ETS and accuracy test
fit1 <- ets(msft_data.train)
fit2 <- ets(sbux_data.train)
forecast1 <- forecast(fit1, h = length(msft_data.test))
forecast2 <- forecast(fit2, h = length(sbux_data.test))
fit1 %>% forecast(h=365) %>%
  autoplot() +
  ylab("ETS forecasting of MSFT")
fit2 %>% forecast(h=365) %>%
  autoplot() +
  ylab("ETS forecasting ofSBUX")

# Calculate the accuracy of the forecast
accuracy(forecast1, msft_data.test)
accuracy(forecast2, sbux_data.test)


