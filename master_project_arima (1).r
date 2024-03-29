library(forecast)
library(readxl)
library(dplyr)

salesdata <- read_excel("C://Users//geeti//Downloads//SalesData.xlsx", sheet=1, col_names = TRUE)
View(salesdata)


salest<-data.frame(salesdata$OrderDate,salesdata$Sales)
####salests<-ts(salest[,-1],frequency =24,start=c(2014, 01),end=c(2018, 01))
salests<-ts(salest[,-1],frequency =12,start=c(2014, 01),end=c(2018, 01))
head(salests)

plot(salests,xlab="Year",ylab="Sales (USD)",main="Superstore Sales,Jan2014-December2017")

seasonplot(salests,ylab="Sales (USD)",col=c(1,2,3,4),main="Seasonalplot:Superstore Sales,Jan2014-December2017",year.labels=TRUE)
Acf(salests)
cycle(salests)
boxplot(salests~cycle(salests))

annual_sales = aggregate(salests)
plot.ts(annual_sales, col = "blue", main = "Yearly Beer sales time series data between 2014 and 2017")

decompose_salesdata <- decompose(salests,type = "additive")
seasonal_sales <- as.ts(decompose_salesdata$seasonal)
trend_sales <- as.ts(decompose_salesdata$trend)
random_sales <- as.ts(decompose_salesdata$random)
plot.ts(seasonal_sales, main = "Seasonal Component")
plot.ts(trend_sales, main = "Trend Component")
plot.ts(random_sales, main = "Randon Component")


plot (decompose(salests, type="additive"))

salests_seasonallyadjusted <- salests - seasonal_sales
plot.ts(salests_seasonallyadjusted, main = "Seasonal Adjusted Plot of sales")

salests_hw <- hw(salests,seasonal="additive", h=24)
salests_hw$model

plot(salests_hw$model)
plot(salests_hw)
plot(salests)

accuracy(salests_hw)

plot(salests_hw$residuals)
abline(0, 0)

Acf(salests_hw$residuals)

####Acf(salests_hw$fitted)

####Data partition

####data partition
####Validation data set has been chosen to be 6 months because the last 1 year shows
####relatively very less variation in the sales. If sales for 1 year are taken to validate
####the model, difference in the forecast and actual values is very large owing to
####the large variation in the training data of the previous months.

nvalid <- 6
ntrain <- length(salests) - nvalid
salests_train <- window(salests,start=c(2014, 01),end=c(2014,ntrain))
salests_valid <- window(salests,start=c(2014,ntrain+1),end=c(2014,ntrain+nvalid))


# plot the series and its ACF, demonstrating trend and seasonality, indicating 
# starting with d=0 and D=1
plot(salests_train, xlab = "Time", ylab = "Sales",bty = "l")

#salests_train %>% diff() %>% Acf()
salests_train %>% diff(lag=12) %>% Acf()
#salests_train %>% diff(lag=12) %>% diff() %>% Acf()

####Pacf for p,P=(0,0)

#salests_train %>% diff() %>% Pacf() 
salests_train %>% diff(lag=12) %>% Pacf()
#salests_train %>% diff(lag=12) %>% diff() %>% Pacf()

####Acf for q,Q=(0,0)

####plot ARIMA
arima_model <- arima(salests_train,order=c(0,0,0),seasonal = c(0,1,1))

accuracy(arima_model)

auto_arima_model <- auto.arima(salests_train)
accuracy(auto.arima(salests_train))

auto.arima(salests_train)

arima_model

forecast_data <- forecast(arima_model,h=6)
forecast_data_auto <- forecast(auto_arima_model,h=6)

accuracy(forecast_data,salests_valid)
accuracy(forecast_data_auto,salests_valid)

plot(forecast_data)
lines(salests_valid,col="green")


