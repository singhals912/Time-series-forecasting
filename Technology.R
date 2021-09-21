library(forecast)
library(readxl)
library(dplyr)
library(tseries)
setwd('/Users/mayankjha/Downloads')
salesdata <- read_excel("SalesData.xlsx", sheet=1, col_names = TRUE)
View(salesdata)

salesdata$Order.Date.Month<-format(salesdata$OrderDate,"%Y-%m")

df_cat <- salesdata %>%
  select('Category','Sales','Order.Date.Month')

summary(df_cat)
df_cat$Sales <- as.numeric((df_cat$Sales))

str(df_cat)

Sales<-df_cat%>%
  group_by(Order.Date.Month, Category)%>%
  summarise(Sales.Per.Month = sum(Sales))%>%subset(Category == 'Technology')


head(Sales)
salests<-ts(Sales$Sales.Per.Month,frequency =12,start=c(2014, 01),end=c(2018, 01))


plot(decompose(salests))

adf.test(salests)

#salest<-data.frame(salesdata$OrderDate,salesdata$Sales)
####salests<-ts(salest[,-1],frequency =24,start=c(2014, 01),end=c(2018, 01))
head(salests)
ets(salests)

nvalid <- 12
ntrain <- length(salests) - nvalid
salests_train <- window(salests,start=c(2014, 01),end=c(2014,ntrain))
salests_valid <- window(salests,start=c(2014,ntrain+1),end=c(2014,ntrain+nvalid))


hwin_Additive <- ets(salests_train, model = "ZNA")
hwin_Multiplicative<- ets(salests_train, model = "ZNM")
hwin <- hw(salests_train, lambda = "auto", h=12)
acf(hwin$fitted)
pacf(hwin$fitted)
acf(hwin$residuals)
hwin_2 <- hw(salests_train,seasonal = "multiplicative", h=12)
hwin_2 <- hw(salests_train,seasonal = "additive", h=12)


ets(salests_train)
fit <- ets(salests_train)
autoplot(fit)


BoxCox.lambda(salests_train)
salests<- salests^-0.57
autoplot(salests)

plot(salests,xlab="Year",ylab="Sales (USD)",main="Superstore Sales,Jan2014-December2017")
plot(decompose(salests))

seasonplot(salests,ylab="Sales (USD)",col=c(1,2,3,4),main="Seasonalplot:Superstore Sales,Jan2014-December2017",year.labels=TRUE)
Acf(salests)
Pacf(salests)
cycle(salests)
boxplot(salests~cycle(salests))

library(zoo)
accuracy(arima(salests, order = c(0,0,1)))
accuracy(ma(salests))

annual_sales = aggregate(salests)
plot.ts(annual_sales, col = "blue", main = "Yearly Beer sales time series data between 2014 and 2017")

decompose_salesdata <- decompose(salests,type = "additive")
seasonal_sales <- as.ts(decompose_salesdata$seasonal)
trend_sales <- as.ts(decompose_salesdata$trend)
random_sales <- as.ts(decompose_salesdata$random)
plot.ts(seasonal_sales, main = "Seasonal Component")
plot.ts(trend_sales, main = "Trend Component")
plot.ts(random_sales, main = "Randon Component")


plot (decompose(salests, type="multiplicat"))

salests_seasonallyadjusted <- salests - seasonal_sales
plot.ts(salests_seasonallyadjusted, main = "Seasonal Adjusted Plot of sales")

####modified
####salests_hw <- hw(salests,seasonal="additive", h=24)
####salests_hw$model

####plot(salests_hw$model)
####plot(salests_hw)
####plot(salests)

####accuracy(salests_hw)

####plot(salests_hw$residuals)
####abline(0, 0)

####Acf(salests_hw$residuals)

####Acf(salests_hw$fitted)

####Data partition

####data partition
####Validation data set has been chosen to be 6 months because the last 1 year shows
####relatively very less variation in the sales. If sales for 1 year are taken to validate
####the model, difference in the forecast and actual values is very large owing to
####the large variation in the training data of the previous months.

nvalid <- 12
ntrain <- length(salests) - nvalid
salests_train <- window(salests,start=c(2014, 01),end=c(2014,ntrain))
salests_valid <- window(salests,start=c(2014,ntrain+1),end=c(2014,ntrain+nvalid))
salests_valid

# plot the series and its ACF, demonstrating trend and seasonality, indicating 
# starting with d=0 and D=1
plot(salests_train, xlab = "Time", ylab = "Sales",bty = "l")

salests_train %>% Acf()
salests_train %>% diff() %>% Acf()
salests_train %>% diff(lag=12) %>% Acf()
salests_train %>% diff(lag=12) %>% diff() %>% Acf()

####Pacf for p,P=(0,0)
salests_train %>% Pacf()
salests_train %>% diff() %>% Pacf() 
salests_train %>% diff(lag=12) %>% Pacf()
#salests_train %>% diff(lag=12) %>% diff() %>% Pacf()

####Acf for q,Q=(0,0)

####plot ARIMA
arima_model <- arima(salests_train,order=c(0,0,0),seasonal = c(0,1,1))
arima_model_transform <- arima(salests_train,order=c(0,0,0),seasonal = c(0,1,1),transform.pars = TRUE)

checkresiduals(arima_model)


accuracy(arima_model)
BoxCox.lambda(salests_train)
auto_arima_model <- auto.arima(salests_train, lambda = 1.965281, stepwise = FALSE)
accuracy(auto_arima_model)

checkresiduals(auto_arima_model)
auto.arima(salests_train)

arima_model

forecast_data <- forecast(arima_model,h=12)
forecast_data_auto <- forecast(auto_arima_model,h=12)

accuracy(forecast_data,salests_valid)
accuracy(forecast_data_auto,salests_valid)

plot(forecast_data)
lines(salests_valid,col="green")

####defining hw here

salests_hw <- hw(salests_train,seasonal="additive", h=12)
salests_hw$model

plot(salests_hw$model)
plot(salests_hw)
plot(salests)

accuracy(salests_hw)

plot(salests_hw$residuals)
abline(0, 0)

Acf(salests_hw$residuals)

forecast_data_hw <- forecast(salests_hw,h=12)
accuracy(forecast_data_hw,salests_valid)

plot(forecast_data_hw)
lines(salests_valid,col="green")

####Naive forecast
naive_model <- naive(salests_train,h=12)
plot(naive_model)
lines(salests_valid,col="green")


plot(forecast_data,xlab="Year",ylab="Sales(Technology)",bty="l",xaxt="n",yaxt="n",
     main="Forecasts from ARIMA(0,0,0)(0,1,1)")
legend("topleft",c("validation data","forecast data"),fill=c("green","blue"))
axis(1,at=seq(2014,2018,1),labels=format(seq(2014,2018,1)),cex.axis=0.8)
axis(2,at=seq(0,60000,10000),labels=format(seq(0,60000,10000)), las=1,cex.axis=0.8)
lines(salests_valid,col="green")