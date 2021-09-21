install.packages("seastests")
library(forecast)
library(readxl)
library(dplyr)
library(seastests)

setwd('C://Users//geeti//Downloads//')
getwd()
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
  summarise(Sales.Per.Month = sum(Sales))%>%subset(Category == 'Office Supplies')


head(Sales)

#plot(decompose(salests))

####check for seasonality
#summary(wo(salests))
#isSeasonal(salests)

#salest<-data.frame(salesdata$OrderDate,salesdata$Sales)
####salests<-ts(salest[,-1],frequency =24,start=c(2014, 01),end=c(2018, 01))
salests<-ts(Sales$Sales.Per.Month,frequency =12,start=c(2014, 01),end=c(2018, 01))
head(salests)

plot(salests,xlab="Year",ylab="Sales (USD)",main="Superstore Sales,Jan2014-December2017")
plot(decompose(salests))

summary(wo(salests))
isSeasonal(salests)

seasonplot(salests,ylab="Sales (USD)",col=c(1,2,3,4),main="Seasonalplot:Superstore Sales,Jan2014-December2017",year.labels=TRUE)
Acf(salests)
Pacf(salests)
cycle(salests)
boxplot(salests~cycle(salests))


####Data partition

nvalid <- 12
ntrain <- length(salests) - nvalid
salests_train <- window(salests,start=c(2014, 01),end=c(2014,ntrain))
salests_valid <- window(salests,start=c(2014,ntrain+1),end=c(2014,ntrain+nvalid))


# plot the series and its ACF, demonstrating trend and seasonality, indicating 
# starting with d=0 and D=1
plot(salests_train, xlab = "Time", ylab = "Sales",bty = "l")

salests_train %>% Acf()
salests_train %>% diff() %>% Acf()
salests_train %>% diff(lag=12) %>% Acf()


####Pacf for p,P=(0,0) (0,1)
salests_train %>% Pacf()
salests_train %>% diff() %>% Pacf() 
salests_train %>% diff(lag=12) %>% Pacf()

####plot ARIMA


arima_model1 <- Arima(salests_train,order=c(1,1,1),seasonal = c(0,1,0))

checkresiduals(arima_model1)
accuracy(arima_model1)


auto_arima_model <- auto.arima(salests_train, stepwise = FALSE,lambda=BoxCox.lambda(salests_train))
accuracy(auto_arima_model)
auto_arima_model

checkresiduals(auto_arima_model)


forecast_data1 <- forecast(arima_model1,h=12)

forecast_data_auto <- forecast(auto_arima_model,h=12)

accuracy(forecast_data1,salests_valid)

accuracy(forecast_data_auto,salests_valid)


####defining hw here
####adjust for Box Cox transformation
#salests_hw <- hw(salests_train,lambda="auto", h=12)

salests_hw <- hw(salests_train, h=12)
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

#forecast_data_hw

plot(forecast_data_hw)
lines(salests_valid,col="green")


