library(forecast)
library(readxl)
salesdata <- read_excel("C:/Users/User/Downloads/SalesData.xlsx", sheet=1, col_names = TRUE)
View(salesdata)


salest<-data.frame(salesdata$OrderDate,salesdata$Sales)
salests<-ts(salest[,-1],frequency =24,start=c(2014, 01),end=c(2018, 01))
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