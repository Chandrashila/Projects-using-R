library(readxl)
WallaceRealtorsFull <- read_excel("C:/Users/rumic/Desktop/Desktop/MSBAPM_First_Semester/OPIM 5603_Manuel_Statistics in business Analytics/Assignment 3/WallaceRealtorsFull.xlsx",
col_types = c("numeric", "numeric", "numeric",
"text", "text"))
View(WallaceRealtorsFull)
#Calculating Mean , Median , Range , SD , Coefficient OF variation , Kurtosis and Skewness for asking and selling prices
#Asking Prices
CV = function(x){(sd(x)/mean(x))*100}
ask_vector = as.vector(as.matrix(WallaceRealtorsFull[,2]))
ask_vector
ask_mean = mean(ask_vector)
ask_mean
ask_median = median(ask_vector)
ask_median
ask_range = max(ask_vector) - min(ask_vector)
ask_range
ask_sd = sd(ask_vector)
ask_sd
ask_cv = CV(ask_vector)
ask_cv
install.packages("moments")
library("moments")
ask_kurtosis = kurtosis(ask_vector)
ask_kurtosis
ask_skewness = skewness(ask_vector)
ask_skewness
# Chebyshev's 75% interval
ask_upperbound = ask_mean + 2*ask_sd
ask_upperbound
ask_lowerbound = ask_mean - 2 * ask_sd
ask_lowerbound
#75% of the data is between $687424 and $133360.8
# Selling price
selling_price = as.vector(as.matrix(WallaceRealtorsFull[,3]))
selling_price
sell_mean = mean(selling_price)
sell_mean
sell_median = median(selling_price)
sell_median
sell_range = max(selling_price) - min(selling_price)
sell_range
sell_sd = sd(selling_price)
sell_cv = CV(selling_price)
sell_cv
sell_kurtosis = kurtosis(selling_price)
sell_kurtosis
sell_skewness = skewness(selling_price)
sell_skewness
# Chebyshev's 75% interval
sell_upperbound = sell_mean + 2* sell_sd
sell_upperbound
sell_lowerbound = sell_mean - 2 * sell_sd
sell_lowerbound
install.packages("moments")
CV = function(x){(sd(x)/mean(x))*100}
#Calculating price difference
WallaceRealtorsFull["PriceDifference"] = (WallaceRealtorsFull[2] - WallaceRealtorsFull[3])
#Calculating company wise mean , median , sd and cv
attach(WallaceRealtorsFull)
tapply(PriceDifference, Company, mean)
mean_pricedifference_companywise = tapply(PriceDifference, Company, mean)
mean_pricedifference_companywise
median_pricedifference_companywise = tapply(PriceDifference, Company, median)
median_pricedifference_companywise
sd_pricedifference_companywise = tapply(PriceDifference, Company, sd)
sd_pricedifference_companywise
cv_pricedifference_companywise = tapply(PriceDifference, Company, CV)
cv_pricedifference_companywise
#Creating histogram
install.packages("ggplot2")
library("ggplot2")
attach(WallaceRealtorsFull)
ggplot(WallaceRealtorsFull , aes(x=PriceDifference , group = Company , fill = Company )) + geom_histogram(position = "dodge" ) + theme_bw()
#Calculating confidence interval to see if the average of the population will be in these intervals
error = qnorm(0.975)*sd_pricedifference_companywise / sqrt (79)
error
left = (mean_pricedifference_companywise) - error
left
right = mean_pricedifference_companywise + error
right
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
#Calculating confidence interval to see if the average of the population will be in these intervals
error = qnorm(0.975)*sd_pricedifference_companywise / sqrt (79)
error
left = (mean_pricedifference_companywise) - error
left
right = mean_pricedifference_companywise + error
right
CV = function(x){(sd(x)/mean(x))*100}
#Asking Price
mean_ask_company = tapply(WallaceRealtorsFull$"Asking Price", Company, mean)
mean_ask_company
median_ask_company = tapply(WallaceRealtorsFull$"Asking Price", Company, median)
median_ask_company
sd_ask_company = tapply(WallaceRealtorsFull$"Asking Price", Company, sd)
sd_ask_company
cv_ask_company = tapply(WallaceRealtorsFull$"Asking Price", Company, CV)
cv_ask_company
#Selling price
mean_sell_company = tapply(WallaceRealtorsFull$"Selling Price", Company, mean)
mean_sell_company
median_sell_company = tapply(WallaceRealtorsFull$"Selling Price", Company, median)
median_sell_company
sd_sell_company = tapply(WallaceRealtorsFull$"Selling Price", Company, sd)
sd_sell_company
cv_sell_company = tapply(WallaceRealtorsFull$"Selling Price", Company, CV)
cv_sell_company
#Histogram
hist((`Asking Price`[Company == 'SRES']))
hist((`Asking Price`[Company == 'Wallace']))
hist((`Selling Price`[Company == 'Wallace']))
hist((`Selling Price`[Company == 'SRES']))
#Quantiles
quantile(`Asking Price`[Company == 'SRES'],probs = seq(0,1,0.05))
quantile(`Asking Price`[Company == 'Wallace'],probs = seq(0,1,0.05))
quantile(`Selling Price`[Company == 'Wallace'],probs = seq(0,1,0.05))
quantile(`Selling Price`[Company == 'SRES'],probs = seq(0,1,0.05))
#Creating frequency tables for "Company" and "House type"
breaks = seq(150000 , 750000,by = 50000)
prop.table(table(Company))
prop.table(table(`House Type`))
#Creating contingency table for Company and House Type
prop.table(table(`House Type`,Company))
#Classify Asking prices into cuts as mentioned in task
AskPriceCuts = cut(`Asking Price`,breaks)
prop.table(table(`House Type`,AskPriceCuts))
#Classify Selling prices into cuts as mentioned in task
Sellpricecuts = cut(`Selling Price`,breaks)
prop.table(table(`House Type`,Sellpricecuts))
