library(readxl)
EasternCafe <- read_excel("C:/Users/rumic/Desktop/Desktop/MSBAPM_First_Semester/OPIM 5603_Manuel_Statistics in business Analytics/Assignment 5/EasternCafe.xlsx",
col_types = c("numeric", "numeric", "text",
"text", "text", "numeric"))
View(EasternCafe)
attach(EasternCafe)
TimeValues = as.vector(as.matrix(EasternCafe$Time))
timemean = mean(TimeValues)
timemean
timemedian = median(TimeValues)
timemedian
#From the above we can see that mean is approximately equal to median 12.5
sdtime = sd(TimeValues)
sdtime
#Standard deviation is less indicating less dispersion around the mean.
install.packages("moments")
library("moments")
skewnesstime = skewness(TimeValues)
skewnesstime
kurtosistime = kurtosis(TimeValues)
kurtosistime
#Skewness shows that symmetry is very lightly inclined towards left but is close to zero.
#Kurtosis shows that the curve is of bell shaped and is equal to 3 closely.
#The above 3 shows that the data is normally distributed.
#Delivery time will exceed 20 minutes
#P(X > 20) = 1 - F(20)
1 - pnorm(20,mean = timemean , sd = sdtime)
#Delivery time will be below 10 minutes
#P(X < 10) = P(X <= 9.59)
pnorm(9.59,mean = timemean , sd = sdtime)
#90% of delivery time under how many minutes?
qnorm(0.9,mean = timemean , sd = sdtime)
#90% of delivery time above how many minutes?
#This will be rewritten as delivery time less than 10% because then 90% will be above this 10%
qnorm(0.1,mean=timemean,sd=sdtime)
install.packages("moments")
#Probability Orders Yang Chow rice given that order includes noodles
#probability of yang chow and noodles
pOrderYandN = prop.table(table(EasternCafe$Y,EasternCafe$N))['Yes','Yes']
pOrderYandN
#probability of noodles
pOrderN = prop.table(table(EasternCafe$N))['Yes']
pOrderN
pOrderYandN/pOrderN
#Orders Yang Chow rice given that order includes the chicken dish
#probability of yang chow and chicken dish
pOrderYandT = prop.table(table(EasternCafe$Y,EasternCafe$T))['Yes','Yes']
pOrderT = prop.table(table(EasternCafe$T))['Yes']
pOrderYandT/pOrderT
#Probability Order noodles given that order includes the chicken dish.
pOrderNandT = prop.table(table(EasternCafe$N,EasternCafe$T))['Yes','Yes']
pOrderNandT/pOrderT
#Order noodles given yang chow
pOrderY = prop.table(table(EasternCafe$Y))['Yes']
pOrderY
pOrderYandN/pOrderY
#Order chicken given yang chow
pOrderYandT/pOrderY
#Order chicken given noodles
pOrderNandT/pOrderN
#See if the three orders are independent of each other from the above
prop.table(table(EasternCafe$Y,EasternCafe$N,EasternCafe$T))['Yes','Yes','Yes']
pOrderY*pOrderN*pOrderT
#The probabilites are not same and hence they are not independent of each other
#To provide recommendations
#We can see the combined probabilites of dishes and see the combo that has higher probability and suggest the same.
#No of orders placed per day between 5 pm to 6 pm
# X is a random variable denoting no of orders placed between 5 pm to 6 pm  on a random day.
#Poisson , since X can be any number of order
#Probability of order more than 10
#P(X > 10) = 1 - F(X)
#Average orders per day should be known
table(EasternCafe$Day)
OrdersPerDay = table(EasternCafe$Day)
Orders = data.frame(OrdersPerDay)
Orders = data.frame("Day" = Orders$Var1 , "Numbers" = Orders$Freq)
View(Orders)
AverageOrder = mean(Orders$Numbers)
AverageOrder
#Lets say average order per day is 20.
#P(X > 10)
1 - ppois(10,lambda = 20)
#99 % no of orders will be more than 10 between 5 pm and 6 pm
# Probability that no of orders will be less than 25
# This can be any value between 0 and 24 (inclusive)
# P(X < 25) = P(X <= 24) = F(24)
ppois(24,lambda = 20)
# Probability that order will be less than 25 is 84%
#Probability that less than 10 orders will have chicken in 20 orders
#Probability of chicken is the success event
# using binomial
#P(X<10) = P(X<=9) = F(9)
pbinom(9,size=20,prob = pOrderT)
#71% of the orders will have chicken
# 90% of days there will be fewer than how many orders?
qpois(0.9,lambda = 20)
# 90% of days there will be more than how many orders?
qpois(0.1,lambda = 20)
