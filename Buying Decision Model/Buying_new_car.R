library(readxl)
Tracy <- read_excel("C:/Users/rumic/Desktop/Desktop/MSBAPM_First_Semester/OPIM 5603_Manuel_Statistics in business Analytics/Assignment 4/Tracy.xlsx",
col_types = c("numeric", "numeric"))
View(Tracy)
attach(Tracy)
#Create frequency table for 'First owner number of years'
t.years=table(Years)
View(Years)
#Create probability table for random variable X = Number of years for first owner before they buy a #second car
p.years=prop.table(t.years)
View(p.years)
#Calculating Estimated value for X
E.X.years=sum(p.years*(2:8))
View(E.X.years)
#Calculating variance so as to get standard deviation
V.X=sum((((2:8) - E.X.years)^2)*p.years)
View(V.X)
SD.X.years=sqrt(V.X)
View(SD.X.years)
#Calculating Coefficient of Variance of X
CV.X.years=SD.X.years/E.X.years
View(CV.X.years)
#Calculating mean of maintenance cost data
mean.maint.cost=mean(Tracy$`Maintenance Cost per Mile`)
View(mean.maint.cost)
#Calculating median of maintenance cost data
median.maint.cost=median(Tracy$`Maintenance Cost per Mile`)
View(median.maint.cost)
#Range of maintenance cost per mile
max.maint.cost=max(Tracy$`Maintenance Cost per Mile`)
min.maint.cot=min(Tracy$`Maintenance Cost per Mile`)
range.maint.cost= max.maint.cost - min.maint.cot
View(max.maint.cost)
View(min.maint.cot)
View(range.maint.cost)
#Calculating standard deviation of maintenance cost data
sd.maint.cost=sd(Tracy$`Maintenance Cost per Mile`)
View(sd.maint.cost)
#Calculating coefficient of variance of maintenance cost data
cv.maint.cost=(sd.maint.cost/mean.maint.cost)*100
View(cv.maint.cost)
#Chebyshev's interval of maintenance cost data
#Lower bound
mean.maint.cost-2*sd.maint.cost
#Upper bound
mean.maint.cost+2*sd.maint.cost
#Calculating Skewness and Kurtosis of maintenance cost data
install.packages("moments")
library(moments)
skewness(Tracy$`Maintenance Cost per Mile`)
kurtosis(Tracy$`Maintenance Cost per Mile`)
#converting p.Y to maintenance cost per year
# multiplying t.Y with 'm'= mean maintenance cost
new.Y=c(9000*0.081,12000*0.081)
View(new.Y)
new.Y1=rep(new.Y,c(60,40))
View(new.Y1)
#frequency table of maintenance cost per year
new.t.Y=table(new.Y1)
View(new.t.Y)
#probability table of maintenance cost per year
new.p.Y=prop.table(table(new.Y1))
View(new.p.Y)
#Creating dataframe D1 for including random variable X, P(X), maintenance cost per year, P(Y)
D1=data.frame("Years"=rep(2:8,2),"P(X)"=rep(p.years,2),"Maintenance cost per year"= rep(c(9000*0.08149,12000*0.08149),c(7,7)),"P(Y)"=rep(new.p.Y,c(7,7)))
View(D1)
#Creating dataframe Z=mXY
Z=data.frame(D1,"Maintenance cost calculated"=Z$Years*Z$Maintenance.cost.per.year,"P(X).P(Y)"= D1$P.X.*D1$P.Y.)
View(Z)
#Calculating expected value for random variable Z
E.Z=sum(Z$Maintenance.cost.calculated*Z$P.X..P.Y.)
#Calculating standard deviation for random variable Z
V.Z=sum(((Z$Maintenance.cost.calculated-E.Z)^2)*Z$P.X..P.Y.)
SD.Z=sqrt(V.Z)
#Calculating Coefficient of Variance for random variable Z
CV.Z=SD.Z/E.Z
CV.Z
#To get the probability where maintenance cost is less than $3000
sumProbability = sum(Z$P.X..P.Y.[Z$Maintenance.cost.calculated < 3000])
sumProbability
