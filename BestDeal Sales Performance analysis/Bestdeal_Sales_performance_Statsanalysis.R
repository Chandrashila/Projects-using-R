# Importing dataset "Bestdeal" in R with employee columns as data type -'numeric'
library(readxl)
Bestdeal <- read_excel("C:/Users/rumic/Desktop/Desktop/MSBAPM_First_Semester/OPIM 5603_Manuel_Statistics in business Analytics/Assignment 2/Bestdeal.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric"))
View(Bestdeal)
# Merging the employees' data into single vector "Sales.Vector"
# Removing Column 1 from dataframe 'Bestdeal' before converting the employee data into a vector
View(Bestdeal)
Bestdeal1=Bestdeal[-1]
View(Bestdeal1)
Sales.Vector=as.vector(as.matrix(Bestdeal1))
#Calculating Mean of Sales.Vector after removing NA from vector
meanofsales=mean(Sales.Vector,na.rm = TRUE)
# Calculating Median of Sales.Vector after removing NA from vector
medianofsales=median(Sales.Vector,na.rm= TRUE)
# Calculating range of Sales.Vector
range=max(Sales.Vector,na.rm = TRUE) - min(Sales.Vector,na.rm = TRUE)
# Calculating sample standard deviation
stdv = sd(Sales.Vector,na.rm = TRUE)
# Calculating Coefficient of variation
cv= (stdv/mean(Sales.Vector,na.rm = TRUE))*100
# Installing package 'moments' for calculating Skewness and Kurtosis
install.packages("moments")
library(moments)
# Calculaing Skewness of Sales.Vector
skewness(Sales.Vector,na.rm = TRUE)
#Calculating Kurtosis for Sales.Vector
kurtosis(Sales.Vector,na.rm = TRUE)
#Calculating ChebyShev's interval
upper.range=meanofsales + 2*stdv
lower.range=meanofsales - 2*stdv
#On analyzing the coefficient of variation from the dataframe "Employee.Summary" we can conclude
#David has the highest dispersed sales data and Alfonso has the lowest dispersed sales data.
#Creating a two-dimensional dataframe with Employee and corresponding sales value
sales.person=c("Geoffrey","Kevin","David","Valori","Alfonso","Deborah","Jenny","Mary","Mindy","Robert")
sales.value=c(Bestdeal1$Geoffrey,Bestdeal1$Kevin,Bestdeal1$David,Bestdeal1$Valori,Bestdeal1$Alfonso,Bestdeal1$Deborah,Bestdeal1$Jenny,Bestdeal1$Mary,Bestdeal1$Mindy,Bestdeal1$Robert)
df1=rep(sales.person,c(160,160,160,160,160,160,160,160,160,160))
df_final= data.frame("Sales person"=df1,"Sales Value"=sales.value)
# 'tapply' to calculate mean, sample standard deviation and coefficient of variation for each employee
mean.person.level=tapply(df_final$Sales.Value,df_final$Sales.person,mean,na.rm=TRUE)
sd.person.level=tapply(df_final$Sales.Value,df_final$Sales.person,sd,na.rm=TRUE)
# Create function to calculate co-efficient of variation
cv = function(mean.person.level, sd.person.level){(sd.person.level/mean.person.level)*100}
cv(sd.person.level,mean.person.level)
cv_final=cv(mean.person.level,sd.person.level)
#Create dataframe "Employee.Summary" to record Mean, Standard deviation and Coefficient of #variation of each employee
Employee.Summary=data.frame("Mean"=mean.person.level,"Sample Standard deviation"=sd.person.level,"Coefficient of Variation"=cv_final)
View(Employee.Summary)
#Note:
#Using apply to calculate employee wise mean , SD and CV
EmployeeWiseMean = apply(Bestdeal[,-1],2,mean , na.rm=T)
EmployeeWiseSD = apply(Bestdeal[,-1],2,sd , na.rm=T)
CV = function(x){(sd(x,na.rm=T) / mean(x, na.rm=T))*100}
EmployeeWiseCV = apply(Bestdeal[,-1],2,CV)
#Employee.Summary Dataset
Employee.Summary = data.frame("MEAN" = EmployeeWiseMean , "SD" = EmployeeWiseSD , "CV" = EmployeeWiseCV)
View(Employee.Summary)
#Calculate High performer and Low performer value(according to the relation given in problem) for each employee
high.performer=mean.person.level - 0.2*sd.person.level
low.performer=mean.person.level + 0.2*sd.person.level
# ifelse command to sort employee as High performer and Low performer
Perfomance.empl=ifelse(high.performer> 146,"High", ifelse(low.performer< 146,"Low","NA"))
Performance=as.data.frame(Perfomance.empl)
# Combining the Performance column in "Employee.Summary" dataframe
Employee.Summary=cbind(Employee.Summary,Performance)
View(Employee.Summary)
#Creating dataframe with Days and total sales value
df2=data.frame(Bestdeal$Day,sales.value)
# Calculating mean sales value for all employee corresponding to each day
mean.day.level=tapply(df2$sales.value,df2$Bestdeal.Day,mean,na.rm=TRUE)
View(mean.day.level)
# Count number of 'NA' in sales value vector
df4=as.data.frame(rowSums(is.na(df2)))
df3=cbind(df2,df4)
View(df3)
# Summation of all 'NA' on the basis of Day levels
Absence.day.level=tapply(df3$`rowSums(is.na(df2))`, list(df3$Bestdeal.Day), function(x) {sum(x)})
View(Absence.day.level)
# Creating final Dataframe: "Daily.Sales"
Daily.Sales=cbind("Average sales"=mean.day.level,"Number of absences"=Absence.day.level)
View(Daily.Sales)
