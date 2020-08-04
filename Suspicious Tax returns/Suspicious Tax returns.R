library(readxl)
Taxes <- read_excel("C:/Users/rumic/Desktop/Desktop/MSBAPM_First_Semester/OPIM 5603_Manuel_Statistics in business Analytics/Assignment 9/Taxes.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"text"))
View(Taxes)
#Building regression model
attach(Taxes)
tax.df=data.frame(Taxes[,2:7])
View(tax.df)
tax.reg=lm(`Taxes Owed ($)`~`Gross Pre-Tax Income ($)`+`Schedule-A Deductions ($)`+`Schedule-C Income ($)`+`Schedule C Deductions Percentage (%)`+`Home Office`)
summary(tax.reg)
#Backward regression model for Tax data
step(tax.reg,data=tax.df,direction = "backward")
tax.reg1=lm(`Taxes Owed ($)`~`Gross Pre-Tax Income ($)`+`Schedule C Deductions Percentage (%)`+`Home Office`)
summary(tax.reg1)
#regression model for new values
tax.df.pred.reg=lm(`Taxes Owed ($)`~`Gross Pre-Tax Income ($)`+`Schedule C Deductions Percentage (%)`+`Home Office`)
attach(tax.df.pred)
tax.df.pred.reg=lm(Taxes.Owed....~Gross.Pre.Tax.Income....+Schedule.C.Deductions.Percentage....+Home.Office)
tax.pred.val=fitted(tax.df.pred.reg)
View(tax.pred.val)
tax.df.pred1=data.frame(tax.df.pred[,1:2],tax.df.pred[5:6],"Tax_owed_predicted"=tax.pred.val)
View(tax.df.pred1)
tax.df.pred1=data.frame(Taxes[,1],tax.df.pred[,1:2],tax.df.pred[5:6],"Tax_owed_predicted"=tax.pred.val)
View(tax.df.pred1)
tax.SE=sd(Taxes.Owed....)/sqrt(10)
tax.SE
reg.diff=tax.df.pred1$Taxes.Owed.... - tax.df.pred1$Tax_owed_predicted
View(reg.diff)
tax.score=(tax.df.pred1$Taxes.Owed.... - tax.df.pred1$Tax_owed_predicted)/tax.SE
View(tax.score)
Red.Flag.Ind=ifelse(tax.score < -3,"Y","N")
tax.df.pred1=data.frame(tax.df.pred1,"Tax.score"=tax.score,"Red.Flag.Ind"=Red.Flag.Ind)
View(tax.df.pred1)
