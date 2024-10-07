#############################################################################
# Example 2.10 A Log Wage Equation
#############################################################################


# set the working directory to "" 
setwd("C:\\Users\\UOS\\Desktop/RCode") 

#loading data for Example 2.10 (log(wage) on edu) [random sample from the population of people in the workforce in 1976]

wage1<-read.table("C:\\Users\\UOS\\Desktop\\RCode\\TextDataFiles\\wage1.raw", header=FALSE) 
#F (T) can be used for FALSE (TRUE)
# use F when there is no header

#to learn more about the data
str(wage1)
summary(wage1)
fix(wage1) #shows data like an excel spread sheet.
#You can also edit directly from this sheet
wage1 #to see all data
#To clear R Console press Ctl + l

#Linear Regression by using lm function
#?lm #to learn more about lm function

###########Model######################
 log(wage) = b0 + b1*educ + u
######################################

#name variables since there is no header
wage<-wage1[,1] #average hourly earnings (in dollars)
#lwage<-log(wage)
educ<-wage1[,2] #years of education

#run a simple regression under homoskedasticity to study relationship between salary and roe
#reg1.wage1<-lm(lwage~educ)
reg1.wage1<-lm(log(wage)~educ)
summary(reg1.wage1)

plot(log(wage)~educ)
abline(reg1.wage1)


####################### Estimation Results ################################
#log(wage) = 0.584 + 0.083*educ
#100*Dlog(wage) = 100*0.083*Deduc 

###########Model######################
#wage = b0 + b1*educ + u
######################################
reg2.wage1<-lm(wage~educ)
summary(reg2.wage1)

plot(wage~educ)
abline(reg2.wage1)
