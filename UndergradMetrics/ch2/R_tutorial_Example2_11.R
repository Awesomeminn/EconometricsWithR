#############################################################################
# Example 2.11 CEO Salary and Firm Sales (Same data as Example 2.3)
#############################################################################

###########Model######################
 log(salary) = b0 + b1*log(sales) + u
######################################

ceosal1<-read.table("C:\\Users\\UOS\\Desktop\\RCode\\TextDataFiles\\ceosal1.raw",header=FALSE) 

salary<-ceosal1[,1] #annual salary in thousands of dollars
#lsalary<-ceosal1[,11] #log(annual salary in thousands of dollars).

sales<-ceosal1[,3] #sales annual firm sales, measured in millions of dollars.
#lsales<-ceosal1[,12] #sales annual firm sales, measured in millions of dollars.


reg1.ceosal1<-lm(log(salary)~log(sales))
summary(reg1.ceosal1)

plot(log(salary)~log(sales))
abline(reg1.ceosal1)

####################### Estimation Results ################################
#1% increase in firm sales increases CEO salary by about 0.257% ceteris paribus.
#elasticity = 0.257

###########Model######################
#salary = b0 + b1*log(sales) + u
######################################

reg2.ceosal1<-lm(salary~log(sales))
summary(reg2.ceosal1)

plot(salary~log(sales))
abline(reg2.ceosal1)

###########Model######################
#log(salary) = b0 + b1*sales + u
######################################

reg3.ceosal1<-lm(log(salary)~sales)
summary(reg3.ceosal1)

plot(log(salary)~sales)
abline(reg3.ceosal1)




