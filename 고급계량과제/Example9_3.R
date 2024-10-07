###################################################################################################################################
##### Example 9.3 IQ as a Proxy for Ability #######################################################################################
### Blackburn and Neumark (1992)
##### Model 1: log(wage) = b0 + b1*educ + b2*exper + b3*tenure + b4*married + b5*south  + b6*urban+ b7*black + u ##################
##### Model 2: log(wage) = b0 + b1*educ + b2*exper + b3*tenure + b4*married + b5*south  + b6*urban+ b7*black + b8*IQ + u ##########
##### Model 3: log(wage) = b0 + b1*educ + b2*exper + b3*tenure + b4*married + b5*south  + b6*urban+ b7*black + b8*IQ + b9*educ*IQu 
#### Our primary interest is in what happens to the estimated return to education.
###################################################################################################################################

# Nobs : 935
WAGE2<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\WAGE2.raw", header=FALSE)

View(WAGE2)
summary(WAGE2)

# make variables for reg
wage<-WAGE2[,1]; #  monthly earnings
educ<-WAGE2[,5];   # years of education
exper<-WAGE2[,6]; # years of work experience
tenure<-WAGE2[,7]; # years with current employer
married<-WAGE2[,9]; # =1 if married
south<-WAGE2[,11]; # =1 if live in south
urban<-WAGE2[,12]; # =1 if live in SMSA
black<-WAGE2[,10]; # =1 if black
IQ<-WAGE2[,3]; # IQ score for 935 men in 1980

# reg under Homoskedasticity
#without using IQ as a proxy variable
reg1.WAGE2<-lm(log(wage) ~ educ + exper + tenure+ married + south + urban + black)
summary(reg1.WAGE2)
#The estimated return to education is 6.5%.
#If we think omitted ability is positively correlated with educ, then we assume that this estimate is too high.

##Using IQ as a proxy variable
reg2.WAGE2<-lm(log(wage) ~ educ + exper + tenure+ married + south + urban + black + IQ)
summary(reg2.WAGE2)
#the return to education falls to 5.4%.
#This corresponds with our prior beliefs about omitted ability bias.

#Add the interaction term educ*IQ.
#We might think that the return to education is higher for people with more ability
reg3.WAGE2<-lm(log(wage) ~ educ + exper + tenure+ married + south + urban + black + IQ + I(educ*IQ))
summary(reg3.WAGE2)
#this turns out not to be the case.
#the interaction term is not significant.
#its addition makes educ and IQ individually insignificant
#while complicating the model. 
#Therefore, the 2nd model is preferred.