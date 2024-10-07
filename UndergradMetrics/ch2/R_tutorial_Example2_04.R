#############################################################################
# Example 2.4 Wage and Education
#############################################################################

#For the population of people in the workforce in 1976,
#526 individuals

###########Model######################
# wage = b0 + b1*educ + u
######################################

wage1<-read.table("C:\\Users\\UOS\\Desktop\\RCode\\TextDataFiles\\wage1.raw",header=FALSE) 

wage<-wage1[,1] #wage measured in dollars per hour. 
educ<-wage1[,2] #years of schooling.

#### functions for basic statistics ############
mean(wage)
sum(educ)
cov(wage,educ)
cor(wage,educ)

reg1.wage1<-lm(wage~educ)
summary(reg1.wage1)

#The slope(0.54) implies that one more year of education increases hourly wage by 54 cents an hour when other things are held fixed.

yhat<-reg1.wage1$fitted.values
#yhat<-reg1.wage1$fit #Do the same thing as above
uhat<-reg1.wage1$residuals
#uhat<-reg1.wage1$resid #Do the same thing as above

sum(uhat)
cov(educ,uhat)
cov(yhat,uhat)
cor(yhat,wage)^2

plot(wage~educ, data=wage1) #draw scattered plot of all pairs of (edu,wage) observations
abline(reg1.wage1) #add a straight line (regression function line) to the plot
points(yhat~educ, data=wage1, pch=17) #Add points to the plot
                                      #draw a sequence of points at the (edu,yhat) coordinates. 
                                      #pch specifies which character to draw, for example 0 square, 1 circle, 2 triangle etc




