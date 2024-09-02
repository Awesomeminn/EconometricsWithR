sink("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\review\\review_example6_2.txt", append = F)

#############################################################################
# Example 6.2 Effects of Pollution on Housing Prices
#############################################################################

#Anything that follows a # on the command line is taken as comment and ignored by R.

# set the working directory to "" 
# You have to use "/" or "\\" not "\"
setwd("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode")    

#Data on sample of 506 communities in the Boston area

HPRICE2<-read.table("C:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\HPRICE2.raw", header=FALSE)

#The <- is a left diamond bracket (<) followed by a minus sign (-). It means ??is assigned to??.
# You can use = instead of <-
#F (T) can be used for FALSE (TRUE)
# Use T when the first line of the data file has header information for the columns.

#to learn more about the data
str(HPRICE2) 
names(HPRICE2) #Returns only names of all variables
nrow(HPRICE2) #Returns the number of rows (observations)
ncol(HPRICE2) #Returns the number of columns (variables)
head(HPRICE2) #Returns variable names and first few observations

View(HPRICE2)
fix(HPRICE2) #shows data like an excel spread sheet.
#You can also edit directly from this sheet
summary(HPRICE2)

HPRICE2 #to see all data

#To clear R Console press Ctl + l

#Linear Regression by using lm function
#?lm #or help(lm) #to learn more about lm function

#################################################################################################
####################################### Model ###################################################
############################ Linear Regression Models ###########################################
#################################################################################################
#################################################################################################

#reg1.HPRICE2<-lm(V10~V4, data=HPRICE2)
#summary(reg1.HPRICE2)
#reg1.HPRICE2<-lm(HPRICE2$V10~HPRICE2$V4) #No variable names
#summary(reg1.HPRICE2)

lprice<-HPRICE2[,10] #in the community to various community characteristics 
lnox<-HPRICE2[,11] #the amount of nitrogen oxide in the air
dist<-HPRICE2[,5] #a weighted distance of the community from five employmeet centers
#ldist<-log(dist)
rooms<-HPRICE2[,4] #the average number of rooms in houses in the community
#rooms2<-rooms^2
proptax<-HPRICE2[,7] #square footage of house
rooms_proptax<-rooms*proptax
stratio<-HPRICE2[,8] #the average student-teacher ratio of schools in the community

mean(lprice)
sum(rooms)
cov(lprice,rooms)
cor(lprice,rooms)


###################### Simple Linear Regression Model #######################################
############################## log(price) = b0 + b1*rooms + u ##############################
#############################################################################################

reg1.HPRICE2<-lm(lprice~rooms)
summary(reg1.HPRICE2)

yhat<-reg1.HPRICE2$fitted.values
#yhat<-reg1.wage1$fit
uhat<-reg1.HPRICE2$residuals
#uhat<-reg1.wage1$resid


sum(uhat)
cov(rooms,uhat)
cov(yhat,uhat)

#jpeg("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\review\\olsgraph.jpg") #to starting a graph
#For pdf 
#pdf("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\review\\olsgraph.pdf")
#For bmp
bmp("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\review\\olsgraph.bmp")
#plot(rooms,lprice)
plot(lprice~rooms)
abline(reg1.HPRICE2)
#abline(reg1.HPRICE2$coefficients)
points(yhat~rooms, pch=19)
graphics.off() #to turn off exporting a graph

####################### Simple Linear Regression Model without an intercept####################
############################## log(price) = b1*rooms + u ##############################
############################################################################################

#without intercept 
reg2.HPRICE2<-lm(lprice~0+rooms) #-1+rooms will do the same thing
summary(reg2.HPRICE2)

####################################### Model ###################################################
#### log(price) = b0 + b1*log(nox) + b2*log(dist) + b3*rooms + b4*(rooms)^2 + b5*stratio + u ####
#################################################################################################

#A linear model explaining
#reg3.HPRICE2<-lm(lprice~lnox+log(dist)+rooms+rooms2+stratio)
reg3.HPRICE2<-lm(lprice~lnox+log(dist)+rooms+I(rooms^2)+stratio)
summary(reg3.HPRICE2)


####################################### Model ###################################################
#### log(price) = b0 + b1*log(nox) + b2*log(dist) + b3*rooms + b4*proptax + b5*rooms*proptax + u ####
#################################################################################################

#reg4.HPRICE2<-lm(lprice~lnox+log(dist)+rooms+proptax+rooms_proptax)
reg4.HPRICE2<-lm(lprice~lnox+log(dist)+rooms+proptax+rooms:proptax)
#reg4.HPRICE2<-lm(lprice~lnox+log(dist)+rooms*proptax)
summary(reg4.HPRICE2)

#####################################################################
#######################testing hypothesis######################
#####################################################################
#need to install package "car"

"car" %in% installed.packages() #to check whether or not R has a package
#install.packages("car", dependencies = T) #dependency means this package is used in other area

library(car) #bring this package whenever needed.

#testing one linear hypothesis
linearHypothesis(reg4.HPRICE2, "lnox-log(dist)=0")

#testing H0:b4=0, b5=0 (q=2 restrictions)
#sample size n

#testing more than one linear hypotheses jointly
linearHypothesis(reg4.HPRICE2, c("rooms=0", "proptax=0"))
linearHypothesis(reg4.HPRICE2, c("(Intercept)=0"))
#lht(reg4.HPRICE2, c("rooms=0", "proptax=0"))
linearHypothesis(reg4.HPRICE2, c("lnox-log(dist)=1", "2*rooms+proptax=0", "rooms:proptax=0"))

sink()
