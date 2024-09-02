sink("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\review\\review_example8_2.txt", append = F)
######################################################################################################
##### Example 8.2 Heteroskedasticity-Robust F Statistic ##############################################
##### Model: cumgpa = b0 + b1*sat + b2*hsperc + b3*tothrs + b4*female + b5*black + b6*white + u ######
######################################################################################################

# Nobs : 732

GPA3<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\GPA3.raw", header=FALSE)

View(GPA3)
summary(GPA3)
nrow(GPA3)

# There are some NA obs and the number of them would be 366
GPA3<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\GPA3.raw", header=FALSE, na.strings=c(".","NA"))

View(GPA3)
summary(GPA3)
nrow(GPA3)

#To get the same estimation resuls as those in the textbook
GPA3<-na.omit(GPA3)

# make variables for reg
cumgap<-GPA3[,4]; #cumulative GPA
sat<-GPA3[,2];    #SAT score
hsperc<-GPA3[,22];#high school rank percentile = 100*(rank/hssize), where hssize = size of high school graduating class
tothrs<-GPA3[,3]; #total hours of college courses prior to term
female<-GPA3[,14];#=1 if female
black<-GPA3[,15]; #=1 if black
white<-GPA3[,16]; #=1 if white

# Run the regression under homoskedasticity
reg1.gpa3<-lm(cumgap ~ sat + hsperc + tothrs + female + black + white)
summary(reg1.gpa3)

# To compute H-robust variance, we can use two packages as follows.
# Just use car rather than lmtest and sandwich.
# car is easy to use.

library(car)
library(lmtest)
#hccm is a function from the car package
#coeftest is a function from the lmtest package
coeftest(reg1.gpa3, vcov = hccm(reg1.gpa3, "hc0"))
#hc0: Eicker (1963) and White (1980)
#hc1: Hinkley (1977), the default robust covariance matrix estimator in Stata
#hc2: Horn, Horn and Duncan (1975)
#hc3: MacKinnon and White (1985), Andrews (1991) default in R packages, car and lmtest
#hc4: Cribari-Neto (2004)

#The differences between the usual standard errors and the heteroskedasticity-robust standard errors are not very big.
#Use of the robust t statistics does not change the statistical significance of any independent variable.

#Joint hypothesis test Ho: b5=0 and b6=0
#hccm is a function from the car package
linearHypothesis(reg1.gpa3, c("black=0", "white=0"),vcov = hccm(reg1.gpa3, "hc0")) # HC0
#Or
lht(reg1.gpa3, c("black=0", "white=0"),white.adjust = 'hc0') #HC0

#The usual F statistic is obtained by
lht(reg1.gpa3, c("black=0", "white=0"))

#The value of the heteroskedasticity-robust F statistic turns out to be .75, 
#which differs only slightly from the nonrobust version.
#We fail to reject the null hypothesis using either test.

library(sandwich)
#vcovHC is a function from the sandwich package
linearHypothesis(reg1.gpa3, c("black=0", "white=0"),vcov=vcovHC) # HC3
linearHypothesis(reg1.gpa3, c("black=white", "2tothrs=female"),vcov=vcovHC) # HC3
#To change HC to HC0
linearHypothesis(reg1.gpa3, c("black=0", "white=0"),vcov=vcovHC(reg1.gpa3,"HC0")) # HC0

sink()

