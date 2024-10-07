###################################################################################################################################################################
##### Example 9.2 Housing Price Equation #########################################################################################################################
##### Model 1: price = b0 + b1*lotsize + b2*sqrft + b3*bdrms + u #######################################
##### Model 2: lprice = b0 + b1*llotsize + b2*lsqrft + b3*bdrms + u #######################################
######RESET for functional form misspecification
###################################################################################################################################################################

# Nobs : 88
HPRICE1<-read.table("C:\\smchoi\\R\\TextDataFiles\\hprice1.raw", header=FALSE)

View(HPRICE1)
summary(HPRICE1)

# make variables for reg
price <-HPRICE1[,1]; #  house price, $1000s
lotsize <-HPRICE1[,4]; # size of lot in square feet
sqrft <-HPRICE1[,5]; # size of house in square feet
bdrms <-HPRICE1[,3]; # number of bedrooms

# resettest: joint significance of yhat and yhar^2.
#install.packages("lmtest")
library(lmtest)
resettest(price ~ lotsize + sqrft + bdrms)
#p-value = 0.01202: evidence of functional form misspecification
resettest(log(price) ~ log(lotsize) + log(sqrft) + bdrms)
#p-value = 0.08308: we do not reject this at the 5% significance level
#On the basis of RESET, the log-log model is preferred.
