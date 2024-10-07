####################################################################################
##### Example 10.7 Housing Investment and Prices            ########################
##### Model 1 : log(invpc) = b0 + b1*log(price) + u         ########################
##### Model 2 : log(invpc) = b0 + b1*log(price) + b2*t + u  ########################
####################################################################################
#annual observations on housing investment and a housing price index in the United States for 1947 through 1988.
# Nobs : 131
HSEINV<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\HSEINV.raw", header=FALSE)

View(HSEINV)
summary(HSEINV)

# make variables for reg
#invpc <-HSEINV[,9]; # real per capita housing investment (in thousands of dollars)
linvpc <-HSEINV[,10]; # log(invpc)
#price <-HSEINV[,4];   # housing price index; 1982 = 1
lprice  <-HSEINV[,7]; # log(price)
t <-HSEINV[,8]; # time trend: t=1,...,42

#To estimate a supply equation for housing stock
# reg under Homoskedasticity
reg1.HSEINV<-lm(linvpc ~ lprice)
summary(reg1.HSEINV)

############### Results #######################
#The elasticity of per capita investment with respect to price is very large and statistically significant.
  #it is not statistically different from one.

#Both invpc and price have upward trends.
plot(t,linvpc,type="l")
plot(t,lprice,type="l")

#To account for the trending behavior of the variables, we add a time trend:
# reg under Homoskedasticity with time trend

reg2.HSEINV<-lm(linvpc ~ lprice + t)
summary(reg2.HSEINV)

############### Results #######################
#the estimated price elasticity is negative and not statistically different from zero.
#The time trend is statistically significant, and 
#its coefficient implies an approximate 1% increase in invpc per year,

#we cannot conclude that real per capita housing investment is influenced at all by price. 
#There are other factors, captured in the time trend, that affect invpc, but we have not modeled these.
#The results in (10.32) (without t) show a spurious relationship between invpc and price 
  #due to the fact that price is also trending upward over time.

plot(t,linvpc, type = "l")
plot(t,lprice, type = "l")
#R-squared from this regression says that we are ??explaining?? 34.1% of the variation in log(invpc).
#This is misleading.
####################################################################################
##### Example 10.10 Housing Investment                                   ###########
##### Model 3 : log(invpc) = b0 + b1*t + u                               ###########
##### Model 4 : log(invpc)_hat[=u_hat] = b0 + b1*log(price) + b2*t + u   ###########
##### Model 5 : log(invpc) = b0 + b1*log(price) + b2*t + u               ###########
####################################################################################

# Detrend
reg3.HSEINV<-lm(linvpc ~ t)
#summary(reg3.HSEINV)
det_linvpc<-reg3.HSEINV$residuals

# Detrended R-squared 
reg4.HSEINV<-lm(det_linvpc ~ lprice + t)
summary(reg4.HSEINV)

#the R-squared becomes .008.
#Thus, movements in log(price) about its trend have virtually no explanatory power for movements in log(invpc) about its trend.


