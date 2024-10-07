################################################################################################
##### Example 10.4 Effects of Personal Exemption on Fertility Rates  ###########################
##### Model 1 : gfr = b0 + b1*pe + b2*ww2 + b3*pill + u              ###########################
##### Model 2 : gfr = a0 + d0*pe + d1*pe_1 + d2*pe_2 + b1*ww2 + b2*pill + u ####################
################################################################################################

# Nobs : 72
FERTIL3<-read.table("FERTIL3.raw", header=FALSE, na.strings=c(".","NA"))

View(FERTIL3)
summary(FERTIL3)

# make variables for reg
gfr <-FERTIL3[,1]; # The general fertility rate: births per 1000 women 15-44
pe <-FERTIL3[,2];   # the average real dollar value of the personal tax exemption (real value pers. exemption, $)
ww2 <-FERTIL3[,11]; # =1, 1941 to 1945, when the United States was involved in World War II.
pill <-FERTIL3[,10]; # =1 if year >= 1963, when the birth control pill was made available for contraception.

# reg under Homoskedasticity
reg1.FERTIL3<-lm(gfr ~ pe + ww2 + pill)
summary(reg1.FERTIL3)
###################Results###########################
#Each variable is statistically significant at the 1% level against a two-sided alternative.
#there were about 24 fewer births for every 1,000 women during World War II.
  #From 1913 through 1984, gfr ranged from about 65 to 127.
#The fertility rate has been substantially lower since the introduction of the birth control pill.
#The coefficient on pe implies that a $12.00 increase in pe increases gfr by about one birth per 1,000 women
  #The average pe over this time period is $100.40, ranging from zero to $243.83.

#The fertility rate may react to changes in pe with a lag.
#Let us estimate a distributed lag model with two lags
pe_1 <-FERTIL3[,6]; #pe_[t-1]
pe_2 <-FERTIL3[,7]; #pe_[t-2]

reg2.FERTIL3<-lm(gfr ~ pe + pe_1 + pe_2 + ww2 + pill)
summary(reg2.FERTIL3)

#In this regression, we only have 70 observations because we lose two when we lag pe twice.
#The coefficients on the pe variables are estimated very imprecisely, and each one is individually insignificant.
  #There is substantial correlation between pe_t,pet_[t-1], and pet_[t-2]. (can be a multicollinearity problem)
  #pe_t, pet_[t-1], and pet_[t-2] are jointly significant.

# linear hyphthesis H0: b1=b2=b3=0 H1: Otherwise
library(car)
lht(reg2.FERTIL3, c("pe = 0", "pe_1=0", "pe_2=0"))

#pet_[t-1], and pet_[t-2] are jointly insignificant.
lht(reg2.FERTIL3, c("pe_1=0", "pe_2=0"))
#At this point, we would be justified in using the static model.

# The estimated LRP is d0 + d1 + d2
sum(reg2.FERTIL3$coefficients[2:4])
#To obtain the standard error of the estimated LRP, let theta0 = d0 + d1 + d2 and
#replace delta0 with theta0 - d1 - d2 then Model 2 becomes
# gfr = a0 + theta0*pe + d1*(pe_1 - pe) + d2*(pe_2 - pe) + b1*ww2 + b2*pill + u
dif1=pe_1-pe
dif2=pe_2-pe
reg3.FERTIL3<-lm(gfr ~ pe + dif1 + dif2 + ww2 + pill)
summary(reg3.FERTIL3)

#The coefficient and associated standard error on pe are what we need.
#So, theta0 is statistically different from zero.
#The 95% confidence interval for the LRP is
confint(reg3.FERTIL3, 'pe', level = .95)


################################################################################################
##### Example 10.8 Fertility Equation                                      #####################
##### Model 4 : gfr = b0 + b1*pe + b2*ww2 + b3*pill + b4*t + u             #####################
##### Model 5 : gfr = b0 + b1*pe + b2*ww2 + b3*pill + b4*t + b4*(t^2) + u  #####################
################################################################################################
#If we add a linear time trend to the fertility equation
# make variables for reg
t <-FERTIL3[,4]; # 1913 to 1984
tsq <-FERTIL3[,5]; # t^2

# reg under Homoskedasticity
reg4.FERTIL3<-lm(gfr ~ pe + ww2 + pill + t)
summary(reg4.FERTIL3)

#The coefficient on pe is more than triple the estimate from (10.18), and it is much more statistically significant.
#Interestingly, pill is not significant.
#gfr was falling, on average, over this period (the coeff of t is negative)

#Since the general fertility rate exhibited both upward and downward trends during the period from 1913 through 1984,
plot(t,gfr, type="l")
#let us use a quadratic trend
reg5.FERTIL3<-lm(gfr ~ pe + ww2 + pill + t + tsq)
summary(reg5.FERTIL3)

#The coefficient on pe is even larger and more statistically significant.
#Now, pill has the expected negative effect and is marginally significant, 
#and both trend terms are statistically significant.
