################################################################################################
##### Example 10.3 Puerto Rican Employment and the Minimum Wage ################################
##### Model 1 : log(prepop) = b0 + b1*log(mincov) + b2*log(usgnp) + u ##########################
# to study the effects of the U.S. minimum wage on employment in Puerto Rico.
################################################################################################
#Annual data on the Puerto Rican employment rate, minimum wage, and other variables
# Nobs : 38 (years 1950 through 1987)
PRMINWGE<-read.table("PRMINWGE.raw", header=FALSE)

View(PRMINWGE)
summary(PRMINWGE)

# make variables for reg
prepop <-PRMINWGE[,9]; # employment rate in Puerto Rico (PR employ/popul ratio)
mincov <-PRMINWGE[,23];   # (avgmin/avgwage)*avgcov: measures the importance of the minimum wage relative to average wages
#avgmin: the average minimum wage,
#avgwage: the average overall wage, and 
#avgcov: the average coverage rate (the proportion of workers actually covered by the minimum wage law).
usgnp <-PRMINWGE[,13]; # US GNP, in billions of dollars

# reg under Homoskedasticity
reg1.PRMINWGE<-lm(log(prepop) ~ log(mincov) + log(usgnp))
summary(reg1.PRMINWGE)

####Results#######
#The estimated elasticity of prepop with respect to mincov is -0.154, and it is statistically significant.
#Therefore, a higher minimum wage lowers the employment rate, something that classical economics predicts. 
#The GNP variable is not statistically significant, but this changes when we account for a time trend in the next section.

################################################################################################
##### Example 10.9 Puerto Rican Employment #####################################################
##### Model 2 : log(prepop) = b0 + b1*log(mincov) + b2*log(usgnp) + t + u ######################
################################################################################################

# make variables for reg
t <-PRMINWGE[,14]; # time trend:  1 to 38

# reg with time trend under Homoskedasticity
reg2.PRMINWGE<-lm(log(prepop) ~ log(mincov) + log(usgnp) + t)
summary(reg2.PRMINWGE)

####Results#######
#The coefficient on log(usgnp) has changed dramatically: from -0.012 and insignificant to 1.06 and very significant.
#The coefficient on the minimum wage has changed only slightly and still significant.
#The variable prepop displays no clear upward or downward trend, 
#but log(usgnp) has an upward, linear trend.
#How to interpret the coefficient of log(usgnp)?
#When usgnp increases by 1% above its long-run trend, prepop increases by about 1.06%.


