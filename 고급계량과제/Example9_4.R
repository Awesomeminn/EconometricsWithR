#############################################################################################################
##### Example 9.4 City Crime Rates ##########################################################################
##### Model 1: log(crmrte87) = b0 + b1*unem87 + b2*log(lawexpc87) + u #######################################
##### Model 2: log(crmrte87) = b0 + b1*unem87 + b2*log(lawexpc87) + b3*log(crmrte82) + u ####################
####  Using the lagged crime rate as a proxy to control for city unobservables
########that affect crime and may be correlated with current law enforcement expenditures.
#############################################################################################################

# Nobs : 92
CRIME2<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\CRIME2.raw", header=FALSE)

View(CRIME2)
summary(CRIME2)

# make variables for reg
crmrte<-CRIME2[,13]; # crimes per 1000 people
unem<-CRIME2[,3];   # unemployment rate
lawexpc<-CRIME2[,15]; # law enforce. expend. pc, $
year<-CRIME2[,9]; # 82 or 87

crmrte87<-subset(crmrte, year==87)
unem87<-subset(unem, year==87)
lawexpc87<-subset(lawexpc, year==87)
crmrte82<-subset(crmrte, year==82)


# reg under Homoskedasticity
#Without the lagged crime rate
reg1.CRIME2<-lm(log(crmrte87) ~ unem87 + log(lawexpc87))
summary(reg1.CRIME2)
#the effects of the unemployment rate and expenditures on law enforcement are counterintuitive
#neither is statistically significant

#Adding the log of the crime rate from five years earlier
reg2.CRIME2<-lm(log(crmrte87) ~ unem87 + log(lawexpc87) + log(crmrte82))
summary(reg2.CRIME2)
#The elasticity of the crime rate with respect to expenditures becomes -0.14, with t = -1.28.
#the current crime rate is strongly related to the past crime rate.
  #if the crime rate in 1982 was 1% higher, then the crime rate in 1987 is predicted to be about 1.19% higher.
  #We cannot reject the hypothesis that the elasticity of current crime with respect to past crime is 1.
#Adding the past crime rate increases the explanatory power of the regression markedly, but this is no surprise.
#The primary reason for including the lagged crime rate is 
#to obtain a better estimate of the ceteris paribus effect of log(lawexpc87) on log(crmrte87)
