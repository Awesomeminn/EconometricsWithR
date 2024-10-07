######################################################################################################
##### Example 10.1 Static Phillips Curve ##############################################
# The static Phillips curve is given by
# Model: inf = b0 + b1*unem + u
#  where inf is the annual inflation rate and unem is the annual unemployment rate.
#######################################################################################

phillips<-read.table("C:/Users/Awesomemin/Desktop/연구아카이브/Proj.Manager/EconometricsWithR/고급계량과제/data/phillips.raw", header=FALSE)
fix(phillips)

year <-phillips[,1] #1948 through 2003
inf <-phillips[,3] #percentage change in CPI 
unem <-phillips[,2] #civilian unemployment rate, %

# reg under Homoskedasticity
reg1.phillips<-lm(inf ~ unem, subset = year<=1996)
summary(reg1.phillips)

#To determine whether there is a tradeoff, on average, between unemployment and inflation, 
#we can test H0: b1 = 0 against H1: b1 < 0

#b1_hat = 0.468 and its p-value against a two-sided alternative is about 0.11
#This equation does not suggest a tradeoff between unem and inf
#There are some problems with this analysis that we will address later.