###################################################################################################################################################################
##### Example 9.1 Economic Model of Crime #########################################################################################################################
##### Model 1: narr86 = b0 + b1*pcnv + b2*avsen + b3*tottime + b4*ptime86 + b5*qemp86 + b6*inc86 + b7*black + b8*hispan + u #######################################
##### Model 2: narr86 = b0 + b1*pcnv + b2*pcnv^2 + b3*avsen + b4*tottime + b5*ptime86 + b6*ptime86^2 + b7*qemp86 + b8*inc86 + b9*inc86^2 + b7*black + b8*hispan + u
###### Use quadratic terms to detect nonlinearities.
###################################################################################################################################################################

# Nobs : 2,725
CRIME1<-read.table("C:\\smchoi\\R\\TextDataFiles\\CRIME1.raw", header=FALSE)

View(CRIME1)
summary(CRIME1)

# make variables for reg
narr86<-CRIME1[,1]; # the number of arrests in the current year (1986)
pcnv<-CRIME1[,4];   # proportion of prior convictions
avgsen<-CRIME1[,5]; # avg sentence length, mos.
tottime<-CRIME1[,6]; # time in prison since 18 (mos.)
ptime86<-CRIME1[,7];# mos. in prison during 1986
qemp86<-CRIME1[,8]; # # quarters employed, 1986
inc86<-CRIME1[,9];  # legal income, 1986, $100s
black<-CRIME1[,11]; # =1 if black
hispan<-CRIME1[,12];# =1 if Hispanic

# reg under Homoskedasticity
#Without any quadratic terms
reg1.CRIME1<-lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 + black + hispan)
summary(reg1.CRIME1)
#With quadratic terms that are significant in the first regression.
#qemp86 is a discrete variable taking on only five values, so we do not include its square
reg2.CRIME1<-lm(narr86 ~ pcnv + I(pcnv^2) + avgsen + tottime + ptime86 + I(ptime86^2) + qemp86 + inc86 + I(inc86^2) + black + hispan)
summary(reg2.CRIME1)

# F test to test joint significance of quadratic terms
library(car)
lht(reg2.CRIME1, c("I(pcnv^2)=0", "I(ptime86^2)=0", "I(inc86^2)=0"))

#Each of the squared terms is significant, and together they are jointly very significant
# Thus, it appears that the initial model overlooked some potentially important nonlinearities.

