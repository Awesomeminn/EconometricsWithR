################################################################################################
##### Example 9.8 R&D Intensity and Firm Size ##################################################
##### Model : rdintens = b0 + b1*sales + b2*profmarg + u #######################################
#### Comparison of OLS results with and without ourliers.
################################################################################################

# Nobs : 32
RDCHEM<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\RDCHEM.raw", header=FALSE)

View(RDCHEM)
summary(RDCHEM)

# make variables for reg
rdintens <-RDCHEM[,4]; # rd as percent of sales
sales <-RDCHEM[,2];   # firm sales, millions $
profmarg <-RDCHEM[,5]; # profits as percent of sales

# reg under Homoskedasticity
# Using all observations
reg1.RDCHEM<-lm(rdintens ~ sales + profmarg)
summary(reg1.RDCHEM)
#Neither sales nor profmarg is statistically significant at even the 10% level
#Of the 32 firms, 31 have annual sales less than $20 billion. 
#One firm has annual sales of almost $40 billion.

# plot
plot(rdintens ~ sales, main= "Scatterplot of R&D intensity against firm sales", ylab="R&D as a percentage of sales", xlab="firm sales (in millions of dollars)")
text(x=39709.0, y=3.60, labels="possible outliers", pos=2)
#Figure 9.1 shows how far this firm is from the rest of the sample.
abline(reg1.RDCHEM)

# reg without an outlier
reg2.RDCHEM<-lm(rdintens ~ sales + profmarg, subset=sales<20000)
summary(reg2.RDCHEM)
#the coefficient on sales more than triples, and it now has a t statistic over two.
#there is a statistically significant positive effect between R&D intensity and firm size. 
#The profit margin is still not significant, and its coefficient has not changed by much.
# plot
plot(rdintens ~ sales, main= "Scatterplot of R&D intensity against firm sales", ylab="R&D as a percentage of sales", xlab="firm sales (in millions of dollars)")
abline(reg2.RDCHEM)

#LAD
install.packages("quantreg")
library(quantreg)

lad1.RDCHEM <- rq(rdintens ~ sales + profmarg, tau = .5)
summary(lad1.RDCHEM)

# The confidence intervals are computed by the rank inversion method described in Koenker [2005], Section 3.4.5

#There are several alternative methods of conducting inference about quantile regression coefficients.
summary(lad1.RDCHEM,se = "nid")
#The standard errors reported in this table are computed for the quantile regression sandwich formula, 
  #and using the Hall-Sheather bandwidth rule.
summary(lad1.RDCHEM,se = "ker")
#To obtain the Powell kernel version of the covariance matrix estimate, se="ker"
summary(lad1.RDCHEM,se = "boot")
#To compute bootstrapped standard errors, se="boot".
