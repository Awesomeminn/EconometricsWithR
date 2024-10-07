##################################################################################
##### Example 9.9 R&D Intensity ##################################################
##### Model : log(rd) = b0 + b1*log(sales) + b2*profmarg + u #####################
####Certain functional forms are less sensitive to outlying observations.
##################################################################################

# Nobs : 32
RDCHEM<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\RDCHEM.raw", header=FALSE)

View(RDCHEM)
summary(RDCHEM)

# make variables for reg
rd <-RDCHEM[,1]; # R&D spending, millions $
sales <-RDCHEM[,2];   # firm sales, millions $
profmarg <-RDCHEM[,5]; # profits as percent of sales

# reg under Homoskedasticity
reg1.RDCHEM<-lm(log(rd) ~ log(sales) + profmarg)
summary(reg1.RDCHEM)
# plot
plot(log(rd) ~ log(sales), main= "Scatterplot of log(R&D) against log(sales)", ylab="log(R&D)", xlab="log(firm sales)")
text(x=log(39709.0), y=log(3.60), labels="possible outliers", pos=2)
abline(reg1.RDCHEM)

# reg without the largest firm(outliers)
reg2.RDCHEM<-lm(log(rd) ~ log(sales) + profmarg, subset=sales<20000)
summary(reg2.RDCHEM)

#Practically, these results are the same.
#In neither case do we reject the null H0: b1 = 1 against H1: b1>1

# plot
plot(log(rd) ~ log(sales), main= "Scatterplot of log(R&D) against log(sales)", ylab="log(R&D)", xlab="log(firm sales)")
abline(reg2.RDCHEM)

