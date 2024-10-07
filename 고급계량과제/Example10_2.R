######################################################################################################
##### Example 10.2 Effects of Inflation and Deficits on Interest Rates ##############################################
# Model: i3 = b0 + b1*inf + b2*def + u
#######################################################################################

intdef<-read.table("intdef.raw", header=FALSE)
fix(intdef)

i3 <-intdef[,2] #three-month T-bill rate
inf <-intdef[,3] # annual inflation rate based on the consumer price index
def <-intdef[,6] #federal budget deficit as a percentage of GDP.

# reg under Homoskedasticity
reg1.intdef <- lm(i3 ~ inf + def)
summary(reg1.intdef)

#increases in inflation or the relative size of the deficit increase short-term interest rates
#Both inf and def are very statistically significant, assuming that the CLM assumptions hold.

