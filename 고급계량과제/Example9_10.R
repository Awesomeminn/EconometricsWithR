##########################################################################################
##### Example 9.10 State Infant Mortality Rates ##########################################
##### Model : infmort90 = b0 + b1*log(pcinc90) + b2*log(physic90) + b3*log(popul90) + u ##
##########################################################################################
#In some cases, certain observations are suspected
   #at the outset of being fundamentally different from the rest of the sample. 
#This often happens when we use data at very aggregated levels, 
   #such as the city, county, or state level.

# Nobs : 102
INFMRT<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\INFMRT.raw", header=FALSE)

View(INFMRT)
summary(INFMRT)

# make variables for reg
infmort <-INFMRT[,2]; # infant mortality rate
#number of deaths within the first year per 1,000 live births
pcinc <-INFMRT[,5];   # per capita income
physic <-INFMRT[,6];   # physicians per 100,000 civilian pop.
popul <-INFMRT[,4]; # population, 1000s
year <-INFMRT[,1]; # 1987 or 1990

# We use the data for the year 1990, and 
#we have all 50 states in the United States, plus the District of Columbia (D.C.).
infmort90 <-subset(infmort, year==1990)
pcinc90 <-subset(pcinc, year==1990)
physic90 <-subset(physic, year==1990)
popul90 <-subset(popul, year==1990)

# reg under Homoskedasticity
# Using all observations
reg1.INFMRT<-lm(infmort90  ~ log(pcinc90) + log(physic90) + log(popul90))
summary(reg1.INFMRT)
#Higher per capita income is estimated to lower infant mortality, an expected result.
#But more physicians per capita is associated with higher infant mortality rates, 
     #something that is counterintuitive.
#Infant mortality rates do not appear to be related to population size.

#The District of Columbia is unusual in that
#it has pockets of extreme poverty and great wealth in a small area.
#the infant mortality rate for D.C. was 20.7, compared with 12.4 for the highest state. 
#It also has 615 physicians per 100,000, compared with 337 for the highest state. 
#The high number of physicians coupled with the high infant mortality rate in D.C. 
#could certainly influence the results.

# reg without unusual point(The District of Columbia)
reg2.INFMRT<-lm(infmort90  ~ log(pcinc90) + log(physic90) + log(popul90), subset=infmort90<20)
summary(reg2.INFMRT)

#We now find that more physicians per capita lowers infant mortality
   #the estimate is statistically different from zero at the 5% level.
#The effect of per capita income has fallen sharply and 
   #is no longer statistically significant.
#infant mortality rates are higher in more populous states, and 
   #the relationship is very statistically significant.
#Also, much more variation in infmort is explained when D.C. is dropped. 
#Clearly, D.C. had substantial influence on the initial estimates, and 
    #we would probably leave it out of any further analysis.


