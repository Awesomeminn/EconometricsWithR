##################################################################################################################
##### Example 10.5 Antidumping Filings and Chemical Imports  #####################################################
##### Model 1 : log(chnimp) = b0 + b1*log(chempi) + b2*log(gas) + b3*log(rtwex) + b4*befile6 + b5*afdec6 + u  ####
##################################################################################################################
#Krupp and Pollard (1996) analyzed the effects of antidumping filings by U.S. chemical industries on imports of various chemicals.
#We focus here on one industrial chemical, barium chloride, 
  #a cleaning agent used in various chemical processes and in gasoline production.
#In the early 1980s, the barium chloride industry filed a complaint against China 
#with the U.S. International Trade Commission (ITC) in October 1983.
#The ITC ruled in favor of the U.S. barium chloride industry in October 1984.

#Some questions to ask:
#1. Were imports unusually high in the period immediately preceding the initial filing?
#2. Did imports change noticeably after an antidumping filing? 
#3. Finally, what was the reduction in imports after a decision in favor of the U.S. industry?
# Nobs : 131
BARIUM<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\BARIUM.raw", header=FALSE)

View(BARIUM)
summary(BARIUM)

#Using monthly data from February 1978 through December 1988
# make variables for reg
#chnimp <-BARIUM[,1]; # the volume of imports of barium chloride from China
lchnimp <-BARIUM[,15]; # log(chnimp)
#chempi <-BARIUM[,9];   # chemical production index (=100 in 1977) (to control for overall demand for barium chloride)
lchempi <-BARIUM[,18];   # log(chempi)
#gas <-BARIUM[,10]; # gasoline production (another demand variable)
lgas <-BARIUM[,16]; # log(gas)
#rtwex <-BARIUM[,11]; # exchange rate index (measures the strength of the dollar against several other currencies.)
lrtwex <-BARIUM[,17]; # log(rtwex)
befile6 <-BARIUM[,3]; # =1 for all 6 months before filing
affile6 <-BARIUM[,4]; # =1 for all 6 months after filing
afdec6 <-BARIUM[,5]; # =1 for all 6 months after decision

# reg under Homoskedasticity
reg1.BARIUM<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6)
summary(reg1.BARIUM)

########## Results ##########
#befile6 is statistically insignificant
  #no evidence that Chinese imports were unusually high during the six months before the suit was filed.
#affile6 is negative, the coefficient is small, and it is statistically very insignificant.
#afdec6 shows a substantial fall in Chinese imports of barium chloride after the decision

# the exact percentage change in Chinese export of barium is
100*(exp(reg1.BARIUM$coefficients[7])-1)

#an increase in overall chemical production increases the demand for the cleaning agent.
#Gasoline production does not affect Chinese imports significantly.
#The coefficient on log(rtwex) shows that 
  #an increase in the value of the dollar relative to other currencies increases the demand for Chinese imports.

#We used monthly data that have not been seasonally adjusted.
#########################################################################################################################################
##### Example 10.11 Effects of Antidumping Filings  #####################################################################################
##### Model 2 : log(chnimp) = b0 + b1*log(chempi) + b2*log(gas) + b3*log(rtwex) + b4*befile6 + b5*afdec6 + d1*feb+ ... + d11*dec + u  ###
#########################################################################################################################################

# make variables for reg with monthly dummies

feb <-BARIUM[,20]; # =1 if month is feb
mar <-BARIUM[,21]; # =1 if month is march
apr <-BARIUM[,22]; # =1 if month is apr
may <-BARIUM[,23]; # =1 if month is may
jun <-BARIUM[,24]; # =1 if month is jun
jul <-BARIUM[,25]; # =1 if month is jul
aug <-BARIUM[,26]; # =1 if month is aug
sep <-BARIUM[,27]; # =1 if month is sep
oct <-BARIUM[,28]; # =1 if month is oct
nov <-BARIUM[,29]; # =1 if month is nov
dec <-BARIUM[,30]; # =1 if month is dec

# reg with monthly dummies under Homoskedasticity
reg2.BARIUM<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 + 
			feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec)
summary(reg2.BARIUM)

library(car)
lht(reg2.BARIUM, c("feb = 0","mar=0","apr=0","may=0","jun=0","jul=0","aug=0","sep=0","oct=0","nov=0","dec=0"))
#the seasonal dummies are jointly insignificant.
#nothing important changes in the other estimates