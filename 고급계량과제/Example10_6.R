######################################################################################################
##### Example 10.6 Election Outcomes and Economic Performance ##############################################
# Model: demvote = b0 + b1*partyWH + b2*incum + b3*partyWH*gnews + b4*partyWH*inf + u
# To explain presidential election outcomes in terms of economic performance.
#######################################################################################

fair<-read.table("D:\\smchoi\\Teaching\\UnderEconometricsII\\2020Sem2\\RCode\\TextDataFiles\\fair.raw", header=FALSE)
fix(fair)

year <-fair[,1] #1916 to 1996, by 4
demvote <-fair[,2] #the proportion of the two-party vote going to the Democratic candidate.
partyWH <-fair[,3] # =1 if a Democrat is in the White House and −1 if a Republican is in the White House
incum <-fair[,4] #1 if a Democratic incumbent is running, −1 if a Republican incumbent is running, and zero otherwise.
gnews <-fair[,8] #number of quarters, during the administration’s first 15 quarters, 
#                   when the quarterly growth in real per capita output was above 2.9% (at an annual rate)
inf <-fair[,7] #average annual inflation rate over the first 15 quarters of the administration

reg1.fair <- lm(demvote ~ partyWH + incum + I(partyWH*gnews) + I(partyWH*inf), subset= year!=1996)
summary(reg1.fair)

#b3 measures the effect of good economic news on the party in power; we expect b3 > 0.
#b4 measures the effect that inflation (bad news) has on the party in power; we expect b4 < 0.

#Incumbency is worth about 5.4 percentage points in the share of the vote.
#one more quarter of good news is worth about 1.1 percentage points.
#if average annual inflation is, say, two percentage points higher, 
  #the party in power loses about 1.5 percentage points of the two-party vote.



