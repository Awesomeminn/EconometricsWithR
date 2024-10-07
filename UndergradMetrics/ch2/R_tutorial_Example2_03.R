sink("C:\\Users\\UOS\\Desktop\\RCode\\Chap02\\example2_3.txt", append = F)

##################################################
# Let us create an R script file for Example 2.3
##################################################
#Anything that follows a # on the command line is taken as comment and ignored by R.

# set the working directory to "" 
# You have to use "/" or "\\" not "\"
setwd("C:\\Users\\UOS\\Desktop/RCode") #setting your working directory    

#loading data for Example 2.3 (The population of chief executive officers (CEO))
#209 CEOs for the year 1990.

ceosal1<-read.table("C:\\Users\\UOS\\Desktop\\RCode\\TextDataFiles\\ceosal1.raw", header=F) 
#The <- is a left diamond bracket (<) followed by a minus sign (-). It means ceosal1 data set is assigned to ceosal1.
# You can use = instead of <-
#F (T) can be used for FALSE (TRUE)
# Use T when the first line of the data file has header information for the columns.

#to learn more about the data
str(ceosal1) #Returns variable name, type, first few observations
names(ceosal1) #Returns only names of all variables
nrow(ceosal1) #Returns the number of rows (observations)
ncol(ceosal1) #Returns the number of columns (variables)
head(ceosal1) #Returns variable names and first few observations

#View(ceosal1) #just can see the data
#fix(ceosal1) #shows data like in an excel spread sheet and we can make changes.
summary(ceosal1)#Returns summary statistics of data

#You can also edit directly from this sheet
ceosal1 #to see all data

#To clear R Console press Ctl + l

#Linear Regression by using lm function
#?lm #or help(lm) #to learn more about lm function

###########Model 1######################
# salary = b0 + b1*roe + u
######################################

#name variables

#reg1.ceosal1<-lm(salary~roe, data=ceosal1)
reg1.ceosal1<-lm(V1~V4, data=ceosal1)
summary(reg1.ceosal1)
reg1.ceosal1<-lm(ceosal1$V1~ceosal1$V4) #No variable names
#summary(reg1.ceosal1)

salary<-ceosal1[,1] #annual salary in thousands of dollars
roe<-ceosal1[,4] #the average return on equity (roe) for the CEO's firm for the previous 3 years (percentage)

#run a simple regression under homoskedasticity to study relationship between salary and roe
reg1.ceosal1<-lm(salary~roe)

summary(reg1.ceosal1)

#To start exporting a graph

#jpeg("C:\\Users\\UOS\\Desktop\\RCode\\Chap02\\olsgraph.jpg") #to starting a graph
#For pdf 
#pdf("C:\\Users\\UOS\\Desktop\\RCode\\Chap02\\olsgraph.pdf")
#For bmp
bmp("C:\\Users\\UOS\\Desktop\\RCode\\Chap02\\olsgraph.bmp")

#plot(roe,salary) 
plot(salary~roe) #Do the same thing as above
abline(reg1.ceosal1)
#abline(reg1.ceosal1$coefficients) #Do the same thing as above

graphics.off() #to turn off exporting a graph

#when the roe increases by 1 percentage point, then salary is predicted to go up by about 18.5, or $18,500 ceteris paribus.
#roe=30, Then salary = 963.191 + 18.501(30) = 1,518,221.

###########Model 2######################
#salary = b1*roe + u
#without intercept 
######################################

reg2.ceosal1<-lm(salary~0+roe) #-1+roe will do the same thing
summary(reg2.ceosal1)


sink()
