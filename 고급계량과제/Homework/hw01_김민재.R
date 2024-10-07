# C1
## 데이터 불러오기 
data <- read.table("CEOSAL1.raw", header = FALSE)
names(data) <- c('salary', 'pcsalary', 'sales', 'roe', 'pcroe',
'ros', 'indus', 'finance', 'consprod', 'utility', 'lsalry',
'lsales')
## 더미변수 추가 
data2 <-transform(data, rosneg = ifelse(ros < 0, 1, 0))
lsalry = data2$lsalry
lsales = data2$lsales
roe = data2$roe
ros = data2$ros  
rosneg = data2$rosneg

## Regression Modeling
reg <- lm(lsalry ~ lsales + roe + rosneg)
summary(reg)

## RESET
library(lmtest)
resettest(reg)

## 이분산 고려하고, RESET
coeftest(reg, vcov = vcovHC)
resettest(reg, vcov = vcovHC)

# C2
data <- read.table('WAGE2.RAW', header = F)
names(data) <- c('wage', 'hours', 'IQ', 'KWW', 'educ',
'exper', 'tenure', 'age', 'married', 'black', 'south', 'urban',
'sibs', 'brthord', 'meduc', 'feduc', 'lwage')
reg <- lm(lwage ~ educ + exper + tenure + married + south
+ urban + black + KWW, data = data)
summary(reg)

reg2 <- lm(lwage ~ educ + exper + tenure + married + south
+ urban + black + KWW + IQ, data = data)
summary(reg2)


# 마지막 문제 
data0 = read.table('elem94_95.raw', header=F)
View(data0)
names(data0) = c('distid', 'schid', 'lunch', 'enrol', 'staff', 'exppp', 'avgsal', 'avgben', 'math4', 
                 'story4', 'bs', 'lavgsal', 'lenrol', 'lstaff')

## (i)
reg1 <-  lm(lavgsal ~ bs + lenrol + lstaff + lunch, data = data0)
summary(reg1)
coeftest(reg1, vcov = hccm(reg1, 'hc0'))


## (ii)
reg2 <-  lm(lavgsal ~ bs + lenrol + lstaff + lunch, data = data0, subset = bs <= 0.5)
summary(reg2)
coeftest(reg2, vcov = hccm(reg2, 'hc0'))

## (iii)
data2 <- transform(data0,
                  d1508 = ifelse(bs > 0.99, 1, 0),
                  d68 = ifelse(bs > 0.65 & bs < 0.66, 1, 0),
                  d1127 = ifelse(bs > 0.57 & bs < 0.58, 1, 0),
                  d1670 = ifelse(bs > 0.5 & bs < 0.51, 1, 0))
View(data2)
reg3 <-  lm(lavgsal ~ bs + lenrol + lstaff + lunch + d1508 + d68 + d1127 + d1670, data = data2)
summary(reg3)

## (v)
reg4 <-  lm(lavgsal ~ bs + lenrol + lstaff + lunch, data = data0, subset = bs <= 0.99)
summary(reg4)

## (vi)
library(quantreg)
lad1 <-  rq(lavgsal ~ bs + lenrol + lstaff + lunch, data = data0, subset = bs <= 0.99)
lad2 <-  rq(lavgsal ~ bs + lenrol + lstaff + lunch, data = data0)
summary(lad1)
summary(lad2)












