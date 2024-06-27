# Enviromental Kuznets curve hypothesis

## 데이터 불러오기, 요약통계량 
library(loedata)
data(Ekc)
names(Ekc)
nrow(Ekc)
head(Ekc)
summary(Ekc)

## 시각화
plot(co2pc~gdppcppp, xlim=c(0, 100000), ylim=c(0, 60), data = Ekc)


# Kosis 공무원수와 재정자립도

## 데이터 불러오기
data(Pubserv, package = "loedata")
nrow(Pubserv)
summary(Pubserv)

## 시각화
plot(finind~servpc, data = Pubserv, xlab = "인구 1천명당 공무원수(천명)", 
     ylab = "재정자립도")


# 교육수준과 임금

## 데이터 불러오기 및 구성 파악
data(Klips, package = "loedata")
Klips2 <- subset(Klips, regular==1 & married == 1)
summary(Klips2)
nrow(Klips2)

## 시각화
plot(labinc~educ, data = Klips2, log = 'y')


# 직선 그리기 with OLS

