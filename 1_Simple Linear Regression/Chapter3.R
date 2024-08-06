# 환경 쿠즈네츠 곡선 가설

library(loedata)
data(Ekc)
View(Ekc)

plot(co2pc ~ gdppcppp, data = Ekc)

# 우리나라 군별 공무원수와 재정자립도 
data(Pubserv)
nrow(Pubserv)
summary(Pubserv)
plot(finind~servpc, data = Pubserv, subset = servpc < 28)

Pubserv1 <- subset(Pubserv, servpc < 28)
plot(finind ~ servpc, data = Pubserv1, pch = 19)
abline(lm(finind ~ servpc, data = Pubserv1), lty = 1)
library(quantreg)
abline(rq(finind ~ servpc, data = Pubserv1), lty = 2)

# 공무원 비율과 재정자립도 OLS
lm(log(finind)~log(servpc), data = Pubserv1)










