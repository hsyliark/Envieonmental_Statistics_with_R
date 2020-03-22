water <- read.csv("C:/Users/HSY/Desktop/표준화 작업 완료/csv 파일/광산 10년 월평균 자료.csv", sep=",", header=T)
water_scale <- scale(water)
setwd("C:/Users/HSY/Desktop/표준화 자료 요청/표준화 데이터")
write.csv(water_scale, file='광산 10년 월평균 자료_표준화.csv', row.names=F)

## Box-Cox Transformation
# reference : https://otexts.com/fppkr/transformations.html
library(MASS)
library(forecast)
water <- water[1:120,]

# Best Lambda
boxcox_lambda <- NULL
for (i in 1:ncol(water)) {
  (boxcox_lambda[i] <- BoxCox.lambda(water[,i]))
}
boxcox_lambda  

# Transformation
water_boxcox <- data.frame(nrow=nrow(water))
for (i in 1:ncol(water)) {
  newdata <- BoxCox(water[,i], boxcox_lambda[i])
  water_boxcox <- data.frame(water_boxcox, newdata)
}

water_boxcox <- water_boxcox[,-1]
colnames(water_boxcox) <- colnames(water)

# Checking normality
qqnorm(water_boxcox[,1]) ; qqline(water_boxcox[,1])
shapiro.test(water_boxcox[,1])
