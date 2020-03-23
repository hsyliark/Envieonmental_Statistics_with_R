## Box-Cox Transformation
# reference : https://otexts.com/fppkr/transformations.html
library(MASS)
library(forecast)

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
