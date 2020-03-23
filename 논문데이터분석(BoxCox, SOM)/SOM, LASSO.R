water <- read.csv("C:/Users/Nier/Desktop/논문데이터분석/논문데이터분석(BoxCox, SOM)/데이터(표준화 요청)/csv 파일/우치rawdata.csv", sep=",", header=T)
water1 <- water[,c(2,5,9,10,11,13,15,16,17)]

## Box-Cox Transformation
# reference : https://otexts.com/fppkr/transformations.html
library(MASS)
library(forecast)

# Best Lambda
boxcox_lambda <- NULL
for (i in 1:ncol(water1)) {
  (boxcox_lambda[i] <- BoxCox.lambda(water1[,i]))
}
boxcox_lambda  

# Transformation
water1_boxcox <- data.frame(nrow=nrow(water1))
for (i in 1:ncol(water1)) {
  newdata <- BoxCox(water1[,i], boxcox_lambda[i])
  water1_boxcox <- data.frame(water1_boxcox, newdata)
}

water1_boxcox <- water1_boxcox[,-1]
colnames(water1_boxcox) <- colnames(water1)
water1_boxcox_scale <- scale(water1_boxcox)

# Checking normality
qqnorm(water1_boxcox_scale[,2]) ; qqline(water1_boxcox_scale[,2])
shapiro.test(water1_boxcox_scale[,2])


## Lasso Regression

# Install packages
install.packages("lars")
library(lars)

# Variables
colnames(water1_boxcox_scale)

# Lasso (Variation of coefficient with s(threshold))
X <- as.matrix(water1_boxcox_scale[,-5])
y <- as.matrix(water1_boxcox_scale[,5])
object <- lars(X, y, type="lasso")
plot(object) 

# s=4, using 3 variables
coef_lars1 <- coef(object, s=4)
coef_lars1
# s=3, using 2 variables
coef_lars2 <- coef(object, s=3)
coef_lars2

# Select regression model with Cp
plot(object, plottype="Cp")
round(object$beta, 4)
coef(object, s=11)

# Calculate R-square
beta_list <- predict(object, s=16, newdata=X, type="coefficients")
beta_hat <- beta_list$coefficients
predicted <- X%*%beta_hat
predicted
mean(y)
SST <- sum((y-mean(y))**2)
SSE <- sum((y-predicted)**2)
R_square <- 1-(SSE/SST)

# reference : https://rpago.tistory.com/59
install.packages("glmnet")
library(glmnet)
X <- as.matrix(water1_boxcox_scale[,-7])
y <- as.matrix(water1_boxcox_scale[,7])
sh <- 10^seq(10,-2,length=100)
lasso <- glmnet(X, y, alpha=1, lambda=sh)
dim(coef(lasso))
plot(lasso)

set.seed(1)
cv.lasso <- cv.glmnet(X, y, alpha=1)
plot(cv.lasso)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

best.lasso <- glmnet(X, y, alpha=1, lambda=sh)
predict(best.lasso, s=bestlam.lasso, type="coefficients")


# Elastic-net regression
# reference : https://daviddalpiaz.github.io/r4sl/elastic-net.html
install.packages("caret")
library(caret)
set.seed(42)
cv_10 = trainControl(method = "cv", number = 10)
hit_elnet = train(COD ~ ., data = water_scale, method = "glmnet", trControl = cv_10)
hit_elnet
plot(hit_elnet)

sh <- 10^seq(10,-2,length=100)
X <- as.matrix(water_scale[,-6])
y <- as.matrix(water_scale[,6])
best.elastic <- glmnet(X, y, alpha=1, lambda=sh)
predict(best.elastic, s=0.01781786, type="coefficients")
