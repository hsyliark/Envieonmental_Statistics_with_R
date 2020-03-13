## Reading file
data_wq <- read.csv("D:/수질데이터분석(논문)/통계관련자료/우치.csv", sep=",", header=T)


## Lasso Regression

# Install packages
install.packages("lars")
library(lars)

# Variables
colnames(data_wq)

# Linear regression model
linearmod <- lm(COD ~ ., data=data_wq)
linearmod # coefficients
summary(linearmod)

# Lasso (Variation of coefficient with s(threshold))
X <- as.matrix(data_wq[,-6])
y <- as.matrix(data_wq[,6])
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
coef(object, s=6)
