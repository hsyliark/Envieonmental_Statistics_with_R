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

# Prediction
p <- predict(object, s=6, newdata=X, type="coefficient")
p$coefficient
predicted <- X%*%p$coefficient

# ROC curve
install.packages("Epi")
library(Epi)
predicted <- X%*%p$coefficient
label <- y
ROC(predicted/max(predicted), label/max(label))


## reference : http://www.science.smith.edu/~jcrouser/SDS293/labs/lab10-r.html

install.packages("ISLR")
install.packages("glmnet")
install.packages("dplyr")
install.packages("tidyr")
library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)

grid = 10^seq(10, -2, length = 100)
lasso_mod <- glmnet(X, y, alpha = 1, lambda = grid) # alpha=0 -> ridge / alpha=1 -> lasso
dim(coef(lasso_mod))
plot(lasso_mod)    # Draw plot of coefficients

lasso_mod$lambda[50] #Display 50th lambda value
coef(lasso_mod)[,50] # Display coefficients associated with 50th lambda value
sqrt(sum(coef(lasso_mod)[-1,50]^2)) # Calculate l2 norm

lasso_mod$lambda[60] #Display 60th lambda value
coef(lasso_mod)[,60] # Display coefficients associated with 60th lambda value
sqrt(sum(coef(lasso_mod)[-1,60]^2)) # Calculate l2 norm

predict(lasso_mod, s = 50, type = "coefficients")[1:20,]
