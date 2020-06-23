water <- read.csv("C:/Users/Nier/Desktop/³í¹®µ¥ÀÌÅÍºĞ¼®(LASSO regression)_¿¹ÃøÆ÷ÇÔ/ÀÚ·á, °á°úÁ¤¸®/PCA¿¡ Rain, Flow Æ÷ÇÔ/csvÆÄÀÏ/10_ÇÔÆò.csv", sep=",", header=T)
water1 <- water
water_scale <- scale(water)
water1_scale <- scale(water1)


water1 <- water[,-(10:11)]
water_scale <- scale(water)
water1_scale <- scale(water1) # PCA, SOM

## Correlation analysis

install.packages('corrplot') 
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)
X <- round(cor(water_scale, method='spearman'),4)
pairs(X) # Correlation plot 1
corrplot(X) # Correlation plot 2
X # Correlation matrix
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(water_scale, method='spearman')
p.mat

# Using ggplot2, ggcorrplot
# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(X, hc.order=T, type="lower", lab=T) +
  ggtitle("Correlation plot(Spearman) for water quality data(all site)") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(X, hc.order=T, type="lower", p.mat=p.mat) +
  ggtitle("Correlation plot(Spearman) for water quality data(all site) with significance level") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))


## Principal Component Analysis

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)
water <- read.csv("C:/Users/Nier/Desktop/?ï¿½ï¿½ï¿??????ÍºĞ¼?/?ï¿½ï¿½ï¿??????ÍºĞ¼?(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/?????????Ú·?/??Ã¼??ï¿½ï¿½.csv", header=T, sep=',')
water <- water[,-11]
water <- water[,-14]
water <- water[,-(18:19)]
water_scale <- scale(water)
water <- water[,-(1:4)]
water <- water[,-(10:11)]
water_scale_1 <- scale(water)

# Descriptive statistics
describe(water1_scale)

# Correlation
round(cor(water1_scale),3) # pearson
round(cor(water1_scale, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(water1_scale)
cortest.bartlett(cor(water1_scale, method="spearman"), n=nrow(water1_scale))

# Number of principal components 
water_pca <- prcomp(water1_scale, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

# Component matrix 
PCA <- principal(water1_scale, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(water1_scale, nfactor=2, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot, main="Biplot")



## normalization
water_scale <- scale(water)



## Stepwise Regression

# Linear regression model
linearmod <- lm(BOD ~ ., data=water_scale)
linearmod # coefficients
summary(linearmod)

step(lm(BOD ~ ., data=water_scale), direction="both") # Stepwise Regression
step(lm(BOD ~ ., data=water_scale), direction="backward") # Backward Elimination
step(lm(BOD ~ ., data=water_scale), direction="forward") # Forward Selection



## Lasso Regression

# Install packages
install.packages("lars")
library(lars)

# Variables
colnames(water_scale)

# Lasso (Variation of coefficient with s(threshold))
X <- as.matrix(water_scale[,-2])
y <- as.matrix(water_scale[,2])
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


# reference : https://niceguy1575.tistory.com/64
X <- as.matrix(water_scale[,-8])
y <- as.matrix(water_scale[,8])

install.packages("glmnet")
library(glmnet)
install.packages("plotmo")
library(plotmo)
set.seed(1)
mod <- glmnet(X, y)
cvfit <- cv.glmnet(X, y)
glmcoef <- coef(mod, cvfit$lambda.min)
coef.increase <- dimnames(glmcoef[glmcoef[,1]>0, 0])[[1]]
coef.decrease <- dimnames(glmcoef[glmcoef[,1]<0, 0])[[1]]
allnames <- names(coef(mod)[, ncol(coef(mod))][order(coef(mod)[, ncol(coef(mod))], decreasing=T)])
allnames <- setdiff(allnames, allnames[grep("Intercept", allnames)])
cols <- rep("blue", length(allnames))
cols[allnames %in% coef.increase] <- "darkgreen"
cols[allnames %in% coef.decrease] <- "red"
plot_glmnet(mod, label=T, s=cvfit$lambda.min, col=cols)
glmcoef


# LASSO coefficient with name
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)

set.seed(1)
lasso_model <- glmnet(X, y, alpha=1)
beta <- coef(lasso_model)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id="coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- log(lasso_model$lambda[tmp$variable+1])
tmp$norm <- apply(abs(beta[-1,]), 2, sum)
ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color=coef)) +
  geom_line(size=1) +
  xlab("Lambda (log scale)") +
  ylab("Coefficients") +
  guides(color=guide_legend(title=""),
         linetype=guide_legend(title="")) +
  theme_bw() +
  theme(legend.key.width = unit(3, "lines"))

# predict
X <- as.matrix(water_scale[,-1])
y <- as.matrix(water_scale[,1])
install.packages("glmnet")
library(glmnet)
set.seed(1)
cv.lasso <- cv.glmnet(X, y, alpha=1)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
pred_y <- predict(cv.lasso, newx=X, s="lambda.min")
plot(y, pch=20, main="test(black) v.s predict(red)")
lines(pred_y, col="red")

# NSE, Bias, MAE
install.packages("hydroGOF")
library(hydroGOF)
my_bias <- function(sim, obs) {
  bias = sum(sim-obs)/length(sim)
  return(bias)
}
obs <- y
sim <- pred_y
NSE(sim, obs)
my_bias(sim, obs)
mae(sim, obs)




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



## Granger causality test
# reference : http://intothedata.com/02.scholar_category/timeseries_analysis/granger_causality/

# Install packages
install.packages("lmtest")
library(lmtest)

# Drawing graph
par(mfrow=c(1,2))
attach(water1_scale)
plot.ts(pH, main="????(pH)")
plot.ts(BOD, main="????(BOD)")
par(mfrow=c(1,1))

# Install packages
install.packages("forecast")
require(forecast)

# KPSS test (Finding difference for stationarity)
ndiffs(water1_scale$pH, alpha=0.05, test=c("kpss")) # 1
ndiffs(water1_scale$BOD, alpha=0.05, test=c("kpss")) # 1

# difference
diff_pH <- diff(water1_scale$pH, 1)
diff_BOD <- diff(water1_scale$BOD, 1)
par(mfrow=c(1,2))
plot.ts(diff_pH, main="????(pH) ????1 ????")
plot.ts(diff_BOD, main="????(BOD) ????1 ????")
par(mfrow=c(1,1))

grangertest(diff_BOD ~ diff_pH, order=1)
# result ~ cause
