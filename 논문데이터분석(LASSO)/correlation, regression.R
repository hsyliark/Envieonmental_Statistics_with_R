### Reference : http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

## Import data
water5 <- read.csv("C:/Users/Nier/Desktop/수질데이터분석(논문)/2번째/분석자료 송부/5year/csv 파일/0.전체지점(5년).csv", header=T, sep=',')
water5_1 <- water5[,-(1:3)]
water5_1$logTC <- log(water5_1$TC)
water5_1$logFC <- log(water5_1$FC)
water5_1$logRain <- log(water5_1$Rain)
water5_1 <- water5_1[,-11]
water5_1 <- water5_1[,-17]
water5_1 <- water5_1[,-17] # water5_1 : 원데이터 (TC, FC, Rain 에 log 취함, 기초통계량 산정)
water5_2 <- scale(water5_1) # water5_2 : 표준화 (상관분석)
water5_3 <- cbind(water5_2[,-(17:18)],water5_1[,17:18]) # water5_3 : 표준화 (logTC, logFC 제외, 회귀분석)


# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)
# Descriptive statistics
describe(water5_1)


## Correlation analysis

install.packages('corrplot') 
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)
X <- round(cor(water5_2),4)
pairs(X) # Correlation plot 1
corrplot(X) # Correlation plot 2
X # Correlation matrix
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(water5_2)
p.mat

## Using ggplot2, ggcorrplot
ggcorrplot(X) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# method = "square" (default)
ggcorrplot(X, method = "circle") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# method = "circle"

# Reordering the correlation matrix
# using hierarchical clustering
ggcorrplot(X, hc.order=T, outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Types of correlogram layout
# Get the lower triangle
ggcorrplot(X, hc.order=T, type="lower", outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Get the upper triangle
ggcorrplot(X, hc.order=T, type="upper", outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(X, hc.order=T, type="lower",
           outline.col="white", ggtheme=ggplot2::theme_gray,
           colors=c("#6D9EC1", "white", "#E46726")) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(X, hc.order=T, type="lower", lab=T) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(X, hc.order=T, type="lower", p.mat=p.mat) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Leave blank on no significant coefficient
ggcorrplot(X, p.mat=p.mat, hc.order=T, type="lower", insig="blank") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))


---------------------------------------------------------------------------------------

## Regression analysis
# reference : http://r-statistics.co/Linear-Regression.html

head(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="blue")

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(linearMod)  # AIC => 419.1569
BIC(linearMod)  # BIC => 424.8929

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

library(DAAG)
cvResults <- suppressWarnings(CVlm(df=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error


## Stepwise
step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="both")
step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="backward")
step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="forward")





