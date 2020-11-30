ex1 <- read.csv("C:/Users/HSY/Desktop/광산(2010-2019).csv", sep=",", header=T)
ex1 <- ex1[,-1]
ex1 <- as.data.frame(ex1)

ex2 <- read.csv("C:/Users/HSY/Desktop/우치(2010-2019).csv", sep=",", header=T)
ex2 <- ex2[,-1]
ex2 <- as.data.frame(ex2)

library(tidyverse)
library(mgcv)
library(ggplot2)
library(gridExtra)
library(moonBook)
library(ztable)
library(survival)
library(ggGam)
library(corrplot)
library(ggcorrplot)
library(car)
library(lmtest)
library(fda)
library(refund)
library(MASS)
library(boot)
library(kohonen)
library(SOMbrero)





## Out-Of-Bag crossvalidation

# 광산 TC

TC1.RMSE.mlr <- c()
TC1.RMSE.glm <- c()
TC1.RMSE.gam <- c()
TC1.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  # Multiple Linear Regression
  fit <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
             +NH3N+PO4P+log(FC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$TC,fitted_values=pred.mlr,
                         time=test$time)
  TC1.RMSE.mlr <- c(TC1.RMSE.mlr,
                sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                       length(data.mlr$response)))
  print(c('TC1.MLR',i))
  # Generalized Linear Model
  m <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
             NH3N+PO4P+log(FC)+Flow+Rain,data=train,
           family=poisson(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$TC,fitted_values=pred.glm,
                         time=test$time)
  TC1.RMSE.glm <- c(TC1.RMSE.glm,
                sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                       length(data.glm$response)))
  print(c('TC1.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(TC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                   +s(log(FC))+s(Flow)+s(Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$TC,fitted_values=pred.gam,
                         time=test$time)
  TC1.RMSE.gam <- c(TC1.RMSE.gam,
                sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                       length(data.gam$response)))
  print(c('TC1.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(TC~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(FC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$TC,fitted_values=pred.tvcm,
                          time=test$time)
  TC1.RMSE.tvcm <- c(TC1.RMSE.tvcm,
                 sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                        length(data.tvcm$response)))
  print(c('TC1.TVCM',i))
}
TC1.RMSE <- data.frame(RMSE=c(TC1.RMSE.mlr,TC1.RMSE.glm,
                              TC1.RMSE.gam,TC1.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(TC1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan TC")


# 광산 FC

FC1.RMSE.mlr <- c()
FC1.RMSE.glm <- c()
FC1.RMSE.gam <- c()
FC1.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  # Multiple Linear Regression
  fit <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
             +NH3N+PO4P+log(TC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$FC,fitted_values=pred.mlr,
                         time=test$time)
  FC1.RMSE.mlr <- c(FC1.RMSE.mlr,
                sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                       length(data.mlr$response)))
  print(c('FC1.MLR',i))
  # Generalized Linear Model
  m <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
             NH3N+PO4P+log(TC)+Flow+Rain,data=train,
           family=poisson(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$FC,fitted_values=pred.glm,
                         time=test$time)
  FC1.RMSE.glm <- c(FC1.RMSE.glm,
                sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                       length(data.glm$response)))
  print(c('FC1.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(FC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                   +s(log(TC))+s(Flow)+s(Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$FC,fitted_values=pred.gam,
                         time=test$time)
  FC1.RMSE.gam <- c(FC1.RMSE.gam,
                sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                       length(data.gam$response)))
  print(c('FC1.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(FC~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$FC,fitted_values=pred.tvcm,
                          time=test$time)
  FC1.RMSE.tvcm <- c(FC1.RMSE.tvcm,
                 sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                        length(data.tvcm$response)))
  print(c('FC1.TVCM',i))
}
FC1.RMSE <- data.frame(RMSE=c(FC1.RMSE.mlr,FC1.RMSE.glm,
                              FC1.RMSE.gam,FC1.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(FC1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan FC")


# 광산 Chla

Chla1.RMSE.mlr <- c()
Chla1.RMSE.glm <- c()
Chla1.RMSE.gam <- c()
Chla1.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  # Multiple Linear Regression
  fit <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)
             +NH3N+PO4P+log(TC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.mlr,
                         time=test$time)
  Chla1.RMSE.mlr <- c(Chla1.RMSE.mlr,
                    sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                           length(data.mlr$response)))
  print(c('Chla1.MLR',i))
  # Generalized Linear Model
  m <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)+
             NH3N+PO4P+log(TC)+Flow+Rain,data=train,
           family=Gamma(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.glm,
                         time=test$time)
  Chla1.RMSE.glm <- c(Chla1.RMSE.glm,
                    sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                           length(data.glm$response)))
  print(c('Chla1.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(Chla~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(FC))+s(NH3N)+s(PO4P)
                   +s(log(TC))+s(Flow)+s(Rain),data=train,
                   family=quasi(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$Chla,fitted_values=pred.gam,
                         time=test$time)
  Chla1.RMSE.gam <- c(Chla1.RMSE.gam,
                    sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                           length(data.gam$response)))
  print(c('Chla1.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(Chla~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(FC))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasi(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$Chla,fitted_values=pred.tvcm,
                          time=test$time)
  Chla1.RMSE.tvcm <- c(Chla1.RMSE.tvcm,
                     sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                            length(data.tvcm$response)))
  print(c('Chla1.TVCM',i))
}
Chla1.RMSE <- data.frame(RMSE=c(Chla1.RMSE.mlr,Chla1.RMSE.glm,
                                Chla1.RMSE.gam,Chla1.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan Chla")
  
  


# 우치 TC

TC2.RMSE.mlr <- c()
TC2.RMSE.glm <- c()
TC2.RMSE.gam <- c()
TC2.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  # Multiple Linear Regression
  fit <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
             +NH3N+PO4P+log(FC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$TC,fitted_values=pred.mlr,
                         time=test$time)
  TC2.RMSE.mlr <- c(TC2.RMSE.mlr,
                    sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                           length(data.mlr$response)))
  print(c('TC2.MLR',i))
  # Generalized Linear Model
  m <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
             NH3N+PO4P+log(FC)+Flow+Rain,data=train,
           family=poisson(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$TC,fitted_values=pred.glm,
                         time=test$time)
  TC2.RMSE.glm <- c(TC2.RMSE.glm,
                    sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                           length(data.glm$response)))
  print(c('TC2.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(TC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                   +s(log(FC))+s(Flow)+s(Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$TC,fitted_values=pred.gam,
                         time=test$time)
  TC2.RMSE.gam <- c(TC2.RMSE.gam,
                    sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                           length(data.gam$response)))
  print(c('TC2.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(TC~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(FC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$TC,fitted_values=pred.tvcm,
                          time=test$time)
  TC2.RMSE.tvcm <- c(TC2.RMSE.tvcm,
                     sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                            length(data.tvcm$response)))
  print(c('TC2.TVCM',i))
}
TC2.RMSE <- data.frame(RMSE=c(TC2.RMSE.mlr,TC2.RMSE.glm,
                              TC2.RMSE.gam,TC2.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(TC2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi TC")


# 우치 FC

FC2.RMSE.mlr <- c()
FC2.RMSE.glm <- c()
FC2.RMSE.gam <- c()
FC2.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  # Multiple Linear Regression
  fit <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
             +NH3N+PO4P+log(TC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$FC,fitted_values=pred.mlr,
                         time=test$time)
  FC2.RMSE.mlr <- c(FC2.RMSE.mlr,
                    sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                           length(data.mlr$response)))
  print(c('FC2.MLR',i))
  # Generalized Linear Model
  m <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
             NH3N+PO4P+log(TC)+Flow+Rain,data=train,
           family=poisson(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$FC,fitted_values=pred.glm,
                         time=test$time)
  FC2.RMSE.glm <- c(FC2.RMSE.glm,
                    sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                           length(data.glm$response)))
  print(c('FC2.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(FC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                   +s(log(TC))+s(Flow)+s(Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$FC,fitted_values=pred.gam,
                         time=test$time)
  FC2.RMSE.gam <- c(FC2.RMSE.gam,
                    sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                           length(data.gam$response)))
  print(c('FC2.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(FC~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasipoisson(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$FC,fitted_values=pred.tvcm,
                          time=test$time)
  FC2.RMSE.tvcm <- c(FC2.RMSE.tvcm,
                     sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                            length(data.tvcm$response)))
  print(c('FC2.TVCM',i))
}
FC2.RMSE <- data.frame(RMSE=c(FC2.RMSE.mlr,FC2.RMSE.glm,
                              FC2.RMSE.gam,FC2.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(FC2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi FC")


# 광산 Chla

Chla2.RMSE.mlr <- c()
Chla2.RMSE.glm <- c()
Chla2.RMSE.gam <- c()
Chla2.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  # Multiple Linear Regression
  fit <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)
             +NH3N+PO4P+log(TC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.mlr,
                         time=test$time)
  Chla2.RMSE.mlr <- c(Chla2.RMSE.mlr,
                      sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                             length(data.mlr$response)))
  print(c('Chla2.MLR',i))
  # Generalized Linear Model
  m <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)+
             NH3N+PO4P+log(TC)+Flow+Rain,data=train,
           family=Gamma(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.glm,
                         time=test$time)
  Chla2.RMSE.glm <- c(Chla2.RMSE.glm,
                      sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                             length(data.glm$response)))
  print(c('Chla2.GLM',i))
  # Generalized Additive Model
  mm.shrink <- gam(Chla~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                   +s(TOC)+s(WT)+s(EC)+s(log(FC))+s(NH3N)+s(PO4P)
                   +s(log(TC))+s(Flow)+s(Rain),data=train,
                   family=quasi(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.gam <- predict(mm.shrink,newdata=test,type="response")
  data.gam <- data.frame(response=test$Chla,fitted_values=pred.gam,
                         time=test$time)
  Chla2.RMSE.gam <- c(Chla2.RMSE.gam,
                      sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                             length(data.gam$response)))
  print(c('Chla2.GAM',i))
  # Time Varying Coefficient Model
  vc.shrink <- gam(Chla~s(time)+s(time,by=pH)+s(time,by=DO)+
                     s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                     s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                     s(time,by=WT)+s(time,by=EC)+s(time,by=log(FC))+
                     s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                     s(time,by=Flow)+s(time,by=Rain),data=train,
                   family=quasi(link="log"),method="GCV.Cp",
                   select=TRUE)
  pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
  data.tvcm <- data.frame(response=test$Chla,fitted_values=pred.tvcm,
                          time=test$time)
  Chla2.RMSE.tvcm <- c(Chla2.RMSE.tvcm,
                       sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                              length(data.tvcm$response)))
  print(c('Chla2.TVCM',i))
}
Chla2.RMSE <- data.frame(RMSE=c(Chla2.RMSE.mlr,Chla2.RMSE.glm,
                                Chla2.RMSE.gam,Chla2.RMSE.tvcm),
                         model=c(rep("1_MLR",10),rep("2_GLM",10),
                                 rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi Chla")






## Out-Of-Bag crossvalidation with Bagging



# 광산 TC

TC1.Bag.RMSE.mlr <- c()
TC1.Bag.RMSE.glm <- c()
TC1.Bag.RMSE.gam <- c()
TC1.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
               +NH3N+PO4P+log(FC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
               NH3N+PO4P+log(FC)+Flow+Rain,data=train.bag,
             family=poisson(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(TC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                     +s(log(FC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(TC~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(FC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$TC,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  TC1.Bag.RMSE.mlr <- c(TC1.Bag.RMSE.mlr,
                    sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                           length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$TC,fitted_values=pred.bag.glm.final,
                         time=test$time)
  TC1.Bag.RMSE.glm <- c(TC1.Bag.RMSE.glm,
                        sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                               length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$TC,fitted_values=pred.bag.gam.final,
                         time=test$time)
  TC1.Bag.RMSE.gam <- c(TC1.Bag.RMSE.gam,
                        sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                               length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$TC,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  TC1.Bag.RMSE.tvcm <- c(TC1.Bag.RMSE.tvcm,
                        sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                               length(data.tvcm$response)))
  print('TC1.bag',i)
}
TC1.Bag.RMSE <- data.frame(RMSE=c(TC1.Bag.RMSE.mlr,TC1.Bag.RMSE.glm,
                              TC1.Bag.RMSE.gam,TC1.Bag.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(TC1.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan TC (Bagging)")


# 광산 FC

FC1.Bag.RMSE.mlr <- c()
FC1.Bag.RMSE.glm <- c()
FC1.Bag.RMSE.gam <- c()
FC1.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
               +NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
               NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
             family=poisson(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(FC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                     +s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(FC~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$FC,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  FC1.Bag.RMSE.mlr <- c(FC1.Bag.RMSE.mlr,
                        sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$FC,fitted_values=pred.bag.glm.final,
                         time=test$time)
  FC1.Bag.RMSE.glm <- c(FC1.Bag.RMSE.glm,
                        sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                               length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$FC,fitted_values=pred.bag.gam.final,
                         time=test$time)
  FC1.Bag.RMSE.gam <- c(FC1.Bag.RMSE.gam,
                        sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                               length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$FC,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  FC1.Bag.RMSE.tvcm <- c(FC1.Bag.RMSE.tvcm,
                         sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                                length(data.tvcm$response)))
  print('FC1.bag',i)
}
FC1.Bag.RMSE <- data.frame(RMSE=c(FC1.Bag.RMSE.mlr,FC1.Bag.RMSE.glm,
                                  FC1.Bag.RMSE.gam,FC1.Bag.RMSE.tvcm),
                           model=c(rep("1_MLR",10),rep("2_GLM",10),
                                   rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(FC1.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan FC (Bagging)")


# 광산 Chla

Chla1.Bag.RMSE.mlr <- c()
Chla1.Bag.RMSE.glm <- c()
Chla1.Bag.RMSE.gam <- c()
Chla1.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10))
  train <- ex1[-a,] ; test <- ex1[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)
               +NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)+
               NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
             family=Gamma(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(Chla~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(FC))+s(NH3N)+s(PO4P)
                     +s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasi(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(Chla~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(FC))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasi(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  Chla1.Bag.RMSE.mlr <- c(Chla1.Bag.RMSE.mlr,
                        sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.bag.glm.final,
                         time=test$time)
  Chla1.Bag.RMSE.glm <- c(Chla1.Bag.RMSE.glm,
                        sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                               length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.final,
                         time=test$time)
  Chla1.Bag.RMSE.gam <- c(Chla1.Bag.RMSE.gam,
                        sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                               length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  Chla1.Bag.RMSE.tvcm <- c(Chla1.Bag.RMSE.tvcm,
                         sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                                length(data.tvcm$response)))
  print('Chla1.bag',i)
}
Chla1.Bag.RMSE <- data.frame(RMSE=c(Chla1.Bag.RMSE.mlr,Chla1.Bag.RMSE.glm,
                                    Chla1.Bag.RMSE.gam,Chla1.Bag.RMSE.tvcm),
                           model=c(rep("1_MLR",10),rep("2_GLM",10),
                                   rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla1.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan Chla (Bagging)")


# 우치 TC

TC2.Bag.RMSE.mlr <- c()
TC2.Bag.RMSE.glm <- c()
TC2.Bag.RMSE.gam <- c()
TC2.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
               +NH3N+PO4P+log(FC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(TC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
               NH3N+PO4P+log(FC)+Flow+Rain,data=train.bag,
             family=poisson(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(TC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                     +s(log(FC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(TC~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(FC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$TC,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  TC2.Bag.RMSE.mlr <- c(TC2.Bag.RMSE.mlr,
                        sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$TC,fitted_values=pred.bag.glm.final,
                         time=test$time)
  TC2.Bag.RMSE.glm <- c(TC2.Bag.RMSE.glm,
                        sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                               length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$TC,fitted_values=pred.bag.gam.final,
                         time=test$time)
  TC2.Bag.RMSE.gam <- c(TC2.Bag.RMSE.gam,
                        sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                               length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$TC,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  TC2.Bag.RMSE.tvcm <- c(TC2.Bag.RMSE.tvcm,
                         sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                                length(data.tvcm$response)))
  print('TC2.bag',i)
}
TC2.Bag.RMSE <- data.frame(RMSE=c(TC2.Bag.RMSE.mlr,TC2.Bag.RMSE.glm,
                                  TC2.Bag.RMSE.gam,TC2.Bag.RMSE.tvcm),
                           model=c(rep("1_MLR",10),rep("2_GLM",10),
                                   rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(TC2.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi TC (Bagging)")


# 우치 FC

FC2.Bag.RMSE.mlr <- c()
FC2.Bag.RMSE.glm <- c()
FC2.Bag.RMSE.gam <- c()
FC2.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)
               +NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(FC~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(Chla)+
               NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
             family=poisson(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(FC~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(Chla))+s(NH3N)+s(PO4P)
                     +s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(FC~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(Chla))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasipoisson(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$FC,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  FC2.Bag.RMSE.mlr <- c(FC2.Bag.RMSE.mlr,
                        sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$FC,fitted_values=pred.bag.glm.final,
                         time=test$time)
  FC2.Bag.RMSE.glm <- c(FC2.Bag.RMSE.glm,
                        sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                               length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$FC,fitted_values=pred.bag.gam.final,
                         time=test$time)
  FC2.Bag.RMSE.gam <- c(FC2.Bag.RMSE.gam,
                        sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                               length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$FC,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  FC2.Bag.RMSE.tvcm <- c(FC2.Bag.RMSE.tvcm,
                         sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                                length(data.tvcm$response)))
  print('FC2.bag',i)
}
FC2.Bag.RMSE <- data.frame(RMSE=c(FC2.Bag.RMSE.mlr,FC2.Bag.RMSE.glm,
                                  FC2.Bag.RMSE.gam,FC2.Bag.RMSE.tvcm),
                           model=c(rep("1_MLR",10),rep("2_GLM",10),
                                   rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(FC2.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi FC (Bagging)")


# 우치 Chla

Chla2.Bag.RMSE.mlr <- c()
Chla2.Bag.RMSE.glm <- c()
Chla2.Bag.RMSE.gam <- c()
Chla2.Bag.RMSE.tvcm <- c()
for (i in 1:50) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10))
  train <- ex2[-a,] ; test <- ex2[a,]
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.glm <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.gam <- matrix(nrow=nrow(test), ncol=30) 
  pred.bag.tvcm <- matrix(nrow=nrow(test), ncol=30) 
  for (j in 1:30) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    # Multiple Linear Regression
    fit <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)
               +NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    # Generalized Linear Model
    m <- glm(Chla~pH+DO+BOD+COD+SS+TN+TP+TOC+WT+EC+log(FC)+
               NH3N+PO4P+log(TC)+Flow+Rain,data=train.bag,
             family=Gamma(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm <- predict(m.step,newdata=test,type="response")
    pred.bag.glm[,j] <- pred.glm
    print(c('GLM',j))
    # Generalized Additive Model
    mm.shrink <- gam(Chla~s(pH)+s(DO)+s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                     +s(TOC)+s(WT)+s(EC)+s(log(FC))+s(NH3N)+s(PO4P)
                     +s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                     family=quasi(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.gam <- predict(mm.shrink,newdata=test,type="response")
    pred.bag.gam[,j] <- pred.gam
    print(c('GAM',j))
    # Time Varying Coefficient Model
    vc.shrink <- gam(Chla~s(time)+s(time,by=pH)+s(time,by=DO)+
                       s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                       s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                       s(time,by=WT)+s(time,by=EC)+s(time,by=log(FC))+
                       s(time,by=NH3N)+s(time,by=PO4P)+s(time,by=log(TC))+
                       s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                     family=quasi(link="log"),method="GCV.Cp",
                     select=TRUE)
    pred.tvcm <- predict(vc.shrink,newdata=test,type="response")
    pred.bag.tvcm[,j] <- pred.tvcm
    print(c('TVCM',j))}
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  Chla2.Bag.RMSE.mlr <- c(Chla2.Bag.RMSE.mlr,
                          sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                                 length(data.mlr$response)))
  pred.bag.glm.final <- rowMeans(pred.bag.glm)
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.bag.glm.final,
                         time=test$time)
  Chla2.Bag.RMSE.glm <- c(Chla2.Bag.RMSE.glm,
                          sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                                 length(data.glm$response)))
  pred.bag.gam.final <- rowMeans(pred.bag.gam)
  data.gam <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.final,
                         time=test$time)
  Chla2.Bag.RMSE.gam <- c(Chla2.Bag.RMSE.gam,
                          sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
                                 length(data.gam$response)))
  pred.bag.tvcm.final <- rowMeans(pred.bag.tvcm)
  data.tvcm <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.final,
                          time=test$time)
  Chla2.Bag.RMSE.tvcm <- c(Chla2.Bag.RMSE.tvcm,
                           sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
                                  length(data.tvcm$response)))
  print('Chla2.bag',i)
}
Chla2.Bag.RMSE <- data.frame(RMSE=c(Chla2.Bag.RMSE.mlr,Chla2.Bag.RMSE.glm,
                                    Chla2.Bag.RMSE.gam,Chla2.Bag.RMSE.tvcm),
                             model=c(rep("1_MLR",10),rep("2_GLM",10),
                                     rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla2.Bag.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi Chla (Bagging)")










### Compute NSE
## reference1 : https://www.rdocumentation.org/packages/hydroGOF/versions/0.4-0/topics/NSE
## reference2 : https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

install.packages("hydroGOF")
library(hydroGOF)

# NOT RUN {
obs <- 1:10
sim <- 1:10
NSE(sim, obs)

obs <- 1:10
sim <- 2:11
NSE(sim, obs)

#################
# Computing NSE on the (natural) logarithm of simulated and observed values
obs <- 1:10/10
sim <- 2:11/10
NSE(sim=sim, obs=obs, FUN=log)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
NSE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'NSE'
NSE(sim=sim, obs=obs)
# }




### Compute Bias
## reference : https://www.rdocumentation.org/packages/SimDesign/versions/1.14/topics/bias

my_bias <- function(sim, obs) {
  bias = sum(sim-obs)/length(sim)
  return(bias)
}









### Compute MAE
## reference1 : https://www.rdocumentation.org/packages/Metrics/versions/0.1.4/topics/mae
## reference2 : https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf

install.packages("hydroGOF")
library(hydroGOF)

obs <- 1:10
sim <- 1:10
mae(sim, obs)
obs <- 1:10
sim <- 2:11
mae(sim, obs)
##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
# Generating a simulated daily time series, initially equal to the observed series
sim <- obs
# Computing the mean absolute error for the "best" case
mae(sim=sim, obs=obs)
# Randomly changing the first 2000 elements of 'sim', by using a normal distribution
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)
# Computing the new mean absolute error
mae(sim=sim, obs=obs)
