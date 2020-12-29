# 반응변수 : Chla
# 설명변수 : BOD, COD, SS, TN, TP, TOC, NH3N, PO4P, Flow, Rain, log(TC), log(FC)

ex1 <- read.csv("C:/Users/stat/Desktop/광산(2013-2019).csv", sep=",", header=T)
ex1 <- ex1[,-1]
ex1 <- as.data.frame(ex1)

ex2 <- read.csv("C:/Users/stat/Desktop/우치(2013-2019).csv", sep=",", header=T)
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


## 광산 Chla
Chla1.RMSE.mlr <- c()
Chla1.RMSE.glm.Gamma <- c()
Chla1.RMSE.gam.Gamma <- c()
Chla1.RMSE.gam.quasi <- c()
Chla1.RMSE.tvcm.Gamma <- c()
Chla1.RMSE.tvcm.quasi <- c()
for (i in 1:1000) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10),replace=FALSE)
  train <- ex1[-a,] ; test <- ex1[a,]
  
  # Multiple Linear Regression
  fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+NH3N+PO4P+log(FC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.mlr,
                         time=test$time)
  Chla1.RMSE.mlr <- c(Chla1.RMSE.mlr,
                      sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                             length(data.mlr$response)))
  print(c('Chla1.MLR',i,sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response))))
  
  # Generalized Linear Model (Gamma)
  m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+NH3N+PO4P+log(FC)+Flow+Rain,data=train,
           family=Gamma(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.glm,
                         time=test$time)
  Chla1.RMSE.glm.Gamma <- c(Chla1.RMSE.glm.Gamma,
                            sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                                   length(data.glm$response)))
  print(c('Chla1.GLM.Gamma',i,sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                                     length(data.glm$response))))
  
  # Generalized Additive Model (Gamma)
  mm.shrink1 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                    +s(TOC)+s(log(TC))+s(NH3N)+s(PO4P)+s(log(FC))+s(Flow)+s(Rain),data=train,
                    family=Gamma(link="log"),method="REML",
                    select=TRUE)
  pred.gam1 <- predict(mm.shrink1,newdata=test,type="response")
  data.gam1 <- data.frame(response=test$Chla,fitted_values=pred.gam1,
                          time=test$time)
  Chla1.RMSE.gam.Gamma <- c(Chla1.RMSE.gam.Gamma,
                            sqrt(sum((data.gam1$response-data.gam1$fitted_values)^2)/
                                   length(data.gam1$response)))
  print(c('Chla1.GAM.Gamma',i,sqrt(sum((data.gam1$response-data.gam1$fitted_values)^2)/
                                     length(data.gam1$response))))
  
  # Generalized Additive Model (quasi)
  mm.shrink2 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                    +s(TOC)+s(log(TC))+s(NH3N)+s(PO4P)+s(log(FC))+s(Flow)+s(Rain),data=train,
                    family=quasi(link="log"),method="REML",
                    select=TRUE)
  pred.gam2 <- predict(mm.shrink2,newdata=test,type="response")
  data.gam2 <- data.frame(response=test$Chla,fitted_values=pred.gam2,
                          time=test$time)
  Chla1.RMSE.gam.quasi <- c(Chla1.RMSE.gam.quasi,
                            sqrt(sum((data.gam2$response-data.gam2$fitted_values)^2)/
                                   length(data.gam2$response)))
  print(c('Chla1.GAM.quasi',i,sqrt(sum((data.gam2$response-data.gam2$fitted_values)^2)/
                                     length(data.gam2$response))))
  
  # Time Varying Coefficient Model (Gamma)
  vc.shrink1 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                      s(time,by=log(TC))+s(time,by=NH3N)+s(time,by=PO4P)+
                      s(time,by=log(FC))+s(time,by=Flow)+s(time,by=Rain),data=train,
                    family=Gamma(link="log"),method="REML",
                    select=TRUE)
  pred.tvcm1 <- predict(vc.shrink1,newdata=test,type="response")
  data.tvcm1 <- data.frame(response=test$Chla,fitted_values=pred.tvcm1,
                           time=test$time)
  Chla1.RMSE.tvcm.Gamma <- c(Chla1.RMSE.tvcm.Gamma,
                             sqrt(sum((data.tvcm1$response-data.tvcm1$fitted_values)^2)/
                                    length(data.tvcm1$response)))
  print(c('Chla1.TVCM.Gamma',i,sqrt(sum((data.tvcm1$response-data.tvcm1$fitted_values)^2)/
                                      length(data.tvcm1$response))))
  
  # Time Varying Coefficient Model (quasi)
  vc.shrink2 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                      s(time,by=log(TC))+s(time,by=NH3N)+s(time,by=PO4P)+
                      s(time,by=log(FC))+s(time,by=Flow)+s(time,by=Rain),data=train,
                    family=quasi(link="log"),method="REML",
                    select=TRUE)
  pred.tvcm2 <- predict(vc.shrink2,newdata=test,type="response")
  data.tvcm2 <- data.frame(response=test$Chla,fitted_values=pred.tvcm2,
                           time=test$time)
  Chla1.RMSE.tvcm.quasi <- c(Chla1.RMSE.tvcm.quasi,
                             sqrt(sum((data.tvcm2$response-data.tvcm2$fitted_values)^2)/
                                    length(data.tvcm2$response)))
  print(c('Chla1.TVCM.quasi',i,sqrt(sum((data.tvcm2$response-data.tvcm2$fitted_values)^2)/
                                      length(data.tvcm2$response))))
  
  
}



Chla1_1.RMSE <- data.frame(RMSE=c(Chla1.RMSE.mlr,Chla1.RMSE.glm.Gamma,
                                Chla1.RMSE.gam.Gamma,Chla1.RMSE.gam.quasi,
                                Chla1.RMSE.tvcm.Gamma,Chla1.RMSE.tvcm.quasi),
                         model=c(rep("a_MLR",1000),rep("b_GLM.Gamma",1000),
                                 rep("c_GAM.Gamma",1000),rep("d_GAM.quasi",1000),
                                 rep("e_TVCM.Gamma",1000),rep("f_TVCM.quasi",1000)))
ggplot(Chla1_1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 70)) + ggtitle("Gwangsan Chla (correct)")



Chla1_2.RMSE <- data.frame(RMSE=c(Chla1.RMSE.glm.Gamma,
                                  Chla1.RMSE.gam.Gamma,Chla1.RMSE.gam.quasi,
                                  Chla1.RMSE.tvcm.Gamma,Chla1.RMSE.tvcm.quasi),
                           model=c(rep("a_GLM.Gamma",1000),
                                   rep("b_GAM.Gamma",1000),rep("c_GAM.quasi",1000),
                                   rep("d_TVCM.Gamma",1000),rep("e_TVCM.quasi",1000)))
ggplot(Chla1_2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 70)) + ggtitle("Gwangsan Chla (correct)")


mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="a_GLM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="a_GLM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="b_GAM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="b_GAM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="c_GAM.quasi"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="c_GAM.quasi"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="d_TVCM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="d_TVCM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="e_TVCM.quasi"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="e_TVCM.quasi"])




# 우치 Chla
Chla2.RMSE.mlr <- c()
Chla2.RMSE.glm.Gamma <- c()
Chla2.RMSE.gam.Gamma <- c()
Chla2.RMSE.gam.quasi <- c()
Chla2.RMSE.tvcm.Gamma <- c()
Chla2.RMSE.tvcm.quasi <- c()

for (i in 1:1000) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10),replace=FALSE)
  train <- ex2[-a,] ; test <- ex2[a,]
  
  # Multiple Linear Regression
  fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+NH3N+PO4P+log(FC)+Flow+Rain,data=train,
             family=gaussian(link="identity"))
  fit.step <- stepAIC(fit, direction="both", trace=FALSE)
  pred.mlr <- predict(fit.step,newdata=test,type="response")
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.mlr,
                         time=test$time)
  Chla2.RMSE.mlr <- c(Chla2.RMSE.mlr,
                      sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                             length(data.mlr$response)))
  print(c('Chla2.MLR',i,sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                               length(data.mlr$response))))
  
  # Generalized Linear Model (Gamma)
  m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+NH3N+PO4P+log(FC)+Flow+Rain,data=train,
           family=Gamma(link="log"))
  m.step <- stepAIC(m, direction="both", trace=FALSE)
  pred.glm <- predict(m.step,newdata=test,type="response")
  data.glm <- data.frame(response=test$Chla,fitted_values=pred.glm,
                         time=test$time)
  Chla2.RMSE.glm.Gamma <- c(Chla2.RMSE.glm.Gamma,
                            sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                                   length(data.glm$response)))
  print(c('Chla2.GLM.Gamma',i,sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
                                     length(data.glm$response))))
  
  # Generalized Additive Model (Gamma)
  mm.shrink1 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                    +s(TOC)+s(log(TC))+s(NH3N)+s(PO4P)+s(log(FC))+s(Flow)+s(Rain),data=train,
                    family=Gamma(link="log"),method="REML",
                    select=TRUE)
  pred.gam1 <- predict(mm.shrink1,newdata=test,type="response")
  data.gam1 <- data.frame(response=test$Chla,fitted_values=pred.gam1,
                          time=test$time)
  Chla2.RMSE.gam.Gamma <- c(Chla2.RMSE.gam.Gamma,
                            sqrt(sum((data.gam1$response-data.gam1$fitted_values)^2)/
                                   length(data.gam1$response)))
  print(c('Chla2.GAM.Gamma',i,sqrt(sum((data.gam1$response-data.gam1$fitted_values)^2)/
                                     length(data.gam1$response))))
  
  # Generalized Additive Model (quasi)
  mm.shrink2 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                    +s(TOC)+s(log(TC))+s(NH3N)+s(PO4P)+s(log(FC))+s(Flow)+s(Rain),data=train,
                    family=quasi(link="log"),method="REML",
                    select=TRUE)
  pred.gam2 <- predict(mm.shrink2,newdata=test,type="response")
  data.gam2 <- data.frame(response=test$Chla,fitted_values=pred.gam2,
                          time=test$time)
  Chla2.RMSE.gam.quasi <- c(Chla2.RMSE.gam.quasi,
                            sqrt(sum((data.gam2$response-data.gam2$fitted_values)^2)/
                                   length(data.gam2$response)))
  print(c('Chla2.GAM.quasi',i,sqrt(sum((data.gam2$response-data.gam2$fitted_values)^2)/
                                     length(data.gam2$response))))
  
  # Time Varying Coefficient Model (Gamma)
  vc.shrink1 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                      s(time,by=log(TC))+s(time,by=NH3N)+s(time,by=PO4P)+
                      s(time,by=log(FC))+s(time,by=Flow)+s(time,by=Rain),data=train,
                    family=Gamma(link="log"),method="REML",
                    select=TRUE)
  pred.tvcm1 <- predict(vc.shrink1,newdata=test,type="response")
  data.tvcm1 <- data.frame(response=test$Chla,fitted_values=pred.tvcm1,
                           time=test$time)
  Chla2.RMSE.tvcm.Gamma <- c(Chla2.RMSE.tvcm.Gamma,
                             sqrt(sum((data.tvcm1$response-data.tvcm1$fitted_values)^2)/
                                    length(data.tvcm1$response)))
  print(c('Chla2.TVCM.Gamma',i,sqrt(sum((data.tvcm1$response-data.tvcm1$fitted_values)^2)/
                                      length(data.tvcm1$response))))
  
  # Time Varying Coefficient Model (quasi)
  vc.shrink2 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                      s(time,by=log(TC))+s(time,by=NH3N)+s(time,by=PO4P)+
                      s(time,by=log(FC))+s(time,by=Flow)+s(time,by=Rain),data=train,
                    family=quasi(link="log"),method="REML",
                    select=TRUE)
  pred.tvcm2 <- predict(vc.shrink2,newdata=test,type="response")
  data.tvcm2 <- data.frame(response=test$Chla,fitted_values=pred.tvcm2,
                           time=test$time)
  Chla2.RMSE.tvcm.quasi <- c(Chla2.RMSE.tvcm.quasi,
                             sqrt(sum((data.tvcm2$response-data.tvcm2$fitted_values)^2)/
                                    length(data.tvcm2$response)))
  print(c('Chla2.TVCM.quasi',i,sqrt(sum((data.tvcm2$response-data.tvcm2$fitted_values)^2)/
                                      length(data.tvcm2$response))))
  
}



Chla2_1.RMSE <- data.frame(RMSE=c(Chla2.RMSE.mlr,Chla2.RMSE.glm.Gamma,
                                Chla2.RMSE.gam.Gamma,Chla2.RMSE.gam.quasi,
                                Chla2.RMSE.tvcm.Gamma,Chla2.RMSE.tvcm.quasi),
                         model=c(rep("a_MLR",1000),rep("b_GLM.Gamma",1000),
                                 rep("c_GAM.Gamma",1000),rep("d_GAM.quasi",1000),
                                 rep("e_TVCM.Gamma",1000),rep("f_TVCM.quasi",1000)))
ggplot(Chla2_1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 70)) + ggtitle("Uchi Chla (correct)")



Chla2_2.RMSE <- data.frame(RMSE=c(Chla2.RMSE.glm.Gamma,
                                  Chla2.RMSE.gam.Gamma,Chla2.RMSE.gam.quasi,
                                  Chla2.RMSE.tvcm.Gamma,Chla2.RMSE.tvcm.quasi),
                           model=c(rep("a_GLM.Gamma",1000),
                                   rep("b_GAM.Gamma",1000),rep("c_GAM.quasi",1000),
                                   rep("d_TVCM.Gamma",1000),rep("e_TVCM.quasi",1000)))
ggplot(Chla2_2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 70)) + ggtitle("Uchi Chla (correct)")


mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="a_GLM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="a_GLM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="b_GAM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="b_GAM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="c_GAM.quasi"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="c_GAM.quasi"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="d_TVCM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="d_TVCM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="e_TVCM.quasi"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="e_TVCM.quasi"])




## 광산 mean, sd

mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="a_GLM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="a_GLM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="b_GAM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="b_GAM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="c_GAM.quasi"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="c_GAM.quasi"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="d_TVCM.Gamma"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="d_TVCM.Gamma"])
mean(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="e_TVCM.quasi"])
sd(Chla1_2.RMSE$RMSE[Chla1_2.RMSE$model=="e_TVCM.quasi"])


## 우치 mean, sd

mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="a_GLM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="a_GLM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="b_GAM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="b_GAM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="c_GAM.quasi"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="c_GAM.quasi"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="d_TVCM.Gamma"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="d_TVCM.Gamma"])
mean(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="e_TVCM.quasi"])
sd(Chla2_2.RMSE$RMSE[Chla2_2.RMSE$model=="e_TVCM.quasi"])
