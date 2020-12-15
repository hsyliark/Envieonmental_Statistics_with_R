# 반응변수 : Chla
# 설명변수 : BOD, COD, SS, TN, TP, TOC, log(TC), Flow, Rain

ex1 <- read.csv("C:/Users/stat/Desktop/광산(2010-2019).csv", sep=",", header=T)
ex1 <- ex1[,-1]
ex1 <- as.data.frame(ex1)

ex2 <- read.csv("C:/Users/stat/Desktop/우치(2010-2019).csv", sep=",", header=T)
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
# Bagging
Chla1.Bag.RMSE.mlr <- c()
Chla1.Bag.RMSE.glm.Gamma <- c()
Chla1.Bag.RMSE.gam.Gamma <- c()
Chla1.Bag.RMSE.gam.quasi <- c()
Chla1.Bag.RMSE.tvcm.Gamma <- c()
Chla1.Bag.RMSE.tvcm.quasi <- c()
for (i in 1:100) {
  a <- sample(1:nrow(ex1),round(3*nrow(ex1)/10),replace=FALSE)
  train <- ex1[-a,] ; test <- ex1[a,]
  
  # Multiple Linear Regression
  fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train,
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
  m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train,
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
                    +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train,
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
                    +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train,
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
                      s(time,by=log(TC))+s(time,by=Flow)+s(time,by=Rain),data=train,
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
                      s(time,by=log(TC))+s(time,by=Flow)+s(time,by=Rain),data=train,
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
  
  ## Bagging
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.glm.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.gam.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.gam.quasi <- matrix(nrow=nrow(test), ncol=100)
  pred.bag.tvcm.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.tvcm.quasi <- matrix(nrow=nrow(test), ncol=100)
  
  for (j in 1:100) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    
    # Multiple Linear Regression
    fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    
    # Generalized Linear Model (Gamma)
    m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train.bag,
             family=Gamma(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm.Gamma <- predict(m.step,newdata=test,type="response")
    pred.bag.glm.Gamma[,j] <- pred.glm.Gamma
    print(c('GLM.Gamma',j))
    
    # Generalized Additive Model (Gamma)
    mm.shrink1 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                      +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                      family=Gamma(link="log"),method="REML",
                      select=TRUE)
    pred.gam.Gamma <- predict(mm.shrink1,newdata=test,type="response")
    pred.bag.gam.Gamma[,j] <- pred.gam.Gamma
    print(c('GAM.Gamma',j))
    
    # Generalized Additive Model (quasi)
    mm.shrink2 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                      +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                      family=quasi(link="log"),method="REML",
                      select=TRUE)
    pred.gam.quasi <- predict(mm.shrink2,newdata=test,type="response")
    pred.bag.gam.quasi[,j] <- pred.gam.quasi
    print(c('GAM.quasi',j))
    
    # Time Varying Coefficient Model (Gamma)
    vc.shrink1 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                        s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                        s(time,by=log(TC))+s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                      family=Gamma(link="log"),method="REML",
                      select=TRUE)
    pred.tvcm.Gamma <- predict(vc.shrink1,newdata=test,type="response")
    pred.bag.tvcm.Gamma[,j] <- pred.tvcm.Gamma
    print(c('TVCM.Gamma',j))
    
    # Time Varying Coefficient Model (quasi)
    vc.shrink2 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                        s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+
                        s(time,by=log(TC))+s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                      family=quasi(link="log"),method="REML",
                      select=TRUE)
    pred.tvcm.quasi <- predict(vc.shrink2,newdata=test,type="response")
    pred.bag.tvcm.quasi[,j] <- pred.tvcm.quasi
    print(c('TVCM.quasi',j))
  }
  
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  Chla1.Bag.RMSE.mlr <- c(Chla1.Bag.RMSE.mlr,
                          sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                                 length(data.mlr$response)))
  print(c('Chla1.Bag.mlr',i,sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                                   length(data.mlr$response))))
  
  pred.bag.glm.Gamma.final <- rowMeans(pred.bag.glm.Gamma)
  data.glm.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.glm.Gamma.final,
                               time=test$time)
  Chla1.Bag.RMSE.glm.Gamma <- c(Chla1.Bag.RMSE.glm.Gamma,
                                sqrt(sum((data.glm.Gamma$response-data.glm.Gamma$fitted_values)^2)/
                                       length(data.glm.Gamma$response)))
  print(c('Chla1.Bag.glm.Gamma',i,sqrt(sum((data.glm.Gamma$response-data.glm.Gamma$fitted_values)^2)/
                                         length(data.glm.Gamma$response))))
  
  pred.bag.gam.Gamma.final <- rowMeans(pred.bag.gam.Gamma)
  data.gam.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.Gamma.final,
                               time=test$time)
  Chla1.Bag.RMSE.gam.Gamma <- c(Chla1.Bag.RMSE.gam.Gamma,
                                sqrt(sum((data.gam.Gamma$response-data.gam.Gamma$fitted_values)^2)/
                                       length(data.gam.Gamma$response)))
  print(c('Chla1.Bag.gam.Gamma',i,sqrt(sum((data.gam.Gamma$response-data.gam.Gamma$fitted_values)^2)/
                                         length(data.gam.Gamma$response))))
  
  pred.bag.gam.quasi.final <- rowMeans(pred.bag.gam.quasi)
  data.gam.quasi <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.quasi.final,
                               time=test$time)
  Chla1.Bag.RMSE.gam.quasi <- c(Chla1.Bag.RMSE.gam.quasi,
                                sqrt(sum((data.gam.quasi$response-data.gam.quasi$fitted_values)^2)/
                                       length(data.gam.quasi$response)))
  print(c('Chla1.Bag.gam.quasi',i,sqrt(sum((data.gam.quasi$response-data.gam.quasi$fitted_values)^2)/
                                         length(data.gam.quasi$response))))
  
  pred.bag.tvcm.Gamma.final <- rowMeans(pred.bag.tvcm.Gamma)
  data.tvcm.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.Gamma.final,
                                time=test$time)
  Chla1.Bag.RMSE.tvcm.Gamma <- c(Chla1.Bag.RMSE.tvcm.Gamma,
                                 sqrt(sum((data.tvcm.Gamma$response-data.tvcm.Gamma$fitted_values)^2)/
                                        length(data.tvcm.Gamma$response)))
  print(c('Chla1.Bag.tvcm.Gamma',i,sqrt(sum((data.tvcm.Gamma$response-data.tvcm.Gamma$fitted_values)^2)/
                                          length(data.tvcm.Gamma$response))))
  
  pred.bag.tvcm.quasi.final <- rowMeans(pred.bag.tvcm.quasi)
  data.tvcm.quasi <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.quasi.final,
                                time=test$time)
  Chla1.Bag.RMSE.tvcm.quasi <- c(Chla1.Bag.RMSE.tvcm.quasi,
                                 sqrt(sum((data.tvcm.quasi$response-data.tvcm.quasi$fitted_values)^2)/
                                        length(data.tvcm.quasi$response)))
  print(c('Chla1.Bag.tvcm.quasi',i,sqrt(sum((data.tvcm.quasi$response-data.tvcm.quasi$fitted_values)^2)/
                                          length(data.tvcm.quasi$response))))
  
  print('Chla1.bag',i)
  
}

Chla1.RMSE <- data.frame(RMSE=c(Chla1.RMSE.mlr,Chla1.RMSE.glm.Gamma,
                                Chla1.RMSE.gam.Gamma,Chla1.RMSE.gam.quasi,
                                Chla1.RMSE.tvcm.Gamma,Chla1.RMSE.tvcm.quasi,
                                Chla1.Bag.RMSE.mlr,Chla1.Bag.RMSE.glm.Gamma,
                                Chla1.Bag.RMSE.gam.Gamma,Chla1.Bag.RMSE.gam.quasi,
                                Chla1.Bag.RMSE.tvcm.Gamma,Chla1.Bag.RMSE.tvcm.quasi),
                         model=c(rep("a_MLR",100),rep("b_GLM.Gamma",100),
                                 rep("c_GAM.Gamma",100),rep("d_GAM.quasi",100),
                                 rep("e_TVCM.Gamma",100),rep("f_TVCM.quasi",100),
                                 rep("g_MLR_Bag",100),rep("h_GLM.Gamma_Bag",100),
                                 rep("i_GAM.Gamma_Bag",100),rep("j_GAM.quasi.Bag",100),
                                 rep("k_TVCM.Gamma_Bag",100),rep("l_TVCM.quasi_Bag",100)))
ggplot(Chla1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 150)) + ggtitle("Gwangsan Chla (correct)")

Chla1_1.RMSE <- data.frame(RMSE=c(Chla1.RMSE.glm.Gamma,
                                Chla1.RMSE.gam.Gamma,Chla1.RMSE.gam.quasi,
                                Chla1.RMSE.tvcm.Gamma,Chla1.RMSE.tvcm.quasi,
                                Chla1.Bag.RMSE.glm.Gamma,
                                Chla1.Bag.RMSE.gam.Gamma,Chla1.Bag.RMSE.gam.quasi,
                                Chla1.Bag.RMSE.tvcm.Gamma,Chla1.Bag.RMSE.tvcm.quasi),
                         model=c(rep("a_GLM.Gamma",100),
                                 rep("b_GAM.Gamma",100),rep("c_GAM.quasi",100),
                                 rep("d_TVCM.Gamma",100),rep("e_TVCM.quasi",100),
                                 rep("f_GLM.Gamma_Bag",100),
                                 rep("g_GAM.Gamma_Bag",100),rep("h_GAM.quasi.Bag",100),
                                 rep("i_TVCM.Gamma_Bag",100),rep("j_TVCM.quasi_Bag",100)))
ggplot(Chla1_1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 150)) + ggtitle("Gwangsan Chla (correct)")






# 우치 Chla
Chla2.RMSE.mlr <- c()
Chla2.RMSE.glm.Gamma <- c()
Chla2.RMSE.gam.Gamma <- c()
Chla2.RMSE.gam.quasi <- c()
Chla2.RMSE.tvcm.Gamma <- c()
Chla2.RMSE.tvcm.quasi <- c()
# Bagging
Chla2.Bag.RMSE.mlr <- c()
Chla2.Bag.RMSE.glm.Gamma <- c()
Chla2.Bag.RMSE.gam.Gamma <- c()
Chla2.Bag.RMSE.gam.quasi <- c()
Chla2.Bag.RMSE.tvcm.Gamma <- c()
Chla2.Bag.RMSE.tvcm.quasi <- c()
for (i in 1:100) {
  a <- sample(1:nrow(ex2),round(3*nrow(ex2)/10),replace=FALSE)
  train <- ex2[-a,] ; test <- ex2[a,]
  
  # Multiple Linear Regression
  fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train,
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
  m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train,
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
                    +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train,
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
                    +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train,
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
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+s(time,by=log(TC))+
                      s(time,by=Flow)+s(time,by=Rain),data=train,
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
                      s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+s(time,by=log(TC))+
                      s(time,by=Flow)+s(time,by=Rain),data=train,
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
  
  ## Bagging
  pred.bag.mlr <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.glm.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.gam.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.gam.quasi <- matrix(nrow=nrow(test), ncol=100)
  pred.bag.tvcm.Gamma <- matrix(nrow=nrow(test), ncol=100) 
  pred.bag.tvcm.quasi <- matrix(nrow=nrow(test), ncol=100)
  
  for (j in 1:100) {
    b <- sample(1:nrow(train),nrow(train),replace=TRUE)
    train.bag <- train[b,]
    
    # Multiple Linear Regression
    fit <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train.bag,
               family=gaussian(link="identity"))
    fit.step <- stepAIC(fit, direction="both", trace=FALSE)
    pred.mlr <- predict(fit.step,newdata=test,type="response")
    pred.bag.mlr[,j] <- pred.mlr
    print(c('MLR',j))
    
    # Generalized Linear Model (Gamma)
    m <- glm(Chla~BOD+COD+SS+TN+TP+TOC+log(TC)+Flow+Rain,data=train.bag,
             family=Gamma(link="log"))
    m.step <- stepAIC(m, direction="both", trace=FALSE)
    pred.glm.Gamma <- predict(m.step,newdata=test,type="response")
    pred.bag.glm.Gamma[,j] <- pred.glm.Gamma
    print(c('GLM.Gamma',j))
    
    # Generalized Additive Model (Gamma)
    mm.shrink1 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                      +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                      family=Gamma(link="log"),method="REML",
                      select=TRUE)
    pred.gam.Gamma <- predict(mm.shrink1,newdata=test,type="response")
    pred.bag.gam.Gamma[,j] <- pred.gam.Gamma
    print(c('GAM.Gamma',j))
    
    # Generalized Additive Model (quasi)
    mm.shrink2 <- gam(Chla~s(BOD)+s(COD)+s(SS)+s(TN)+s(TP)
                      +s(TOC)+s(log(TC))+s(Flow)+s(Rain),data=train.bag,
                      family=quasi(link="log"),method="REML",
                      select=TRUE)
    pred.gam.quasi <- predict(mm.shrink2,newdata=test,type="response")
    pred.bag.gam.quasi[,j] <- pred.gam.quasi
    print(c('GAM.quasi',j))
    
    # Time Varying Coefficient Model (Gamma)
    vc.shrink1 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                        s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+s(time,by=log(TC))+
                        s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                      family=Gamma(link="log"),method="REML",
                      select=TRUE)
    pred.tvcm.Gamma <- predict(vc.shrink1,newdata=test,type="response")
    pred.bag.tvcm.Gamma[,j] <- pred.tvcm.Gamma
    print(c('TVCM.Gamma',j))
    
    # Time Varying Coefficient Model (quasi)
    vc.shrink2 <- gam(Chla~s(time)+s(time,by=BOD)+s(time,by=COD)+s(time,by=SS)+
                        s(time,by=TN)+s(time,by=TP)+s(time,by=TOC)+s(time,by=log(TC))+
                        s(time,by=Flow)+s(time,by=Rain),data=train.bag,
                      family=quasi(link="log"),method="REML",
                      select=TRUE)
    pred.tvcm.quasi <- predict(vc.shrink2,newdata=test,type="response")
    pred.bag.tvcm.quasi[,j] <- pred.tvcm.quasi
    print(c('TVCM.quasi',j))
  }
  
  pred.bag.mlr.final <- rowMeans(pred.bag.mlr)
  data.mlr <- data.frame(response=test$Chla,fitted_values=pred.bag.mlr.final,
                         time=test$time)
  Chla2.Bag.RMSE.mlr <- c(Chla2.Bag.RMSE.mlr,
                          sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                                 length(data.mlr$response)))
  print(c('Chla2.Bag.mlr',i,sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
                                   length(data.mlr$response))))
  
  pred.bag.glm.Gamma.final <- rowMeans(pred.bag.glm.Gamma)
  data.glm.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.glm.Gamma.final,
                               time=test$time)
  Chla2.Bag.RMSE.glm.Gamma <- c(Chla2.Bag.RMSE.glm.Gamma,
                                sqrt(sum((data.glm.Gamma$response-data.glm.Gamma$fitted_values)^2)/
                                       length(data.glm.Gamma$response)))
  print(c('Chla2.Bag.glm.Gamma',i,sqrt(sum((data.glm.Gamma$response-data.glm.Gamma$fitted_values)^2)/
                                         length(data.glm.Gamma$response))))
  
  pred.bag.gam.Gamma.final <- rowMeans(pred.bag.gam.Gamma)
  data.gam.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.Gamma.final,
                               time=test$time)
  Chla2.Bag.RMSE.gam.Gamma <- c(Chla2.Bag.RMSE.gam.Gamma,
                                sqrt(sum((data.gam.Gamma$response-data.gam.Gamma$fitted_values)^2)/
                                       length(data.gam.Gamma$response)))
  print(c('Chla2.Bag.gam.Gamma',i,sqrt(sum((data.gam.Gamma$response-data.gam.Gamma$fitted_values)^2)/
                                         length(data.gam.Gamma$response))))
  
  pred.bag.gam.quasi.final <- rowMeans(pred.bag.gam.quasi)
  data.gam.quasi <- data.frame(response=test$Chla,fitted_values=pred.bag.gam.quasi.final,
                               time=test$time)
  Chla2.Bag.RMSE.gam.quasi <- c(Chla2.Bag.RMSE.gam.quasi,
                                sqrt(sum((data.gam.quasi$response-data.gam.quasi$fitted_values)^2)/
                                       length(data.gam.quasi$response)))
  print(c('Chla2.Bag.gam.quasi',i,sqrt(sum((data.gam.quasi$response-data.gam.quasi$fitted_values)^2)/
                                         length(data.gam.quasi$response))))
  
  pred.bag.tvcm.Gamma.final <- rowMeans(pred.bag.tvcm.Gamma)
  data.tvcm.Gamma <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.Gamma.final,
                                time=test$time)
  Chla2.Bag.RMSE.tvcm.Gamma <- c(Chla2.Bag.RMSE.tvcm.Gamma,
                                 sqrt(sum((data.tvcm.Gamma$response-data.tvcm.Gamma$fitted_values)^2)/
                                        length(data.tvcm.Gamma$response)))
  print(c('Chla2.Bag.tvcm.Gamma',i,sqrt(sum((data.tvcm.Gamma$response-data.tvcm.Gamma$fitted_values)^2)/
                                          length(data.tvcm.Gamma$response))))
  
  pred.bag.tvcm.quasi.final <- rowMeans(pred.bag.tvcm.quasi)
  data.tvcm.quasi <- data.frame(response=test$Chla,fitted_values=pred.bag.tvcm.quasi.final,
                                time=test$time)
  Chla2.Bag.RMSE.tvcm.quasi <- c(Chla2.Bag.RMSE.tvcm.quasi,
                                 sqrt(sum((data.tvcm.quasi$response-data.tvcm.quasi$fitted_values)^2)/
                                        length(data.tvcm.quasi$response)))
  print(c('Chla2.Bag.tvcm.quasi',i,sqrt(sum((data.tvcm.quasi$response-data.tvcm.quasi$fitted_values)^2)/
                                          length(data.tvcm.quasi$response))))
  
  print('Chla2.bag',i)
}

Chla2.RMSE <- data.frame(RMSE=c(Chla2.RMSE.mlr,Chla2.RMSE.glm.Gamma,
                                Chla2.RMSE.gam.Gamma,Chla2.RMSE.gam.quasi,
                                Chla2.RMSE.tvcm.Gamma,Chla2.RMSE.tvcm.quasi,
                                Chla2.Bag.RMSE.mlr,Chla2.Bag.RMSE.glm.Gamma,
                                Chla2.Bag.RMSE.gam.Gamma,Chla2.Bag.RMSE.gam.quasi,
                                Chla2.Bag.RMSE.tvcm.Gamma,Chla2.Bag.RMSE.tvcm.quasi),
                         model=c(rep("a_MLR",100),rep("b_GLM.Gamma",100),
                                 rep("c_GAM.Gamma",100),rep("d_GAM.quasi",100),
                                 rep("e_TVCM.Gamma",100),rep("f_TVCM.quasi",100),
                                 rep("g_MLR_Bag",100),rep("h_GLM.Gamma_Bag",100),
                                 rep("i_GAM.Gamma_Bag",100),rep("j_GAM.quasi.Bag",100),
                                 rep("k_TVCM.Gamma_Bag",100),rep("l_TVCM.quasi_Bag",100)))
ggplot(Chla2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 150)) + ggtitle("Uchi Chla (correct)")

Chla2_1.RMSE <- data.frame(RMSE=c(Chla2.RMSE.glm.Gamma,
                                Chla2.RMSE.gam.Gamma,Chla2.RMSE.gam.quasi,
                                Chla2.RMSE.tvcm.Gamma,Chla2.RMSE.tvcm.quasi,
                                Chla2.Bag.RMSE.glm.Gamma,
                                Chla2.Bag.RMSE.gam.Gamma,Chla2.Bag.RMSE.gam.quasi,
                                Chla2.Bag.RMSE.tvcm.Gamma,Chla2.Bag.RMSE.tvcm.quasi),
                         model=c(rep("a_GLM.Gamma",100),
                                 rep("b_GAM.Gamma",100),rep("c_GAM.quasi",100),
                                 rep("d_TVCM.Gamma",100),rep("e_TVCM.quasi",100),
                                 rep("f_GLM.Gamma_Bag",100),
                                 rep("g_GAM.Gamma_Bag",100),rep("h_GAM.quasi.Bag",100),
                                 rep("i_TVCM.Gamma_Bag",100),rep("j_TVCM.quasi_Bag",100)))
ggplot(Chla2_1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() +
  coord_cartesian(ylim = c(0, 150)) + ggtitle("Uchi Chla (correct)")
