## 10-fold crossvalidation

## 광산 TC

TC1.RMSE.mlr <- c()
TC1.RMSE.glm <- c()
TC1.RMSE.gam <- c()
TC1.RMSE.tvcm <- c()
for (i in 1:10) {
  a <- sample(1:nrow(ex1),round(4*nrow(ex1)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
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
for (i in 1:10) {
  a <- sample(1:nrow(ex1),round(4*nrow(ex1)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
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
for (i in 1:10) {
  a <- sample(1:nrow(ex1),round(4*nrow(ex1)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
}
Chla1.RMSE <- data.frame(RMSE=c(Chla1.RMSE.mlr,Chla1.RMSE.glm,
                                Chla1.RMSE.gam,Chla1.RMSE.tvcm),
                       model=c(rep("1_MLR",10),rep("2_GLM",10),
                               rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla1.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Gwangsan Chla")
  
  












## 우치 TC

TC2.RMSE.mlr <- c()
TC2.RMSE.glm <- c()
TC2.RMSE.gam <- c()
TC2.RMSE.tvcm <- c()
for (i in 1:10) {
  a <- sample(1:nrow(ex2),round(4*nrow(ex2)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
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
for (i in 1:10) {
  a <- sample(1:nrow(ex2),round(4*nrow(ex2)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
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
for (i in 1:10) {
  a <- sample(1:nrow(ex2),round(4*nrow(ex2)/10))
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
  print(c('MLR',i))
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
  print(c('GLM',i))
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
  print(c('GAM',i))
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
  print(c('TVCM',i))
}
Chla2.RMSE <- data.frame(RMSE=c(Chla2.RMSE.mlr,Chla2.RMSE.glm,
                                Chla2.RMSE.gam,Chla2.RMSE.tvcm),
                         model=c(rep("1_MLR",10),rep("2_GLM",10),
                                 rep("3_GAM",10),rep("4_TVCM",10)))
ggplot(Chla2.RMSE, aes(x=model, y=RMSE, fill=model)) + geom_boxplot() + 
  ggtitle("Uchi Chla")

