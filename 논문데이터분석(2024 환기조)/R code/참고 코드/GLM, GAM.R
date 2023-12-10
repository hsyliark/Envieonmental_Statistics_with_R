cell_all <- read.csv("C:/Users/User/Desktop/논문데이터/cell_all.csv",sep=",",header=T)
# cell_all <- cell_all[cell_all$spot=="J1" | cell_all$spot=="J2",]
# cell_all <- cell_all[cell_all$spot=="J1",]

X <- cell_all[,9:25]
X_scale <- scale(X)
cell0 <- as.factor(ifelse(cell_all$Cyanophytes==0,"YES","NO"))
cell_all_scale <- cbind(cell_all[,1:8],cell0,X_scale)

train <- cell_all_scale[cell_all_scale$year != 2022,] 
test <- cell_all_scale[cell_all_scale$year == 2022,]


Cyanophytes <- test$Cyanophytes
date <- test$date ; spot <- test$spot

# Generalized Linear Model

# reference : https://rfriend.tistory.com/490
glm.D93 <- glm(Cyanophytes ~ BOD + COD + TN + TP + TOC +
                 SS + EC + pH + DO + Temperature +
                 Turbidity + Transparency + Chla + LowWaterLevel + Inflow +
                 Discharge + Reservoir, data=train, family = poisson(link=log)) # Poisson Regression Model (Variance = Mean)
summary(glm.D93)
step.glm.D93 <- step(glm.D93,direction="both") # stepwise regression
summary(step.glm.D93)
library(car)
vif(step.glm.D93)
PRM <- predict(step.glm.D93, newdata=test, type="response")
PRM






# reference : https://www.youtube.com/watch?v=Scr2uQqLkjI
library(MASS)
glm.D94 <- glm.nb(Cyanophytes ~ BOD + COD + TN + TP + TOC +
                    SS + EC + pH + DO + Temperature +
                    Turbidity + Transparency + Chla + LowWaterLevel + Inflow +
                    Discharge + Reservoir, data=train, link=log) # Negative Binomial Regression Model (Variance > Mean)

glm.D94 <- glm(Cyanophytes ~ BOD + COD + TN + TP + TOC +
                 SS + EC + pH + DO + Temperature +
                 Turbidity + Transparency + Chla + LowWaterLevel + Inflow +
                 Discharge + Reservoir, data=train, family = negative.binomial(1000))
summary(glm.D94)
step.glm.D94 <- step(glm.D94,direction="both")
summary(step.glm.D94)
vif(step.glm.D94)
NBRM <- predict(step.glm.D94, newdata=test, type="response")
NBRM


# Zero-Inflated
# reference : https://m.blog.naver.com/ollehw/221581563165
library(ggplot2)
library(pscl)
library(boot)

ggplot(cell_all_scale, aes(Cyanophytes)) + geom_histogram()

zip_model <- zeroinfl(Cyanophytes ~ BOD + COD + TN + TP + TOC +
                        SS + EC + pH + DO + Temperature +
                        Turbidity + Transparency + Chla + LowWaterLevel + Inflow +
                        Discharge + Reservoir, data=train, link="logit", dist="poisson")
summary(zip_model)
library(mpath)
step.zip_model <- be.zeroinfl(zip_model, data=train, dist="poisson", alpha=0.05, trace=TRUE) # stepwise regression
summary(step.zip_model)
vif(step.zip_model)
ZIPM <- predict(step.zip_model, newdata=test, type="response")
ZIPM

zin_model <- zeroinfl(Cyanophytes ~ BOD + COD + TN + TP + TOC +
                        SS + EC + pH + DO + Temperature +
                        Turbidity + Transparency + Chla + LowWaterLevel + Inflow +
                        Discharge + Reservoir, data=train, link="logit", dist="negbin")
summary(zin_model)
step.zin_model <- be.zeroinfl(zin_model, data=train, dist="negbin", alpha=0.05, trace=TRUE) # stepwise regression
summary(step.zin_model)
vif(step.zin_model)
ZINBM <- predict(step.zin_model, newdata=test, type="response")
ZINBM



# Generalized Additive Model

library(mgcv)
library(gam)

train_a <- train[,10:26]
gam.D93 <- gam::gam(Cyanophytes ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(Temperature) +
                      s(Turbidity) + s(Transparency) + s(Chla) + s(LowWaterLevel) + s(Inflow) +
                      s(Discharge) + s(Reservoir), data=train, family=poisson, link=log)
summary(gam.D93)
step.gam.D93 <- step.Gam(gam.D93,direction="both",scope=gam.scope(train_a)) # stepwise regression in GAM
summary(step.gam.D93)
vif(step.gam.D93)
GAPM <- predict(step.gam.D93, newdata=test, type="response")
GAPM

gam.D94 <- gam::gam(Cyanophytes ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(Temperature) +
                      s(Turbidity) + s(Transparency) + s(Chla) + s(LowWaterLevel) + s(Inflow) +
                      s(Discharge) + s(Reservoir), data=train, family=nb, link=log)
summary(gam.D94)
vif(gam.D94)
step.gam.D94 <- step.Gam(gam.D94,direction="both",scope=gam.scope(train_a))
vif(step.gam.D94)
predict(step.gam.D94, newdata=test, type="response")
GANBM <- predict(gam.D94, newdata=test, type="response")
GANBM


### Random Forest (For classification that cell count of Cyanophytes is 0?)
# reference : https://data-make.tistory.com/81
library(randomForest)

RF_model <- randomForest(cell0 ~ BOD + COD + TN + TP + TOC +
                           SS + EC + pH + DO + Temperature + Turbidity +
                           Transparency + Chla + LowWaterLevel + Inflow +
                           Discharge + Reservoir, data = train,
                         ntree = 1000)
RF_model
RF_model$predict
RF_model$importance
layout(matrix(c(1,2),nrow=1),width=c(4,1)) 

par(mar=c(5,4,4,7)) 
plot(RF_model)
par(mar=c(5,4,4,1),new=T) 
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("topright", colnames(RF_model$err.rate),col=1:5,cex=0.8,fill=1:5)
par(mar=c(5,4,4,2))

pred_forest <- predict(RF_model, newdata = test, type = 'class')
cm <- confusionMatrix(pred_forest, test$cell0)
cm
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]
cm$byClass["Specificity"]
cm$byClass["Precision"]
cm$byClass["Recall"]
mean(pred_forest != test$cell0) # misclassification rate





# Extreme Gradient Boosting (XGBoost) (Regression after classification with Random Forest)
# reference : https://www.projectpro.io/recipes/apply-xgboost-r-for-regression

library(xgboost)
library(caret)

#define predictor and response variables in training set
train_x = data.matrix(train[,10:26])
train_y = train[,5]

#define predictor and response variables in testing set
test_x = data.matrix(test[,10:26])
test_y = test[,5]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 4, verbose = 0)

summary(model_xgboost)

#use model to make predictions on test data
pred_xgb <- ifelse(pred_forest=="NO",predict(model_xgboost, xgb_test),0) 
XGBRF = ifelse(pred_xgb<0,0,pred_xgb)
XGBRF






# observed vs predicted
res <- data.frame(date=date, spot=spot, 
                  Cyanophytes=ifelse(Cyanophytes >= 0 & Cyanophytes < exp(1),Cyanophytes^(1/1000),log(Cyanophytes)), 
                  PRM=ifelse(PRM >= 0 & PRM < exp(1),PRM^(1/1000),log(PRM)), 
                  NBRM=ifelse(NBRM >= 0 & NBRM < exp(1),NBRM^(1/1000),log(NBRM)), 
                  ZIPM=ifelse(ZIPM >= 0 & ZIPM < exp(1),ZIPM^(1/1000),log(ZIPM)), 
                  ZINBM=ifelse(ZINBM >= 0 & ZINBM < exp(1),ZINBM^(1/1000),log(ZINBM)),
                  GAPM=ifelse(GAPM >= 0 & GAPM < exp(1),GAPM^(1/1000),log(GAPM)), 
                  GANBM=ifelse(GANBM >= 0 & GANBM < exp(1),GANBM^(1/1000),log(GANBM)), 
                  XGBRF=ifelse(XGBRF >= 0 & XGBRF < exp(1),XGBRF^(1/1000),log(XGBRF)))
res <- data.frame(date=date, spot=spot, 
                  Cyanophytes=Cyanophytes, 
                  PRM=PRM, 
                  NBRM=NBRM, 
                  ZIPM=ZIPM, 
                  ZINBM=ZINBM,
                  GAPM=GAPM, 
                  GANBM=GANBM, 
                  XGBRF=XGBRF)
res_J1 <- res[res$spot == "J1",c(-2)]
res_J2 <- res[res$spot == "J2",c(-2)]
res_T1 <- res[res$spot == "T1",c(-2)]
res_T2 <- res[res$spot == "T2",c(-2)]

library(reshape2)
res_J1_melt <- melt(res_J1,
                 id.vars = 'date',
                 variable.name = "model",
                 value.name = "cells") 
res_J1_melt$date <- as.factor(res_J1_melt$date)

res_J2_melt <- melt(res_J2,
                    id.vars = 'date',
                    variable.name = "model",
                    value.name = "cells") 
res_J2_melt$date <- as.factor(res_J2_melt$date)

res_T1_melt <- melt(res_T1,
                    id.vars = 'date',
                    variable.name = "model",
                    value.name = "cells") 
res_T1_melt$date <- as.factor(res_T1_melt$date)

res_T2_melt <- melt(res_T2,
                    id.vars = 'date',
                    variable.name = "model",
                    value.name = "cells") 
res_T2_melt$date <- as.factor(res_T2_melt$date)

library(ggplot2)
ggplot(res_J1_melt, aes(x=date, y=cells, group=model, color=model)) +
  geom_line(size=1) +
  coord_cartesian(ylim = c(0,50)) +
  ggtitle("J1 spot (Cell count -> cells/mL)") +
  labs(x ="date", y = "Predicted value of log(cell count)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        title = element_text(size=20, face='bold'), 
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"))

ggplot(res_J2_melt, aes(x=date, y=cells, group=model, color=model)) +
  geom_line(size=1) +
  coord_cartesian(ylim = c(0,25)) +
  ggtitle("J2 spot (Cell count -> cells/mL)") +
  labs(x ="date", y = "Predicted value of log(cell count)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        title = element_text(size=20, face='bold'),
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"))
  
ggplot(res_T1_melt, aes(x=date, y=cells, group=model, color=model)) +
  geom_line(size=1) +
  coord_cartesian(ylim = c(0,80)) +
  ggtitle("T1 spot (Cell count -> cells/mL)") +
  labs(x ="date", y = "Predicted value of log(cell count)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        title = element_text(size=20, face='bold'),
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold")) 

ggplot(res_T2_melt, aes(x=date, y=cells, group=model, color=model)) +
  geom_line(size=1) +
  coord_cartesian(ylim = c(0,60)) +
  ggtitle("T2 spot (Cell count -> cells/mL)") +
  labs(x ="date", y = "Predicted value of log(cell count)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        title = element_text(size=20, face='bold'),
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold")) 








# Long Short Term Memory (LSTM)
# reference : https://www.datatechnotes.com/2019/01/regression-example-with-lstm-networks.html

library(keras)
library(tensorflow)
use_condaenv("keras-tf", required = T)
library(dplyr)
library(caret)

install.packages("devtools")
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")


# Example

N = 400
step = 2
set.seed(123)
n = seq(1:N)
a = n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)
a = c(a,replicate(step,tail(a,1)))


x = NULL
y = NULL

for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

X = array(x, dim=c(N,step,1))

model = keras_model_sequential() %>% 
  layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>% 
  layer_dense(units=32) %>% 
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()

model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE, verbose=0)
y_pred  =  model %>% predict(X)

scores  =  model %>% evaluate(X, y, verbose = 0)
print(scores)






