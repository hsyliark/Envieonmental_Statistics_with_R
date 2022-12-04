### Loading data
T2 <- read.csv("C:/조류관련 자료/상수원(주암호, 탐진호)/csv 파일(조류 우점종 분류)/T2 spot.csv")

dom <- rep(NA,nrow(T2))
for (i in 1:nrow(T2)) {
  if (T2$dominant[i] == "blue") {
    dom[i] <- 1}
  else if (T2$dominant[i] == "diatom") {
    dom[i] <- 2}
  else if (T2$dominant[i] == "green") {
    dom[i] <- 3}
  else {dom[i] <- 4}
}
T2$dom <- dom
T2$dominant <- as.factor(T2$dominant)

# train : 2017~2021, test : 2022
train <- T2[T2$year != 2022,]
test <- T2[T2$year == 2022,]

table(train$dominant)
table(test$dominant)


# train : 2017~2019, test : 2020~2022
train <- T2[T2$year == 2017 | T2$year == 2018 | T2$year == 2019,]
test <- T2[T2$year == 2020 | T2$year == 2021 | T2$year == 2022,]

table(train$dominant)
table(test$dominant)


# train : 2017~2020, test : 2021
train <- T2[T2$year == 2017 | T2$year == 2018 | T2$year == 2019 | T2$year == 2020,]
test <- T2[T2$year == 2021,]

table(train$dominant)
table(test$dominant)


### Pattern analysis with Self Organizing Map

data_all <- read.csv("C:/조류관련 자료/상수원(주암호, 탐진호)/csv 파일(조류 우점종 분류)/data_all.csv")

library(kohonen)

set.seed(1234)

data_all_matrix <- as.matrix(data_all[,4:20])

som_grid <- somgrid(xdim=32, ydim=32, topo="hexagonal")
som_model <- som(data_all_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=2/3, alpha=alpha)[n:1]}

par(mfrow=c(2,3))
for (i in 1:17) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))



### Decision tree
# reference : https://woosa7.github.io/R-Classification-Decision-Tree/
# response variable : factor
library(party)
library(caret)

tree_model <- ctree(dominant ~ BOD + COD + T_N + T_P + TOC +
                      SS + EC + pH + DO + temperature + turbidity +
                      transparency + Chl_a + low + flow1 +
                      flow2 + reservoir, data = train)
tree_model
plot(tree_model)
plot(tree_model, type = "simple")
pred_tree <- predict(tree_model, newdata = test)
confusionMatrix(pred_tree, test$dominant) 
mean(pred_tree != test$dominant) # misclassification rate


### Bagging
# reference : https://todayisbetterthanyesterday.tistory.com/53
library(adabag)

bag_model <- bagging(dominant ~ BOD + COD + T_N + T_P + TOC +
                       SS + EC + pH + DO + temperature + turbidity +
                       transparency + Chl_a + low + flow1 +
                       flow2 + reservoir, data = train, mfinal=1000)
bag_model$importance
plot(bag_model$trees[[10]])
text(bag_model$trees[[10]])

pred_bag <- as.factor(predict(bag_model, newdata=test)$class)
confusionMatrix(pred_bag, test$dominant) 
mean(pred_bag != test$dominant) # misclassification rate


### AdaBoost
# reference : https://todayisbetterthanyesterday.tistory.com/53
library(adabag)

ada_model <- boosting(dominant ~ BOD + COD + T_N + T_P + TOC +
                        SS + EC + pH + DO + temperature + turbidity +
                        transparency + Chl_a + low + flow1 +
                        flow2 + reservoir, data = train, boos=TRUE, mfinal=1000)
ada_model$importance
plot(ada_model$trees[[10]])
text(ada_model$trees[[10]])

pred_ada <- as.factor(predict(ada_model, newdata=test)$class)
confusionMatrix(pred_ada, test$dominant) 
mean(pred_ada != test$dominant) # misclassification rate


### Gradient Boosting
# reference(Boosting) : https://hyunlee103.tistory.com/25
# reference(Gradient Boosting) : https://3months.tistory.com/368
# reference : https://datascienceplus.com/gradient-boosting-in-r/
library(gbm)
gbm_model <- gbm(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train,
                 distribution = "multinomial", n.trees = 1000,
                 shrinkage = 0.01, interaction.depth = 4)
summary(gbm_model)
predictions_gbm <- predict(gbm_model, newdata=test)
prd_g <- predictions_gbm[,,1]
pred_gbm <- rep(NA,nrow(test))
for (i in 1:nrow(test)) {
  if (which(prd_g[i,] == max(prd_g[i,])) == 1) {
    pred_gbm[i] <- "blue"}
  else if (which(prd_g[i,] == max(prd_g[i,])) == 2) {
    pred_gbm[i] <- "diatom"}
  else if (which(prd_g[i,] == max(prd_g[i,])) == 3) {
    pred_gbm[i] <- "green"}
  else {pred_gbm[i] <- "others"}
}
pred_gbm <- as.factor(pred_gbm)  

confusionMatrix(pred_gbm, test$dominant)  
mean(as.character(pred_gbm) != as.character(test$dominant)) # misclassification rate


### Random Forest
# reference : https://data-make.tistory.com/81
library(randomForest)

forest_model <- randomForest(dominant ~ BOD + COD + T_N + T_P + TOC +
                               SS + EC + pH + DO + temperature + turbidity +
                               transparency + Chl_a + low + flow1 +
                               flow2 + reservoir, data = train,
                             ntree = 1000)
forest_model
forest_model$predict
forest_model$importance
layout(matrix(c(1,2),nrow=1),width=c(4,1)) 

par(mar=c(5,4,4,7)) 
plot(forest_model)
par(mar=c(5,4,4,1),new=T) 
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("topright", colnames(forest_model$err.rate),col=1:5,cex=0.8,fill=1:5)
par(mar=c(5,4,4,2))

pred_forest <- predict(forest_model, newdata = test, type = 'class')
confusionMatrix(pred_forest, test$dominant) 
mean(pred_forest != test$dominant) # misclassification rate


### XGBoost
# reference : https://apple-rbox.tistory.com/6
# reference : https://dacon.io/codeshare/2480
# reference : https://wooono.tistory.com/97
# reference : https://www.projectpro.io/recipes/apply-xgboost-for-classification-r
# reference : http://contents2.kocw.or.kr/KOCW/document/2019/chungbuk/seonghyeongon1218/10.pdf
library(xgboost)
library(dplyr)
library(ggplot2)

# cross-validation for find best iteration
X <- train[,4:20] %>% data.matrix
y <- train$dominant 

set.seed(1234)

xgbcv_model <- xgb.cv(data = X, label = as.numeric(y)-1, num_class = levels(y) %>% length,
                    nfold = 5, nrounds = 200, early_stopping_rounds = 20,
                    objective = 'multi:softprob', eval_metric = 'mlogloss',
                    verbose = T, prediction = T)
xgbcv_model$evaluation_log
xgbcv_model$pred
pred_xgbcv <- xgbcv_model$pred %>% as.data.frame %>%
  mutate(pred = levels(y)[max.col(.)] %>% as.factor,actual = y)

cvplot <- function(model){ 
  eval.log = model$evaluation_log
  
  std = names(eval.log[,2]) %>% gsub('train_','',.) %>% gsub('_mean','',.)
  
  data.frame(error = c(unlist(eval.log[,2]),unlist(eval.log[,4])),
             class = c(rep('train',nrow(eval.log)),
                       rep('test',nrow(eval.log))),
             nround = rep(1:nrow(eval.log),2)
  ) %>%
    ggplot(aes(nround,error,col = class))+
    geom_point(alpha = 0.2)+
    geom_smooth(alpha = 0.4,se = F)+
    theme_bw()+
    ggtitle("XGBoost Cross-validation Visualization",
            subtitle = paste0('fold : ',length(model$folds),
                              '  iteration : ',model$niter
            )
    )+ylab(std)+theme(axis.title=element_text(size=11))
}
cvplot(xgbcv_model)

# Variable Importance with best iteration
imp_model <- xgboost(data = X, label = as.numeric(y)-1, num_class = levels(y) %>% length,
                nrounds = 8, objective = 'multi:softprob', verbose = F)
imp <- xgb.importance(model = imp_model)
imp
xgb.plot.importance(imp)

as.numeric(train[,21])-1

xgb_train <- xgb.DMatrix(data = data.matrix(train[,4:20]), label = as.numeric(train[,21])-1)
xgb_test <- xgb.DMatrix(data = data.matrix(test[,4:20]), label = as.numeric(test[,21])-1)

xgb_model <- xgb.train(data = xgb_train, 
                       nrounds= 8,  # Best iteration
                       objective= "multi:softprob", num_class=4,  
                       eval_metric= "mlogloss")
pred_xgbm <- predict(xgb_model, newdata = xgb_test, reshape=T)
pred_xgb <- rep(NA,nrow(test))
for (i in 1:nrow(test)) {
  if (which(pred_xgbm[i,] == max(pred_xgbm[i,])) == 1) {
    pred_xgb[i] <- "blue"}
  else if (which(pred_xgbm[i,] == max(pred_xgbm[i,])) == 2) {
    pred_xgb[i] <- "diatom"}
  else if (which(pred_xgbm[i,] == max(pred_xgbm[i,])) == 3) {
    pred_xgb[i] <- "green"}
  else {pred_xgb[i] <- "others"}
}
pred_xgb <- as.factor(pred_xgb)

confusionMatrix(pred_xgb, test$dominant) 
mean(pred_xgb != test$dominant) # misclassification rate


### Discriminant Analysis
# reference : http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/
library(tidyverse)
library(caret)

# 1) Linear discriminant analysis (LDA): Uses linear combinations of predictors to predict the class of a given observation. Assumes that the predictor variables (p) are normally distributed and the classes have identical variances (for univariate analysis, p = 1) or identical covariance matrices (for multivariate analysis, p > 1).
library(MASS)
lda_model <- lda(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train)
lda_model
plot(lda_model)

lda.data <- cbind(train, predict(lda_model)$x)

library(ggplot2)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = dominant))

predictions_lda <- lda_model %>% predict(test)
lda.data.test <- cbind(test, predictions_lda$x)
ggplot(lda.data.test, aes(LD1, LD2)) +
  geom_point(aes(color = dominant))
pred_lda <- predictions_lda$class

confusionMatrix(pred_lda, test$dominant) 
mean(pred_lda != test$dominant) # misclassification rate

# 2) Quadratic discriminant analysis (QDA): More flexible than LDA. Here, there is no assumption that the covariance matrix of classes is the same.
library(MASS)
qda_model <- qda(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train)
qda_model
predictions_qda <- qda_model %>% predict(test)
pred_qda <- predictions_qda$class

confusionMatrix(pred_qda, test$dominant)  
mean(pred_qda != test$dominant) # misclassification rate

# 3) Mixture discriminant analysis (MDA): Each class is assumed to be a Gaussian mixture of subclasses.
library(mda)
mda_model <- mda(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train)
mda_model
pred_mda <- mda_model %>% predict(test)

confusionMatrix(pred_mda, test$dominant) # misclassification rate = 0.4358974
mean(pred_mda != test$dominant)

# 4) Flexible Discriminant Analysis (FDA): Non-linear combinations of predictors is used such as splines.
library(mda)
fda_model <- fda(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train)
fda_model
pred_fda <- fda_model %>% predict(test)

confusionMatrix(pred_fda, test$dominant) 
mean(pred_fda != test$dominant) # misclassification rate 

# 5) Regularized discriminant anlysis (RDA): Regularization (or shrinkage) improves the estimate of the covariance matrices in situations where the number of predictors is larger than the number of samples in the training data. This leads to an improvement of the discriminant analysis.
library(klaR)
rda_model <- rda(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train)
rda_model
predictions_rda <- rda_model %>% predict(test)
pred_rda <- predictions_rda[["class"]]

confusionMatrix(pred_rda, test$dominant) 
mean(pred_rda != test$dominant) # misclassification rate 


### Support Vector Machine
# reference : https://blog.naver.com/PostView.nhn?blogId=pmw9440&logNo=221586667065
# reference : https://ratsgo.github.io/machine%20learning/2017/05/23/SVM/
library(e1071)
svm_model <- svm(dominant ~ BOD + COD + T_N + T_P + TOC +
                   SS + EC + pH + DO + temperature + turbidity +
                   transparency + Chl_a + low + flow1 +
                   flow2 + reservoir, data = train,  
                 type = "C-classification", kernel = "radial")
summary(svm_model)
pred_svm <- predict(svm_model, newdata = test)

confusionMatrix(pred_svm, test$dominant)  
mean(pred_svm != test$dominant) # misclassification rate


### Generalized Additive Multinomial Logistic Regression
# 1) Multinomial Logistic Regression
library(nnet)
mlr_model <- multinom(dominant ~ BOD + COD + T_N + T_P + TOC +
                        SS + EC + pH + DO + temperature + turbidity +
                        transparency + Chl_a + low + flow1 +
                        flow2 + reservoir, data = train)
summary(mlr_model)
pred_mlr <- predict(mlr_model, newdata=test)

confusionMatrix(pred_mlr, test$dominant) 
mean(pred_mlr != test$dominant) # misclassification rate

# 2) Generalized Additive Multinomial Logistic Regression
# reference : https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/multinom.html
library(mgcv)
gamlr_model <- gam(list(dominant ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a) + s(low) + s(flow1) +
                        s(flow2) + s(reservoir), 
                    ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a) + s(low) + s(flow1) +
                      s(flow2) + s(reservoir),
                    ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a) + s(low) + s(flow1) +
                      s(flow2) + s(reservoir)), 
               data = train,
               family = multinom(K=3))


### Deep Neural Network
# reference : http://bigdata.dongguk.ac.kr/lectures/dm/_book/%EB%94%A5%EB%9F%AC%EB%8B%9D-%EA%B8%B0%EC%B4%88-dnn-deep-neural-network.html
# reference : https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/
library(tensorflow)
library(keras)
library(deepNN)

# package 'neuralnet'
library(neuralnet)

softmax <- function(x) exp(x)/sum(exp(x))

set.seed(1234)

dnn_model <- neuralnet(dominant ~ BOD + COD + T_N + T_P + TOC +
                         SS + EC + pH + DO + temperature + turbidity +
                         transparency + Chl_a + low + flow1 +
                         flow2 + reservoir, data = train,
                       hidden = c(3,3),
                       linear.output = FALSE,
                       err.fct = "ce",
                       act.fct = "logistic")

plot(dnn_model)
predictions_dnn <- predict(dnn_model, newdata = test)
pred_dnn <- rep(NA,nrow(test))
for (i in 1:nrow(test)) {
  if (which(predictions_dnn[i,] == max(predictions_dnn[i,])) == 1) {
    pred_dnn[i] <- "blue"}
  else if (which(predictions_dnn[i,] == max(predictions_dnn[i,])) == 2) {
    pred_dnn[i] <- "diatom"}
  else if (which(predictions_dnn[i,] == max(predictions_dnn[i,])) == 3) {
    pred_dnn[i] <- "green"}
  else {pred_dnn[i] <- "others"}
}
pred_dnn <- as.factor(pred_dnn)

confusionMatrix(pred_dnn, test$dominant) 
mean(pred_dnn != test$dominant) # misclassification rate

# package 'mxnet'
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

