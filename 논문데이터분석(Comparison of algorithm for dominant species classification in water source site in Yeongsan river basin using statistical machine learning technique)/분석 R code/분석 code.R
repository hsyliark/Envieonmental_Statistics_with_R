### Loading data
T1 <- read.csv("C:/조류관련 자료/상수원(주암호, 탐진호)/csv 파일(조류 우점종 분류)/T1 spot.csv")
T1$dominant <- as.factor(T1$dominant)

dom <- rep(NA,nrow(T1))
for (i in 1:nrow(T1)) {
  if (T1$dominant[i] == "blue") {
    dom[i] <- 1}
  else if (T1$dominant[i] == "diatom") {
    dom[i] <- 2}
  else if (T1$dominant[i] == "green") {
    dom[i] <- 3}
  else {dom[i] <- 4}
}
T1$dom <- dom

train <- T1[T1$year != 2022,]
test <- T1[T1$year == 2022,]

table(train$dominant)
table(test$dominant)


### Pattern analysis with Self Organizing Map
library(kohonen)

data_all_matrix <- as.matrix(T1[,4:16])

som_grid <- somgrid(xdim=10, ydim=28, topo="hexagonal")
som_model <- som(data_all_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=2/3, alpha=alpha)[n:1]}

par(mfrow=c(1,1))
for (i in 1:13) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))



### Decision tree
# reference : https://woosa7.github.io/R-Classification-Decision-Tree/
# response variable : factor
library(party)
library(caret)

tree_model <- ctree(dominant ~ BOD + COD + T_N + T_P + TOC +
                      SS + pH + DO + temperature + turbidity +
                      transparency + Chl_a, data = train)
tree_model
plot(tree_model)
plot(tree_model, type = "simple")
pred_tree <- predict(tree_model, newdata = test)
confusionMatrix(pred_tree, test$dominant) 
mean(pred_tree != test$dominant) # misclassification rate


### Random Forest
# reference : https://data-make.tistory.com/81
library(randomForest)

forest_model <- randomForest(dominant ~ BOD + COD + T_N + T_P + TOC +
                           SS + pH + DO + temperature + turbidity +
                           transparency + Chl_a, data = train,
                           ntree = 500)
forest_model
forest_model$predict
forest_model$importance
plot(forest_model)
legend("center", colnames(forest_model$err.rate),col=1:5,cex=0.8,fill=1:5)

pred_forest <- predict(forest_model, newdata = test, type = 'class')
confusionMatrix(pred_forest, test$dominant) 
mean(pred_forest != test$dominant) # misclassification rate


### Discriminant Analysis
# reference : http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/
library(tidyverse)
library(caret)

# 1) Linear discriminant analysis (LDA): Uses linear combinations of predictors to predict the class of a given observation. Assumes that the predictor variables (p) are normally distributed and the classes have identical variances (for univariate analysis, p = 1) or identical covariance matrices (for multivariate analysis, p > 1).
library(MASS)
lda_model <- lda(dominant ~ BOD + COD + T_N + T_P + TOC +
               SS + pH + DO + temperature + turbidity +
               transparency + Chl_a, data = train)
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
               SS + pH + DO + temperature + turbidity +
               transparency + Chl_a, data = train)
qda_model
predictions_qda <- qda_model %>% predict(test)
pred_qda <- predictions_qda$class

confusionMatrix(pred_qda, test$dominant)  
mean(pred_qda != test$dominant) # misclassification rate

# 3) Mixture discriminant analysis (MDA): Each class is assumed to be a Gaussian mixture of subclasses.
library(mda)
mda_model <- mda(dominant ~ BOD + COD + T_N + T_P + TOC +
               SS + pH + DO + temperature + turbidity +
               transparency + Chl_a, data = train)
mda_model
pred_mda <- mda_model %>% predict(test)

confusionMatrix(pred_mda, test$dominant) # misclassification rate = 0.4358974
mean(pred_mda != test$dominant)

# 4) Flexible Discriminant Analysis (FDA): Non-linear combinations of predictors is used such as splines.
library(mda)
fda_model <- fda(dominant ~ BOD + COD + T_N + T_P + TOC +
               SS + pH + DO + temperature + turbidity +
               transparency + Chl_a, data = train)
fda_model
pred_fda <- fda_model %>% predict(test)

confusionMatrix(pred_fda, test$dominant) 
mean(pred_fda != test$dominant) # misclassification rate 

# 5) Regularized discriminant anlysis (RDA): Regularization (or shrinkage) improves the estimate of the covariance matrices in situations where the number of predictors is larger than the number of samples in the training data. This leads to an improvement of the discriminant analysis.
library(klaR)
rda_model <- rda(dominant ~ BOD + COD + T_N + T_P + TOC +
               SS + pH + DO + temperature + turbidity +
               transparency + Chl_a, data = train)
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
            SS + pH + DO + temperature + turbidity +
            transparency + Chl_a, data = train,  
            type = "C-classification", kernel = "radial")
summary(svm_model)
pred_svm <- predict(svm_model, newdata = test)

confusionMatrix(pred_svm, test$dominant)  
mean(pred_svm != test$dominant) # misclassification rate


### Gradient Boosting
# reference(Boosting) : https://hyunlee103.tistory.com/25
# reference(Gradient Boosting) : https://3months.tistory.com/368
# reference : https://datascienceplus.com/gradient-boosting-in-r/
library(gbm)
gbm_model <- gbm(dominant ~ BOD + COD + T_N + T_P + TOC +
          SS + pH + DO + temperature + turbidity +
          transparency + Chl_a, data = train,
          distribution = "multinomial", n.trees = 10000,
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


### Generalized Additive Multinomial Logistic Regression
# 1) Multinomial Logistic Regression
library(nnet)
mlr_model <- multinom(dominant ~ BOD + COD + T_N + T_P + TOC +
                  SS + pH + DO + temperature + turbidity +
                  transparency + Chl_a, data = train)
summary(mlr_model)
pred_mlr <- predict(mlr_model, newdata=test)

confusionMatrix(pred_mlr, test$dominant) 
mean(pred_mlr != test$dominant) # misclassification rate

# 2) Generalized Additive Multinomial Logistic Regression
# reference : https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/multinom.html
library(mgcv)
gamlr_model <- gam(list(dominant ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a), 
                    ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a),
                    ~ s(BOD) + s(COD) + s(T_N) + s(T_P) + s(TOC) +
                      s(SS) + s(pH) + s(DO) + s(temperature) + s(turbidity) +
                      s(transparency) + s(Chl_a)), 
               data = train,
               family = multinom(K=3))


# Deep Neural Network
# reference : http://bigdata.dongguk.ac.kr/lectures/dm/_book/%EB%94%A5%EB%9F%AC%EB%8B%9D-%EA%B8%B0%EC%B4%88-dnn-deep-neural-network.html
# reference : https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/
library(tensorflow)
library(keras)
library(neuralnet)

dnn_model <- neuralnet(dominant ~ BOD + COD + T_N + T_P + TOC +
                       SS + pH + DO + temperature + turbidity +
                       transparency + Chl_a, data = train,
                       hidden = c(3,2),
                       linear.output = FALSE,
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

