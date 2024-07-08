### reference 1 : https://nbisweden.github.io/workshop-r/2011/lab_ggplot2.html
### reference 2 : https://www.kaggle.com/code/leolcling/visualizing-iris-datasets-with-r-ggplot2

install.packages("ggplot2")
library(ggplot2)

## Load 'iris' dataset

data("iris")
head(iris)

## Visualization with package 'ggplot2'

# Scatter plot

ggplot(data=iris) +
  geom_point(mapping=aes(x=Petal.Length, y=Petal.Width))

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species)) +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species), size=3) +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species, size=Sepal.Width)) +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species, size=Sepal.Width)) +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("red", "blue", "green"))

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Sepal.Width)) +
  geom_smooth(method="lm")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Sepal.Width)) +
  geom_smooth(method="lm") +
  scale_color_continuous(name="New Legend Title") +
  labs(title="This Is A Title", subtitle="This is a subtitle",
       x="Petal Length", y="Petal Width", 
       caption="This is a little caption.")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Sepal.Width)) +
  geom_smooth(method="lm") +
  scale_color_continuous(name="New Legend Title") +
  scale_x_continuous(breaks=1:8) +
  labs(title="This Is A Title",subtitle="This is a subtitle",
       x="Petal Length", y="Petal Width", 
       caption="This is a little caption.")

ggplot(data=iris, mapping=aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Sepal.Width)) +
  geom_smooth(method="lm") +
  scale_color_continuous(name="New Legend Title") +
  scale_x_continuous(breaks=1:8) +
  labs(title="This Is A Title", subtitle="This is a subtitle",
       x="Petal Length", y="Petal Width", 
       caption="This is a little caption.") +
  facet_wrap(~Species)

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + 
  geom_point() +geom_smooth() + theme_minimal()

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + 
  geom_point() +geom_smooth() + theme_minimal() +
  facet_wrap(~Species)

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + 
  geom_point() +geom_smooth() + theme_minimal() +
  facet_wrap(~Species, scale="free_y")

# boxplot

ggplot(data=iris, aes(x=Species, y=Petal.Length, color=Species)) + 
  geom_boxplot() + theme_minimal() +
  theme(legend.position="none")

ggplot(data=iris, aes(x=Species, y=Petal.Length, color=Species)) + 
  geom_violin() + theme_minimal() +
  theme(legend.position="none")

# histogram

ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) + 
  geom_histogram() +theme_minimal()

ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) + 
  geom_histogram() +theme_minimal() +
  facet_wrap(~Species)

# density plot

ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) + 
  geom_density() + theme_minimal()

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +
  geom_density2d() + theme_minimal()





### reference 3 : https://www.rpubs.com/StephanieStallworth/269560
### reference 4 : https://www.rpubs.com/Aakansha_garg/261616
### reference 5 : https://cozydatascientist.tistory.com/77
### reference 6 : https://topepo.github.io/caret/visualizations.html


## Load 'iris' dataset

# Install packages
install.packages("caret")
library(caret)

# Attach iris data set to environment
data(iris)

# Rename data set
dataset <- iris


## Create training and validation datasets

# Create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)

# Select 20% of the data for validation
validation <- dataset[-validation_index, ]

# Use the remaining 80% of data to train and test the models
dataset <- dataset[validation_index, ]


## Summarize Dataset

# Correlation plot
install.packages("corrplot")
library(corrplot)
M <- cor(iris[,1:4])
corrplot(M, method="circle")

# Dataset dimensions
dim(dataset)

# List types for each attribute
sapply(dataset,class)

# View first five rows of the data
head(dataset)

# Levels of the Factor Variables
levels(dataset$Species)

# Summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

# Summarize attribute distributions
summary(dataset)


## Visualize Dataset

# Split input and output
x <- dataset[ ,1:4]
y <- dataset[ ,5]

# Boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# Install packages
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# Barplot of class breakdown
dataset %>% 
  ggplot(aes(x=y, fill=y)) + geom_bar() +
  labs(x = "Iris Flower Species")

# box and whisker plots for each attribute
library(caret)
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
transparentTheme(trans = .9)
featurePlot(x=x, y=y,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2, 2), 
            auto.key = list(columns = 3))

# scatterplot matrix
transparentTheme(trans = .4)
featurePlot(x=x, y=y, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))


## Statistical Machine Learning Algorithm Evaluation

# step 1 : Set-up the test harness to use 10-fold cross validation
# step 2 : Build 5 different models to predict species from flower measurements
# step 3 : Select the best model

# Linear Discriminant Analysis (LDA)
# Classification and Regression Trees (CART)
# k-Nearest Neighbors (kNN)
# Support Vector Machines (SVM) with a linear kernel
# Random Forest (RF)

# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# Linear Algorithms: LDA
# Linear Discriminant Analysis (LDA)  
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# Nonlinear Algorithms: CART and kNN
# Classification and Regression Trees (CART)
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# k-Nearest Neighbors (kNN)
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

# Advanced Algorithms: SVM and RF
# Support Vector Machines (SVM)
install.packages("kernlab")
library(kernlab)
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest (RF)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# Select Best Model
# Summarize model accuracy for each model
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# Compare accuracy of models
dotplot(results)
# Summarize Best Model
print(fit.lda)
print(fit.knn)


## Make Predictions

# Estimate skill of LDA on the validation dataset
predictions1 <- predict(fit.lda, validation)
confusionMatrix(predictions1, validation$Species)

predictions2 <- predict(fit.knn, validation)
confusionMatrix(predictions2, validation$Species)




### reference 7 : https://cozydatascientist.tistory.com/77

## Deep Learning with iris data (for classification)

# install packages
install.packages("keras")
library(keras)

# install tensorflow
install_keras()

# loading data
data(iris)
iris <- iris

# Change data frame to matrix
iris[,5] <- as.numeric(as.factor(unlist(iris[,5]))) -1
iris <- as.matrix(iris)
dimnames(iris) <- NULL

# normalize data and remake data matrix
iris_x <- scale(iris[,1:4])
iris_mat <- cbind(iris_x, iris[,5])
head(iris_mat)

# seperate data set
ind <- sample(2, nrow(iris_mat), replace=TRUE, prob=c(0.67, 0.33))

# model matrix
iris.training <- iris_mat[ind==1, 1:4]
iris.test <- iris_mat[ind==2, 1:4]

# model predictive variable
iris.trainingtarget <- iris_mat[ind==1, 5]
iris.testtarget <- iris_mat[ind==2, 5]

# One-hot-encoding (predictive variable)
iris.trainLabels <- to_categorical(iris.trainingtarget)
iris.testLabels <- to_categorical(iris.testtarget)

# make Deep Learning model
set.seed(777)

model <- keras_model_sequential()
model %>%   
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>%   
  layer_dense(units = 3, activation = 'softmax')

summary(model)

get_config(model)

get_layer(model, index = 1)

model$layers

model$inputs

model$outputs

model %>% compile(  
  loss = 'categorical_crossentropy',  
  optimizer = 'adam',  
  metrics = 'accuracy')

model %>% fit(  
  iris.training,   
  iris.trainLabels,   
  epochs = 500,   
  batch_size = 5,  
  validation_split = 0.1)

classes <- model %>% 
  predict_classes(iris.test, batch_size = 128)
table(iris.testtarget, classes)

score <- model %>% 
  evaluate(iris.test, iris.testLabels, batch_size = 128)
print(score)