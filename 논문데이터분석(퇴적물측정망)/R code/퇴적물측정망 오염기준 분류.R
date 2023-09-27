## Loading data
sediment <- read.csv("C:/Users/User/Desktop/sediment_read.csv", sep=",", header=T,
                     fileEncoding="CP949", encoding="UTF-8")

## Splitting data into train and test
sediment <- na.exclude(sediment)
sediment$pollution <- as.factor(sediment$pollution)
sedi_train <- na.exclude(sediment[(sediment$year != 2021) & (sediment$year != 2022), c(7:32)])
sedi_test <- na.exclude(sediment[(sediment$year == 2021) | (sediment$year == 2022), c(7:32)])

# Feature Scaling
train_scale <- scale(sedi_train[, 2:26])
test_scale <- scale(sedi_test[, 2:26])

## K-Nearest Neighbor (KNN)
library(e1071)
library(caTools)
library(class)

# to training dataset
classifier_knn <- knn(train=sedi_train,
                      test=sedi_test,
                      cl=sedi_train$pollution,
                      k=5)
classifier_knn




