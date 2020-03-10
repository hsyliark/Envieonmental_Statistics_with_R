## Reading data
water <- read.csv("C:/Users/Nier/Desktop/2020년 02월 수질측정망.csv",
                  sep=",", header=T)
View(water)

## Principal Component Analysis

# Install and Attach required library
install.packages("psych") 
library(psych)

# Bartlett's test
cortest.bartlett(cor(water), n=nrow(water))

# Number of principal components
water_pca <- prcomp(water, center=T, scale.=T)
water_pca
summary(water_pca)
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")

