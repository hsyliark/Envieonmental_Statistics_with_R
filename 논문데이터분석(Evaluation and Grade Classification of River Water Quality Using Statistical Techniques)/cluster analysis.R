water <- read.csv("C:/Users/HSY/Desktop/논문분석자료 송부_hsy/군집분석.csv", sep=",", header=T)
water_name <- water[,1]
water <- water[,-1]
rownames(water) <- water_name


## Cluster analysis
# reference1 : https://data-make.tistory.com/91
# reference2 : https://www.statmethods.net/advstats/cluster.html

# Distance matrix
water_scale <- scale(water)
d <- dist(water_scale, method="euclidean")
as.matrix(d)[1:5,1:5]

# Apply Distance matrix model
fit <- hclust(d, method="ward.D")
plot(fit)

# Decide number of clusters
install.packages("NbClust")
library(NbClust)
nc <- NbClust(water_scale, distance="euclidean", method="ward.D")
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=2)
