## Self Organizing Map 

# Data loading
region <- read.csv("C:/Users/HSY/Desktop/논문분석요청/전처리 후 데이터/지점별 평균 데이터.csv", 
               header=T, sep=",")
rownames(region) <- region[,1]
region <- data.frame(region[,-1])

# Install packages
install.packages("SOMbrero")
library(SOMbrero)

# Normalization of data
region_scale <- data.frame(scale(region))

# Training the SOM model
model <- trainSOM(x.data=region_scale, dimension=c(3,2),
                  nb.save=10, maxit=1000, scaling="none", radius.type="letremy")
par(mfrow=c(1,1))
plot(model, what="energy")

# Visualization
table(model$clustering)
plot(model, what="prototypes", type="umatrix", print.title=T)
plot(model, what="prototypes", type="radar")
plot(model, what="obs", type="names", print.title=T, scale=c(1,1))
plot(model, type="codes")

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")
