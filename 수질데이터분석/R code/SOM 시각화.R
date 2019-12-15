## Self Organizing Map 

# Data loading
df <- read.csv("C:/Users/HSY/Desktop/논문분석요청/전처리 후 데이터/SOM 패턴분석(영산포-1, 고막원천2-1).csv", 
               header=T, sep=",")
rownames(df) <- df[,1]
ECTN <- data.frame(df[,-1])

# Install packages
install.packages("SOMbrero")
library(SOMbrero)

# Normalization of data
water_scale <- data.frame(scale(water))

# Training the SOM model
model <- trainSOM(x.data=water_scale, dimension=c(5,5),
                  nb.save=10, maxit=1000, scaling="none", radius.type="letremy")
plot(model, what="energy")

# Visualization
table(model$clustering)
plot(model, what="prototypes", type="umatrix", print.title=T)
plot(model, what="prototypes", type="radar")
plot(model, what="obs", type="names", print.title=T, scale=c(1,1))

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")
