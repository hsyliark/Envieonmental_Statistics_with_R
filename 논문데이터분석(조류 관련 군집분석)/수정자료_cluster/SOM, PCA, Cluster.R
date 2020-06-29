## PCA
data1 <- read.csv("C:/Users/Nier/Desktop/¼öÁ¤ÀÚ·á_cluster/°á°ú1/SOM ºÐ¼®¿ë_17_LKL_¼öÁ¤.csv", sep=",", header=T)
data1_name <- data1[,1]
data1 <- data1[,-1]
rownames(data1) <- data1_name
data2 <- read.csv("C:/Users/Nier/Desktop/¼öÁ¤ÀÚ·á_cluster/°á°ú2/SOM ºÐ¼®¿ë_Total_AF_16_¼öÁ¤.csv", sep=",", header=T)
data2_name <- data2[,1]
data2 <- data2[,-1]
rownames(data2) <- data2_name
data3 <- read.csv("C:/Users/Nier/Desktop/¼öÁ¤ÀÚ·á_cluster/°á°ú3/SOM ºÐ¼®¿ë_Total_BF_11_¼öÁ¤.csv", sep=",", header=T)
data3_name <- data3[,1]
data3 <- data3[,-1]
rownames(data3) <- data3_name


## Principal Component Analysis

data1_scale <- scale(data1)
data2_scale <- scale(data2)
data3_scale <- scale(data3)

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)

# Descriptive statistics
describe(data3_scale)

# Correlation
round(cor(data3_scale),3) # pearson
round(cor(data3_scale, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(data3_scale)
cortest.bartlett(cor(data3_scale, method="spearman"), n=nrow(data3_scale))

# Number of principal components 
water_pca <- prcomp(data3_scale, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

# Component matrix 
PCA <- principal(data3_scale, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(data3_scale, nfactor=3, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot)


## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Install packages
install.packages("kohonen")
library(kohonen)

data_all_matrix <- as.matrix(data_all)

som_grid <- somgrid(xdim=40, ydim=40, topo="hexagonal")
som_model <- som(data_all_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(2,2))
for (i in 1:11) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))



## Cluster analysis
# reference1 : https://data-make.tistory.com/91
# reference2 : https://www.statmethods.net/advstats/cluster.html

data1 <- read.csv("C:/Users/Nier/Desktop/?ï¿½ï¿½ï¿??????ÍºÐ¼?(?????Ð¼??Ú·?)/??ï¿½ï¿½?Ú·?/????1/SOM ?Ð¼???_17_LKL_??ï¿½ï¿½.csv", sep=",", header=T)
data1_name <- data1[,1]
data1 <- data1[,-1]
rownames(data1) <- data1_name
data2 <- read.csv("C:/Users/Nier/Desktop/?ï¿½ï¿½ï¿??????ÍºÐ¼?(?????Ð¼??Ú·?)/??ï¿½ï¿½?Ú·?/????2/SOM ?Ð¼???_Total_AF_16_??ï¿½ï¿½.csv", sep=",", header=T)
data2_name <- data2[,1]
data2 <- data2[,-1]
rownames(data2) <- data2_name
data3 <- read.csv("C:/Users/Nier/Desktop/?ï¿½ï¿½ï¿??????ÍºÐ¼?(?????Ð¼??Ú·?)/??ï¿½ï¿½?Ú·?/????3/SOM ?Ð¼???_Total_BF_11_??ï¿½ï¿½.csv", sep=",", header=T)
data3_name <- data3[,1]
data3 <- data3[,-1]
rownames(data3) <- data3_name

data1_matrix <- as.matrix(data1)
data2_matrix <- as.matrix(data2)
data3_matrix <- as.matrix(data3)

# Distance matrix
data1_scale <- scale(data1)
d <- dist(data1_scale, method="euclidean")
as.matrix(d)[1:19,1:19]

# Apply Distance matrix model
fit <- hclust(d, method="ward.D")
plot(fit)

# Decide number of clusters
install.packages("NbClust")
library(NbClust)
nc <- NbClust(data1_scale, distance="euclidean", method="ward.D")
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]), xlab="Number of clusters", ylab="Number of criteria", 
        main="Number of clusters chosen by 26 criteria")

par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=3)



## SOM cluster
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

# Install packages
install.packages("SOMbrero")
library(SOMbrero)
install.packages("kohonen")
library(kohonen)

# Normalization of data
data1_scale <- data.frame(scale(data1))
data1_scale_matrix <- as.matrix(data1_scale)

# Training the SOM model
som_grid <- somgrid(xdim=1, ydim=3, topo="hexagonal")
som_model1 <- som(data1_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=data1_scale, dimension=c(3,1),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")
str(som_model2)

# Visualization
par(mfrow=c(1,1))
plot(som_model1, main="feature distribution")
table(som_model2$clustering)
plot(som_model2, what="prototypes", type="umatrix", print.title=T)
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
plot(som_model1, type="counts", main="cluster size")

par(mfrow=c(1,3))
plot(som_model1, main="feature distribution")
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
par(mfrow=c(1,1))

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")

