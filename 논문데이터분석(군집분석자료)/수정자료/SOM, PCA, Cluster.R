## PCA
data_all <- read.csv("C:/Users/Nier/Desktop/?占쏙옙占????첬劇?_hsy/?占쏙옙占??????故劇?(Evaluation and Grade Classification of River Water Quality Using Statistical Techniques)/?玲??克劇?.csv", sep=",", header=T)
data_all_scale <- scale(data_all)

## Principal Component Analysis

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)

# Descriptive statistics
describe(data_all_scale)

# Correlation
round(cor(data_all_scale),3) # pearson
round(cor(data_all_scale, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(data_all_scale)
cortest.bartlett(cor(data_all_scale, method="spearman"), n=nrow(data_all_scale))

# Number of principal components 
water_pca <- prcomp(data_all_scale, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

# Component matrix 
PCA <- principal(data_all_scale, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(data_all_scale, nfactor=3, rotate="varimax", score=T) # varimax rotate 
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

data1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(군집분석자료)/수정자료/결과1/SOM 분석용_17_LKL_수정.csv", sep=",", header=T)
data1_name <- data1[,1]
data1 <- data1[,-1]
rownames(data1) <- data1_name
data2 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(군집분석자료)/수정자료/결과2/SOM 분석용_Total_AF_16_수정.csv", sep=",", header=T)
data2_name <- data2[,1]
data2 <- data2[,-1]
rownames(data2) <- data2_name
data3 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(군집분석자료)/수정자료/결과3/SOM 분석용_Total_BF_11_수정.csv", sep=",", header=T)
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
data3_scale <- data.frame(scale(data3))
data3_scale_matrix <- as.matrix(data3_scale)

# Training the SOM model
som_grid <- somgrid(xdim=1, ydim=3, topo="hexagonal")
som_model1 <- som(data3_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=data3_scale, dimension=c(3,1),
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

