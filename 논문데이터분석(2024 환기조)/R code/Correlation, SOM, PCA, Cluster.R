### For data by day

## Loading data
water_day <- read.csv("C:/Users/User/Desktop/2024 환경기초조사사업/데이터 수집/일자료/2012-2023 일자료.csv", 
                      sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")

## Correlation analysis
# reference : https://rpubs.com/Alema/1000474

# Correlation
round(cor(water_day[,5:18]),3) # pearson
round(cor(water_day[,5:18], method="spearman"),3) # spearman

library(ggcorrplot)

corr <- round(cor(water_day[,5:18], method="spearman"),3)
p_mat <- cor_pmat(corr)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
ggcorrplot(corr, hc.order = TRUE, type = "lower", p.mat = p_mat)

library(ggstatsplot)

ggstatsplot::ggcorrmat(
  data = water_day[,5:18],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

library(kohonen)

water_day_matrix <- as.matrix(water_day[,5:18])

som_grid <- somgrid(xdim=80, ydim=80, topo="hexagonal")
som_model <- som(water_day_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=2/3, alpha=alpha)[n:1]}

par(mfrow=c(2,2))
for (i in 1:14) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))





### For data by year average

## PCA
water <- read.csv("C:/Users/User/Desktop/2024 환경기초조사사업/데이터 수집/연평균/2023년 평균.csv", 
                     sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
water_name <- water[,1]
water <- water[,-1]
rownames(water) <- water_name

# Descriptive statistics
library(psych)
describe(water)

# KMO and Bartlett's test
# reference : https://blog.naver.com/bosangmaster/221173325133
KMO(water)
cortest.bartlett(cor(water, method="spearman"), n=nrow(water))

# Number of principal components 
water_pca <- prcomp(water, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

library(ggfortify)
autoplot(water_pca, data=water, label=TRUE, label.size=5,
         loadings=TRUE, loadings.colour='blue',
         loadings.label=TRUE, loadings.label.size=5)

# Component matrix 
PCA <- principal(water, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(water, nfactor=5, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot)

## Cluster analysis
# reference1 : https://data-make.tistory.com/91
# reference2 : https://www.statmethods.net/advstats/cluster.html
# reference3 : https://rfriend.tistory.com/585

# Distance matrix
water_scale <- scale(water)
d <- dist(water_scale, method="euclidean")
as.matrix(d)[1:5,1:5]

# Apply Distance matrix model
fit <- hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=2)

# Decide number of clusters
library(NbClust)
nc <- NbClust(water_scale, distance="euclidean", method="ward.D",
              min.nc=1, max.nc=14)
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=3)

## fviz_silhouette: Visualize Silhouette Information from Clustering
library(factoextra)
# K-means clustering
set.seed(1004)
km.res <- kmeans(water_scale, centers=2)
# Visualize silhouhette information
library(cluster)
sil <- silhouette(km.res$cluster, dist(water_scale, method="euclidean"))
fviz_silhouette(sil)


## SOM cluster
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

# Install packages
library(SOMbrero)
library(kohonen)

# Normalization of data
water_scale <- data.frame(scale(water))
water_scale_matrix <- as.matrix(water_scale)

# Training the SOM model
som_grid <- somgrid(xdim=2, ydim=1, topo="hexagonal")
som_model1 <- som(water_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=water_scale, dimension=c(2,1),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")
str(som_model2)

# Visualization
plot(som_model1, main="feature distribution")
table(som_model2$clustering)
plot(som_model2, what="prototypes", type="umatrix", print.title=T)
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
plot(som_model1, type="counts", main="cluster size")

par(mfrow=c(1,2))
plot(som_model1, main="feature distribution")
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
par(mfrow=c(1,1))

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")