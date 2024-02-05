### For data by day

## Loading data
water_day <- read.csv("C:/Users/Hi/Desktop/2024 ?™˜ê²½ê¸°ì´ˆì¡°?‚¬?‚¬?—…/?°?´?„° ?ˆ˜ì§?/231214/?¼?ë£?/2012-2023 ?¼?ë£?.csv",             
                      sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")

## Correlation analysis
# reference : https://rpubs.com/Alema/1000474

# Correlation
round(cor(water_day),3) # pearson
round(cor(water_day, method="spearman"),3) # spearman

library(ggcorrplot)

corr <- round(cor(water_day, method="spearman"),3)
p_mat <- cor_pmat(corr)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
ggcorrplot(corr, hc.order = TRUE, type = "lower", p.mat = p_mat)

library(ggstatsplot)

ggstatsplot::ggcorrmat(
  data = water_day,
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

## Self Organizing Map (Pattern)
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

library(kohonen)

water_day_matrix <- as.matrix(water_day)

som_grid <- somgrid(xdim=100, ydim=100, topo="hexagonal")
som_model <- som(water_day_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=2/3, alpha=alpha)[n:1]}

par(mfrow=c(2,2))
for (i in 1:14) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))




### For data by year average

## PCA

# by year average
water <- read.csv("C:/2024 Àú³Î ³í¹®/2024 È¯°æ±âÃÊÁ¶»ç»ç¾÷/µ¥ÀÌÅÍ ¼öÁı/240205/average 2019-2023_correct.csv",
                  sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
water_name <- water[,1]
water <- water[,-1]
water_scale <- data.frame(scale(water))
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
PCA_rot <- principal(water, nfactor=3, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot)

## Cluster analysis
# reference1 : https://data-make.tistory.com/91
# reference2 : https://www.statmethods.net/advstats/cluster.html
# reference3 : https://rfriend.tistory.com/585

# Decide number of clusters 1 (NbClust)
library(NbClust)
nc <- NbClust(water_scale, distance="euclidean", method="ward.D",
              min.nc=1, max.nc=13)
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=3)

# Decide number of clusters 2 (fviz_nbclust)
library(factoextra)
fviz_nbclust(water_scale, 
             FUNcluster = kmeans,
             method = c("silhouette"), diss = NULL,
             k.max = 10, nboot = 100, verbose = interactive(),
             barfill = "steelblue",
             barcolor = "steelblue",
             linecolor = "steelblue",
             print.summary = TRUE)

# Distance matrix
water_scale <- scale(water)
d <- dist(water_scale, method="euclidean")
as.matrix(d)[1:5,1:5]

# Apply Distance matrix model
## Hierarchical Cluster Analysis
# Decide number of clusters
library(factoextra)
fviz_nbclust(water_scale, 
             FUNcluster = hcut,
             method = c("silhouette"), diss = NULL,
             k.max = 10, nboot = 100, verbose = interactive(),
             barfill = "steelblue",
             barcolor = "steelblue",
             linecolor = "steelblue",
             print.summary = TRUE)
d <- dist(water_scale, method="euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=4)

## fviz_silhouette: Visualize Silhouette Information from Clustering
library(factoextra)
## K-means clustering
# Decide number of clusters
fviz_nbclust(water_scale, 
             FUNcluster = kmeans,
             method = c("silhouette"), diss = NULL,
             k.max = 10, nboot = 100, verbose = interactive(),
             barfill = "steelblue",
             barcolor = "steelblue",
             linecolor = "steelblue",
             print.summary = TRUE)
km.res <- kmeans(water_scale, centers=4)
km.res[["cluster"]]
# Visualize silhouhette information
library(cluster)
sil <- silhouette(km.res$cluster, dist(water_scale, method="euclidean"))
fviz_silhouette(sil)
# visualizing K-means clusters
fviz_cluster(km.res, data = water_scale,
             choose.vars = NULL, stand = TRUE,
             axes = c(1, 2), geom = c("point", "text"),
             repel = FALSE, show.clust.cent = TRUE,
             ellipse = TRUE, ellipse.type = "convex",
             ellipse.level = 0.95, ellipse.alpha = 0.2,
             shape = NULL, pointsize = 1.5,
             labelsize = 12, main = "Cluster plot",
             xlab = NULL, ylab = NULL,
             outlier.color = "black", outlier.shape = 19,
             outlier.pointsize = pointsize, outlier.labelsize = labelsize,
             ggtheme = theme_grey()
)

## Partitioning Around Medoids
# reference : https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/
library(cluster)
library(factoextra)
# Decide number of clusters
fviz_nbclust(water_scale, 
             FUNcluster = pam,
             method = c("silhouette"), diss = NULL,
             k.max = 10, nboot = 100, verbose = interactive(),
             barfill = "steelblue",
             barcolor = "steelblue",
             linecolor = "steelblue",
             print.summary = TRUE)
pam.res <- pam(water_scale, k=6)
print(pam.res)
# visualizing PAM clusters
fviz_cluster(pam.res, data = water_scale,
  choose.vars = NULL, stand = TRUE,
  axes = c(1, 2), geom = c("point", "text"),
  repel = FALSE, show.clust.cent = TRUE,
  ellipse = TRUE, ellipse.type = "convex",
  ellipse.level = 0.95, ellipse.alpha = 0.2,
  shape = NULL, pointsize = 1.5,
  labelsize = 12, main = "Cluster plot",
  xlab = NULL, ylab = NULL,
  outlier.color = "black", outlier.shape = 19,
  outlier.pointsize = pointsize, outlier.labelsize = labelsize,
  ggtheme = theme_grey()
  )

## Gaussian Mixture Model
# reference 1 : https://search.r-project.org/CRAN/refmans/ClusterR/html/GMM.html
# reference 2 : https://cran.r-project.org/web/packages/ClusterR/vignettes/the_clusterR_package.html
# Install packages
library(ClusterR)
dat <- as.matrix(water_scale)
dat <- center_scale(dat)
# Decide number of clusters
opt_gmm <- Optimal_Clusters_GMM(
  dat, max_clusters = 10, criterion = "AIC", 
  dist_mode = "maha_dist", seed_mode = "random_subset",
  km_iter = 10, em_iter = 10, var_floor = 1e-10, 
  plot_data = T)
opt_gmm <- Optimal_Clusters_GMM(
  dat, max_clusters = 10, criterion = "BIC", 
  dist_mode = "maha_dist", seed_mode = "random_subset",
  km_iter = 10, em_iter = 10, var_floor = 1e-10, 
  plot_data = T)
gmm <- GMM(dat, 5, dist_mode="maha_dist", seed_mode="random_subset", 
           km_iter=10, em_iter=10)
gmm[["Log_likelihood"]]

## SOM cluster
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

# Install packages
library(SOMbrero)
library(kohonen)

# Normalization of data
water_scale <- data.frame(scale(water))
water_scale_matrix <- as.matrix(water_scale)

# Training the SOM model
som_grid <- somgrid(xdim=5, ydim=1, topo="hexagonal")
som_model1 <- som(water_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=water_scale, dimension=c(5,1),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")
str(som_model2)

# Visualization
plot(som_model1, main="feature distribution")
som_model1[["unit.classif"]]
table(som_model2$clustering)
plot(som_model2, what="prototypes", type="umatrix", print.title=T)
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
som_model2[["clustering"]]
plot(som_model1, type="counts", main="cluster size")

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")

## Density plot with group
water$cluster <- c(5,5,2,2,3,1,4,1,4,1,4,3,4)
water$cluster <- as.factor(water$cluster)

library(ggplot2)
# BOD, COD, SS, TN, NO3N, NH3N, TP, PO4P, Chla, TOC
# animal, manure, land
ggplot(water, aes(x=land, fill=cluster)) +
  geom_density(alpha=0.4)


## PCA after clustering
k1 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/K-means/k1_2023.csv",  
                  sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
k1_name <- k1[,1]
k1 <- k1[,-1]
rownames(k1) <- k1_name

k2 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/K-means/k2_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
k2_name <- k2[,1]
k2 <- k2[,-1]
rownames(k2) <- k2_name

k3 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/K-means/k3_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
k3_name <- k3[,1]
k3 <- k3[,-1]
rownames(k3) <- k3_name

g1 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/GMM/g1_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
g1_name <- g1[,1]
g1 <- g1[,-1]
rownames(g1) <- g1_name

g2 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/GMM/g2_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
g2_name <- g2[,1]
g2 <- g2[,-1]
rownames(g2) <- g2_name

g3 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/GMM/g3_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
g3_name <- g3[,1]
g3 <- g3[,-1]
rownames(g3) <- g3_name

s1 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/SOM/s1_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
s1_name <- s1[,1]
s1 <- s1[,-1]
rownames(s1) <- s1_name

s2 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/SOM/s2_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
s2_name <- s2[,1]
s2 <- s2[,-1]
rownames(s2) <- s2_name

s3 <- read.csv("C:/Users/Hi/Desktop/2024 ???ê²½ê¸°ì´ˆì¡°?????????/????????? ??????/231227/CA/SOM/s3_2023.csv",  
               sep=",", header=T, fileEncoding = "CP949", encoding = "UTF-8")
s3_name <- s3[,1]
s3 <- s3[,-1]
rownames(s3) <- s3_name

# Number of principal components 
s3_pca <- prcomp(s3, center=T, scale.=T)
s3_pca
screeplot(s3_pca, type="l")
biplot(s3_pca, main="Biplot")
summary(s3_pca)
s3_pca$sdev^2 # Eigenvalue with respect to principal components

library(ggfortify)
autoplot(s3_pca, data=s3, label=TRUE, label.size=5,
         loadings=TRUE, loadings.colour='blue',
         loadings.label=TRUE, loadings.label.size=5)

# Component matrix 
PCA <- principal(s3, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(s3, nfactor=5, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot)