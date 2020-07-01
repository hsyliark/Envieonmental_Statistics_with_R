water <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(K-means SOM clustering)/낙동강 지류 분석/지류취합.csv", sep=",", header=T)
water_name <- water[,1]
rownames(water) <- water_name
water <- water[,-1]
water_scale <- scale(water)
water_scale_t <- t(water_scale)
water_t <- as.data.frame(t(water))
water_scale_d <- as.data.frame(water_scale)
water_scale_td <- as.data.frame(water_scale_t)



## reference : https://rkabacoff.github.io/datavis/Time.html
# heatmap
install.packages("superheat")
library(superheat)
superheat(water, scale = TRUE, left.label.text.size=3,
          bottom.label.text.size=3, bottom.label.size = .05,
          row.dendrogram = TRUE, title = "Water Quality Heatmap")
superheat(water, scale = TRUE, left.label.text.size=3,
          bottom.label.text.size=3, bottom.label.size = .05,
          row.dendrogram = FALSE, title = "Water Quality Heatmap")



## Determining the optimal number of clusters
set.seed(1)
d <- dist(water_scale, method="euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=4, border = "red")

wss <- 0
for(i in 1:10) {wss[i] <- kmeans(water_scale, centers=i)$tot.withinss}
plot(1:10, wss, type="b", xlab="Number of clusters", 
     ylab="Within group sum of squares",
     main="Select the best number of clusters (K-means)")

install.packages("NbClust")
library(NbClust)
nc <- NbClust(water_scale, min.nc=2, max.nc=10, method="kmeans")
barplot(table(nc$Best.nc[1,]), xlab="Number of clusters", 
        ylab="Number of criteria", main="Number of clusters chosen by 26 criteria")



## K-means clustering
# reference : http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
install.packages("ggplot2")
library(ggplot2)
install.packages("cluster")
library(cluster)

set.seed(1)
km <- kmeans(water_scale, centers=4)
str(km)
km
clusplot(water_scale, km$cluster)

water_cluster <- water
water_cluster$cluster <- as.character(km$cluster)

ggplot(water_cluster, aes(x=BOD, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=COD, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=T.N, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=DTN, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=NO3.N, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=NH3.N, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=T.P, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=DTP, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=PO4.P, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=Chlorophyll.a, fill=cluster)) +
  geom_density(alpha=0.5)

x <- ggplot(cdata, aes(x=factor(1), fill=cluster))
x + geom_bar(width=1) + coord_polar(theta="y")



## SOM clustering
install.packages("kohonen")
library(kohonen)
install.packages("SOMbrero")
library(SOMbrero)
water_scale_matrix <- as.matrix(water_scale)

# Training the SOM model
set.seed(1)
som_grid <- somgrid(xdim=4, ydim=1, topo="hexagonal")
som_model1 <- som(water_scale_matrix, grid=som_grid)
som_model2 <- trainSOM(x.data=water_scale_matrix, dimension=c(1,4),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")

# Visualization
plot(som_model1, main="feature distribution")
table(som_model2$clustering)
plot(som_model2, what="obs", type="names")
plot(som_model1, type="counts", main="cluster size")


