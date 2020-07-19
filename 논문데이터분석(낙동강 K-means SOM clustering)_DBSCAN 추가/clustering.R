water <- read.csv("D:/Workplace/Environmental_Statistics_with_R/논문데이터분석(낙동강 K-means SOM clustering)/시간적 군집분석/낙동강 본류 분석(1, 16 제외)/본류취합(1, 16 제외).csv", sep=",", header=T)
water_name <- water[,1]
rownames(water) <- water_name
water <- water[,-1]
water_scale <- scale(water)
water_scale_t <- t(water_scale)
water_t <- as.data.frame(t(water))
water_scale_d <- as.data.frame(water_scale)
water_scale_td <- as.data.frame(water_scale_t)


water <- read.csv("C:/Users/Nier/Desktop/?���?????ͺм?(?????? K-means SOM clustering)_heatmap ??��/?????? ?????м?/????/csv????/????T-P.csv", sep=",", header=T)
water_name <- water[,1]
rownames(water) <- water_name
water <- water[,-1]
water_t <- as.data.frame(t(water))
water_t_name <- c(2005:2019)
rownames(water_t) <- water_t_name
water_t_rev <- water_t[nrow(water_t):1,]


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
install.packages("RColorBrewer")
library(RColorBrewer)
colors <- brewer.pal(5, "Blues")
superheat(water_t_rev, scale = FALSE, left.label.text.size=3,
          bottom.label.text.size=3, bottom.label.size = .05,
          row.dendrogram = FALSE, heat.pal = colors,
          title = "Water Quality Heatmap")



## Determining the optimal number of clusters
set.seed(1)
d <- dist(water_scale, method="euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=3, border = "red")

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
km <- kmeans(water_scale, centers=3)
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


ggplot(water_cluster, aes(x=X2005, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2006, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2007, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2008, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2009, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2010, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2011, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2012, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2013, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2014, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2015, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2016, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2017, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2018, fill=cluster)) +
  geom_density(alpha=0.5)
ggplot(water_cluster, aes(x=X2019, fill=cluster)) +
  geom_density(alpha=0.5)


## SOM clustering
install.packages("kohonen")
library(kohonen)
install.packages("SOMbrero")
library(SOMbrero)
water_scale_matrix <- as.matrix(water_scale)

# Training the SOM model
set.seed(1)
som_grid <- somgrid(xdim=3, ydim=1, topo="hexagonal")
som_model1 <- som(water_scale_matrix, grid=som_grid)
som_model2 <- trainSOM(x.data=water_scale_matrix, dimension=c(1,3),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")

# Visualization
plot(som_model1, main="feature distribution")
table(som_model2$clustering)
plot(som_model2, what="obs", type="names")
plot(som_model1, type="counts", main="cluster size")


## DBSCAN(Density-based spatial clustering of applications with noise) clustering
# reference : https://leedakyeong.tistory.com/entry/R-DBSCAN-%EC%BD%94%EB%93%9C-%EC%98%88%EC%8B%9C-%ED%95%B4%EC%84%9D-%ED%8C%8C%EB%9D%BC%EB%AF%B8%ED%84%B0-%EC%A1%B0%EC%A0%95-%EB%B0%A9%EB%B2%95-in-R
# reference : https://brunch.co.kr/@gimmesilver/40
install.packages("fpc")
library(fpc)
install.packages("ggplot2")
library(ggplot2)
install.packages("cluster")
library(cluster)
# select parameter (eps, MinPts)
dis_eps <- function(dat, a, b, c, d, e, f, g, h, i, j){
  sqrt((dat[1]-a)^2 + (dat[2]-b)^2 + (dat[3]-c)^2 +
         (dat[4]-d)^2 + (dat[5]-e)^2 + (dat[6]-f)^2 +
         (dat[7]-g)^2 + (dat[8]-h)^2 + (dat[9]-i)^2 +
         (dat[10]-j)^2)
}
k <- 3
dis <- c()
for(m in 1:nrow(water_scale)){
  dis <- c(dis,sort(apply(water_scale, 1, dis_eps, 
                          a=water_scale[m,1], b=water_scale[m,2],
                          c=water_scale[m,3], d=water_scale[m,4],
                          e=water_scale[m,5], f=water_scale[m,6],
                          g=water_scale[m,7], h=water_scale[m,8],
                          i=water_scale[m,9], j=water_scale[m,10]))[k+1])
}
sort(dis, decreasing = T)
ggplot() +
  geom_point(aes(x=1:length(dis), y=sort(dis, decreasing = T))) +
  theme_bw()
# clustering
db <- dbscan(water_scale, eps=2.517006, MinPts=2)
db
db$cluster

clusplot(water_scale, db$cluster)

water_cluster <- water
water_cluster$cluster <- as.character(db$cluster)
# water_cluster <- water_cluster[-c(3),]

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



## GMM(Gaussian Mixture Model) clustering
# reference : https://bradleyboehmke.github.io/HOML/model-clustering.html
# reference : https://rpackage.blogspot.com/2018/04/mclust.html
# reference : http://blog.naver.com/PostView.nhn?blogId=sw4r&logNo=221033034225
# reference : https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
install.packages("mclust")
library(mclust)
# select number
waterBIC <- mclustBIC(water_scale)
waterBIC
plot(waterBIC)
# clustering
watermod <- Mclust(water_scale)
summary(watermod, parameters = TRUE)
waterclassify <- cbind(water, watermod$classification)
waterclassify

clusplot(water_scale, watermod$classification)

water_cluster <- water
water_cluster$cluster <- as.character(db$cluster)

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


