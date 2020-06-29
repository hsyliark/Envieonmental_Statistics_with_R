watersom1 <- read.csv("C:/Users/HSY/Desktop/논문데이터분석(LASSO)/자료, 결과정리/유량(×)/csv파일/som1.csv", sep=",", header=T)
watersom2 <- read.csv("C:/Users/HSY/Desktop/논문데이터분석(LASSO)/자료, 결과정리/유량(×)/csv파일/som2.csv", sep=",", header=T)

## Self Organizing Map 
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

## Import data
water5 <- read.csv("C:/Users/Nier/Desktop/?????????ͺм?(?���)/2??°/?м??ڷ? ?ۺ?/5year/csv ????/0.??ü??��(5??).csv", header=T, sep=',')
water5_1 <- water5[,-(1:3)]
water5_1$logTC <- log(water5_1$TC)
water5_1$logFC <- log(water5_1$FC)
water5_1$logRain <- log(water5_1$Rain)
water5_1 <- water5_1[,-11]
water5_1 <- water5_1[,-17]
water5_1 <- water5_1[,-17] # water5_1 : ???????? (TC, FC, Rain ?? log ????, ???????跮 ??��)
water5_2 <- scale(water5_1) # water5_2 : ǥ??ȭ (?????м?)
water5_3 <- cbind(water5_2[,-(17:18)],water5_1[,17:18]) # water5_3 : ǥ??ȭ (logTC, logFC ��??, ȸ?ͺм?)


# Install packages
install.packages("SOMbrero")
library(SOMbrero)
install.packages("kohonen")
library(kohonen)

# Normalization of data
month_scale <- data.frame(scale(month))
month_scale_matrix <- as.matrix(month_scale)

# Training the SOM model
som_grid <- somgrid(xdim=3, ydim=4, topo="hexagonal")
som_model1 <- som(month_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=month_scale, dimension=c(4,3),
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
