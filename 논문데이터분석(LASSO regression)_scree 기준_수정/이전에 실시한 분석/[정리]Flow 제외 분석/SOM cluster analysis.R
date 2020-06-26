## Self Organizing Map 
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

## Import data
water5 <- read.csv("C:/Users/Nier/Desktop/수질데이터분석(논문)/2번째/분석자료 송부/5year/csv 파일/0.전체지점(5년).csv", header=T, sep=',')
water5_1 <- water5[,-(1:3)]
water5_1$logTC <- log(water5_1$TC)
water5_1$logFC <- log(water5_1$FC)
water5_1$logRain <- log(water5_1$Rain)
water5_1 <- water5_1[,-11]
water5_1 <- water5_1[,-17]
water5_1 <- water5_1[,-17] # water5_1 : 원데이터 (TC, FC, Rain 에 log 취함, 기초통계량 산정)
water5_2 <- scale(water5_1) # water5_2 : 표준화 (상관분석)
water5_3 <- cbind(water5_2[,-(17:18)],water5_1[,17:18]) # water5_3 : 표준화 (logTC, logFC 제외, 회귀분석)


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
