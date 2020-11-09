watersom1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(LASSO)/PCA에 Rain, Flow 포함/csv파일/함평som/som1.csv", sep=",", header=T)
watersom2 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(LASSO)/PCA에 Rain, Flow 포함/csv파일/함평som/som2.csv", sep=",", header=T)
watersom3 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(LASSO)/PCA에 Rain, Flow 포함/csv파일/함평som/som3.csv", sep=",", header=T)

## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Install packages
install.packages("kohonen")
library(kohonen)

# Original
watersom1_matrix <- as.matrix(watersom1)
watersom2_matrix <- as.matrix(watersom2)
watersom3_matrix <- as.matrix(watersom3)

som_grid <- somgrid(xdim=6, ydim=10, topo="hexagonal")
som_model <- som(watersom3_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(1,1))
for (i in 1:5) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))
