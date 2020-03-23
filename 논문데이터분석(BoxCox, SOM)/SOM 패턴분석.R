water <- read.csv("C:/Users/Nier/Desktop/논문데이터분석/논문데이터분석(Boxcox 변환 관련)/데이터(표준화 요청)/csv 파일/우치rawdata.csv", sep=",", header=T)
water <- water[1:510,1:17]

# Install packages
install.packages("kohonen")
library(kohonen)

# Normalization of data
water_scale <- data.frame(scale(water))
water_scale_matrix <- as.matrix(water_scale)

# Original
water_matrix <- as.matrix(water)
water1_matrix <- water_matrix[,c(2,5,9,10,11,13,15,16,17)]

som_grid <- somgrid(xdim=22, ydim=22, topo="hexagonal")
som_model <- som(water1_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(3,3))
for (i in 1:9) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))