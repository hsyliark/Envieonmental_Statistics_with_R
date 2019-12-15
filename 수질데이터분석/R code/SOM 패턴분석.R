## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Data loading
df <- read.csv("C:/Users/HSY/Desktop/논문분석요청/전처리 후 데이터/SOM 패턴분석(영산포-1, 고막원천2-1).csv", 
               header=T, sep=",")
rownames(df) <- df[,1]
ECTN <- data.frame(df[,-1])

# Install packages
install.packages("kohonen")
library(kohonen)

# Normalization of data
ECTN_scale <- data.frame(scale(ECTN))
ECTN_scale_matrix <- as.matrix(ECTN_scale)

# Original
ECTN_matrix <- as.matrix(ECTN)

som_grid <- somgrid(xdim=13, ydim=24, topo="hexagonal")
som_model <- som(ECTN_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(2,2))
for (i in 1:4) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)
}

