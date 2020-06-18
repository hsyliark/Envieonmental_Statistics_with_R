watersom1 <- read.csv("D:/Workplace/Environmental_Statistics_with_R/논문데이터분석(LASSO)/자료, 결과정리/PCA에 Rain, Flow 포함/csv파일/som1.csv", sep=",", header=T)
watersom2 <- read.csv("D:/Workplace/Environmental_Statistics_with_R/논문데이터분석(LASSO)/자료, 결과정리/PCA에 Rain, Flow 포함/csv파일/som2.csv", sep=",", header=T)


## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

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


PC1 <- read.csv("C:/Users/Nier/Desktop/?���?????ͺм?(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/?????????ڷ?/PC1.csv", sep=",", header=T)
PC2_1 <- read.csv("C:/Users/Nier/Desktop/?���?????ͺм?(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/?????????ڷ?/PC2_1.csv", sep=",", header=T)
PC2_2 <- read.csv("C:/Users/Nier/Desktop/?���?????ͺм?(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/?????????ڷ?/PC2_2.csv", sep=",", header=T)
water1 <- read.csv("C:/Users/Nier/Desktop/?���?????ͺм?(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/?????????ڷ?/??????.csv", sep=",", header=T)

# Install packages
install.packages("kohonen")
library(kohonen)

# Normalization of data
ECTN_scale <- data.frame(scale(ECTN))
ECTN_scale_matrix <- as.matrix(ECTN_scale)


# Original
watersom1_matrix <- as.matrix(watersom1)
watersom2_matrix <- as.matrix(watersom2)

T_P <- read.csv("C:/Users/Nier/Desktop/??ü??��/T-P.csv", sep=",", header=T)
T_P_matrix <- as.matrix(T_P)

som_grid <- somgrid(xdim=16, ydim=15, topo="hexagonal")
som_model <- som(watersom2_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(1,1))
for (i in 1:3) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))



## Granger causality test
# reference : http://intothedata.com/02.scholar_category/timeseries_analysis/granger_causality/

# Install packages
install.packages("lmtest")
library(lmtest)

# Drawing graph
par(mfrow=c(2,2))
attach(ECTN)
plot.ts(??????_1.EC, main="??????-1(EC)")
plot.ts(??????_1.TN, main="??????-1(T-N)")
plot.ts(?���??õ2_1.EC, main="?���??õ2-1(EC)")
plot.ts(?���??õ2_1.TN, main="?���??õ2-1(T-N)")
par(mfrow=c(1,1))

# Time series
install.packages("forecast")
require(forecast)

# KPSS test (��???ð迭�� ��?? ???н??? ??��)
ndiffs(ECTN$??????_1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$??????_1.TN, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$?���??õ2_1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$?���??õ2_1.TN, alpha=0.05, test=c("kpss")) 

# ???н????? ???? ???쿡?? ?ش?
ECTN$diff1_??????_1.EC <- diff(ECTN$??????_1.EC, 1)
ECTN$diff1_??????_1.TN <- diff(ECTN$??????_1.TN, 1)
ECTN$diff1_?���??õ2_1.EC <- diff(ECTN$?���??õ2_1.EC, 1)
ECTN$diff1_?���??õ2_1.TN <- diff(ECTN$?���??õ2_1.TN, 1)
par(mfrow=c(2,2))
plot.ts(ECTN$diff1_??????_1.EC, main="??????-1(EC) 1???? ????")
plot.ts(ECTN$diff1_??????_1.TN, main="??????-1(T-N) 1???? ????")
plot.ts(ECTN$diff1_?���??õ2_1.EC, main="?���??õ2-1(EC) 1???? ????")
plot.ts(ECTN$diff1_?���??õ2_1.TN, main="?���??õ2-1(T-N) 1???? ????")
par(mfrow=c(1,1))

# ?ΰ????? ?м? (???? ~ ????)
grangertest(ECTN$?���??õ2_1.EC ~ ECTN$??????_1.EC, order=3)
grangertest(ECTN$?���??õ2_1.TN ~ ECTN$??????_1.TN, order=3)
