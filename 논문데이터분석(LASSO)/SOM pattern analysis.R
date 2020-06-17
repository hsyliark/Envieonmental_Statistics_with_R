## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

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


PC1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/통계관련자료/PC1.csv", sep=",", header=T)
PC2_1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/통계관련자료/PC2_1.csv", sep=",", header=T)
PC2_2 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/통계관련자료/PC2_2.csv", sep=",", header=T)
water1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(A study on evaluate water quality using LASSO in Yeongsan River, Republic of Korea)/통계관련자료/나머지.csv", sep=",", header=T)

# Install packages
install.packages("kohonen")
library(kohonen)

# Normalization of data
ECTN_scale <- data.frame(scale(ECTN))
ECTN_scale_matrix <- as.matrix(ECTN_scale)


# Original
water1_matrix <- as.matrix(water1)

T_P <- read.csv("C:/Users/Nier/Desktop/전체지점/T-P.csv", sep=",", header=T)
T_P_matrix <- as.matrix(T_P)

som_grid <- somgrid(xdim=25, ydim=24, topo="hexagonal")
som_model <- som(water1_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(1,1))
for (i in 1:6) {
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
plot.ts(영산포_1.EC, main="영산포-1(EC)")
plot.ts(영산포_1.TN, main="영산포-1(T-N)")
plot.ts(고막원천2_1.EC, main="고막원천2-1(EC)")
plot.ts(고막원천2_1.TN, main="고막원천2-1(T-N)")
par(mfrow=c(1,1))

# Time series
install.packages("forecast")
require(forecast)

# KPSS test (정상시계열을 위한 차분시차 결정)
ndiffs(ECTN$영산포_1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$영산포_1.TN, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$고막원천2_1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$고막원천2_1.TN, alpha=0.05, test=c("kpss")) 

# 차분시차가 나온 경우에만 해당
ECTN$diff1_영산포_1.EC <- diff(ECTN$영산포_1.EC, 1)
ECTN$diff1_영산포_1.TN <- diff(ECTN$영산포_1.TN, 1)
ECTN$diff1_고막원천2_1.EC <- diff(ECTN$고막원천2_1.EC, 1)
ECTN$diff1_고막원천2_1.TN <- diff(ECTN$고막원천2_1.TN, 1)
par(mfrow=c(2,2))
plot.ts(ECTN$diff1_영산포_1.EC, main="영산포-1(EC) 1시차 차분")
plot.ts(ECTN$diff1_영산포_1.TN, main="영산포-1(T-N) 1시차 차분")
plot.ts(ECTN$diff1_고막원천2_1.EC, main="고막원천2-1(EC) 1시차 차분")
plot.ts(ECTN$diff1_고막원천2_1.TN, main="고막원천2-1(T-N) 1시차 차분")
par(mfrow=c(1,1))

# 인과관계 분석 (결과 ~ 원인)
grangertest(ECTN$고막원천2_1.EC ~ ECTN$영산포_1.EC, order=3)
grangertest(ECTN$고막원천2_1.TN ~ ECTN$영산포_1.TN, order=3)
