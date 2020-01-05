## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Data loading
df <- read.csv("C:/Users/Nier/Desktop/수질데이터분석(논문)/1번째/전처리 후 데이터/SOM 패턴분석 월별 (영산포-1, 고막원천2-1).csv", 
               header=T, sep=",")
rownames(df) <- df[,1]
ECTN <- data.frame(df[,-(1:3)])

# Install packages
install.packages("kohonen")
library(kohonen)

# Normalization of data
ECTN_scale <- data.frame(scale(ECTN))
ECTN_scale_matrix <- as.matrix(ECTN_scale)

# Original
ECTN_matrix <- as.matrix(ECTN)

som_grid <- somgrid(xdim=7, ydim=12, topo="hexagonal")
som_model <- som(ECTN_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(2,2))
for (i in 1:4) {
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
