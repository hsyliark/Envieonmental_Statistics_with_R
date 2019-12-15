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
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}




## Granger causality test
# reference : http://intothedata.com/02.scholar_category/timeseries_analysis/granger_causality/

# Install packages
install.packages("lmtest")
library(lmtest)

# Drawing graph
par(mfrow=c(2,2))
attach(ECTN)
plot.ts(고막원천2.1.EC, main="고막원천2-1(EC)")
plot.ts(고막원천2.1.T.N, main="고막원천2-1(T-N)")
plot.ts(영산포.1.EC, main="영산포-1(EC)")
plot.ts(영산포.1.T.N, main="영산포-1(T-N)")

# Time series
install.packages("forecast")
require(forecast)

# KPSS test (정상시계열을 위한 차분시차 산출)
ndiffs(ECTN$고막원천2.1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$고막원천2.1.T.N, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$영산포.1.EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$영산포.1.T.N, alpha=0.05, test=c("kpss")) 

# 차분이 필요한 경우에 해당
ECTN$diff1_고막원천2.1.EC <- diff(ECTN$고막원천2.1.EC, 1)
ECTN$diff1_고막원천2.1.T.N <- diff(ECTN$고막원천2.1.T.N, 1)
ECTN$diff1_영산포.1.EC <- diff(ECTN$영산포.1.EC, 1)
ECTN$diff1_영산포.1.T.N <- diff(ECTN$영산포.1.T.N, 1)
par(mfrow=c(2,2))
plot.ts(ECTN$diff1_고막원천2.1.EC, main="고막원천2-1(EC) 1시차 차분")
plot.ts(ECTN$diff1_고막원천2.1.T.N, main="고막원천2-1(T-N) 1시차 차분")
plot.ts(ECTN$diff1_영산포.1.EC, main="영산포-1(EC) 1시차 차분")
plot.ts(ECTN$diff1_영산포.1.T.N, main="영산포-1(T-N) 1시차 차분")

# 인과관계 검정 (결과 ~ 원인)
for (i in 1:nrow(ECTN)) {
  grangertest(ECTN$영산포.1.EC ~ ECTN$고막원천2.1.EC, order=i)}
for (i in 1:nrow(ECTN)) {  
  grangertest(ECTN$영산포.1.T.N ~ ECTN$고막원천2.1.T.N, order=i)}

grangertest(ECTN$영산포.1.EC ~ ECTN$고막원천2.1.EC, order=5)
grangertest(ECTN$영산포.1.T.N ~ ECTN$고막원천2.1.T.N, order=5)
# order=103 까지만 가능...