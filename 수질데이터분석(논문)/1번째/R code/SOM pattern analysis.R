## Self Organizing Map 
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Data loading
df <- read.csv("C:/Users/Nier/Desktop/수질데이터분석(논문)/1번째/분석데이터/분석자료(낙동강)/신반천 합류부/신반천 합류부 패턴분석.csv", 
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

som_grid <- somgrid(xdim=4, ydim=12, topo="hexagonal")
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
plot.ts(합천_TN, main="합천(T_N)")
plot.ts(합천_EC, main="합천(EC)")
plot.ts(신반천_TN, main="신반천(T_N)")
plot.ts(신반천_EC, main="신반천(EC)")
par(mfrow=c(1,1))

# Time series
install.packages("forecast")
require(forecast)

# KPSS test (정상시계열을 위한 차분시차 결정)
ndiffs(ECTN$합천_TN, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$합천_EC, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$신반천_TN, alpha=0.05, test=c("kpss")) 
ndiffs(ECTN$신반천_EC, alpha=0.05, test=c("kpss")) 

# 차분시차가 나온 경우에만 해당
diff1_합천_TN <- as.data.frame(diff(ECTN$합천_TN, 1))
diff1_합천_EC <- as.data.frame(diff(ECTN$합천_EC, 1))
diff1_신반천_TN <- as.data.frame(diff(ECTN$신반천_TN, 1))
diff1_신반천_EC <- as.data.frame(diff(ECTN$신반천_EC, 1))
ECTN_diff1 <- cbind(diff1_합천_TN, diff1_합천_EC, diff1_신반천_TN, diff1_신반천_EC)
colnames(ECTN_diff1) <- c('diff1_합천_TN', 'diff1_합천_EC', 'diff1_신반천_TN', 'diff1_신반천_EC')

par(mfrow=c(2,2))
plot.ts(ECTN_diff1$diff1_합천_TN, main="합천(T_N) 1시차 차분")
plot.ts(ECTN_diff1$diff1_합천_EC, main="합천(EC) 1시차 차분")
plot.ts(ECTN_diff1$diff1_신반천_TN, main="신반천(T_N) 1시차 차분")
plot.ts(ECTN_diff1$diff1_신반천_EC, main="신반천(EC) 1시차 차분")
par(mfrow=c(1,1))

# 인과관계 분석 (결과 ~ 원인)
grangertest(ECTN_diff1$diff1_신반천_TN ~ ECTN_diff1$diff1_합천_TN, order=3)
grangertest(ECTN_diff1$diff1_신반천_EC ~ ECTN_diff1$diff1_합천_EC, order=3)
