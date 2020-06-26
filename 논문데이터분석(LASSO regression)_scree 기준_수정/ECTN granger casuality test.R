## Self Organizing Map (자기조직화지도)
# reference : https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/

# Data loading
month_TNTP <- read.csv("X:/수질측정망 자료 통계분석/수질데이터(200001-201909)/패턴, 인과관계 분석/수질(200001-201909) 측정소별 TN, TP (무안2,영암천,삼포천2,무안1).csv", header=T, sep=",")
rownames(month_TNTP) <- month_TNTP[,1]
month_TNTP <- data.frame(month_TNTP[,-1])

# Install packages
install.packages("kohonen")
library(kohonen)

# Modeling
TNTP_matrix <- as.matrix(month_TNTP)

som_grid <- somgrid(xdim=15, ydim=15, topo="hexagonal")
som_model <- som(TNTP_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mar=c(5.1,4.1,4.1,2.1)) # 텍스트의 라인 수
par(mai=c(1.02,0.82,0.82,0.42)) # 인치
par(mfrow=c(1,1))
plot(som_model, type="changes", palette.name=coolBlueHotRed)
plot(som_model, type="count", main="Node Counts", palette.name=coolBlueHotRed)
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", 
     palette.name=coolBlueHotRed) 
plot(som_model, type="codes", palette.name=coolBlueHotRed) 

par(mfrow=c(4,2))
for (i in 1:ncol(TNTP_matrix)) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)
}


## Granger causality test
# reference : http://intothedata.com/02.scholar_category/timeseries_analysis/granger_causality/

# Install packages
install.packages("lmtest")
library(lmtest)

# Drawing graph
par(mfrow=c(2,2))
TNTP_df <- as.data.frame(TNTP_matrix)
attach(TNTP_df)
plot.ts(무안2.TN., main="무안2(TN)")
plot.ts(무안2.TP., main="무안2(TP)")
plot.ts(무안1.TN., main="무안1(TN)")
plot.ts(무안1.TP., main="무안1(TP)")

# 정상시계열이 아니므로 변환 필요
install.packages("forecast")
require(forecast)

# KPSS test (정상성을 만족시키기 위한 시차 찾기)
ndiffs(TNTP_df$무안2.TN., alpha=0.05, test=c("kpss")) # 1
ndiffs(TNTP_df$무안2.TP., alpha=0.05, test=c("kpss")) # 1
ndiffs(TNTP_df$무안1.TN., alpha=0.05, test=c("kpss")) # 1
ndiffs(TNTP_df$무안1.TP., alpha=0.05, test=c("kpss")) # 1

# 차분
diff1_무안2.TN. <- diff(TNTP_df$무안2.TN., 1)
diff1_무안2.TP. <- diff(TNTP_df$무안2.TP., 1)
diff1_무안1.TN. <- diff(TNTP_df$무안1.TN., 1)
diff1_무안1.TP. <- diff(TNTP_df$무안1.TP., 1)
par(mfrow=c(2,2))
plot.ts(diff1_무안2.TN., main="무안2(TN) 시차1 차분")
plot.ts(diff1_무안2.TP., main="무안2(TP) 시차1 차분")
plot.ts(diff1_무안1.TN., main="무안1(TN) 시차1 차분")
plot.ts(diff1_무안1.TP., main="무안1(TP) 시차1 차분")

grangertest(diff1_무안1.TN. ~ diff1_무안2.TN., order=3)
grangertest(diff1_무안1.TP. ~ diff1_무안2.TP., order=3)
# 무안2 지점의 T-N, T-P가 각각 3개월 후의 무안1 지점의 T-N, T-P에 영향을 주는 것으로 보임.



