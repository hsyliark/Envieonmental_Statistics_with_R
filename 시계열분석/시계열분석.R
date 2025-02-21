## Data loading
water <- read.csv("C:/Users/Nier/Desktop/시계열분석/광주2-1.csv", sep=",", header=T)
water_name <- water$date
rownames(water) <- water_name

## Install packages
install.packages("TTR")
install.packages("forecast")
library(TTR)
library(forecast)

## Time series data
TP <- ts(water[,4], frequency=48, start=c(2015,1))
TOC <- ts(water[,5], frequency=48, start=c(2015,1)) 
plot.ts(TP, main="T-P of Gwangju2-1 point")
plot.ts(TOC, main="TOC of Gwangju2-1 point")

## data decomposition - trend, seasonal, random
TP_comp <- decompose(TP)
plot(TP_comp)
TOC_comp <- decompose(TOC)
plot(TOC_comp)

## ?떆怨꾩뿴 ?뜲?씠?꽣?뿉?꽌 怨꾩젅?꽦 ?슂?씤 ?젣嫄?
TP_adjusted <- TP - TP_comp$seasonal
plot.ts(TP_adjusted, main = "TP - seasonal factor")
plot.ts(TP_comp$seasonal, main="Seasonal factor of TP")
TOC_adjusted <- TOC - TOC_comp$seasonal
plot.ts(TOC_adjusted, main = "TOC - seasonal factor")
plot.ts(TOC_comp$seasonal, main="Seasonal factor of TOC")

## Dickey-Fuller unit root test
# H0: ?옄猷뚯뿉 ?떒?쐞洹쇱씠 議댁옱?븳?떎.
# H1: ?떆怨꾩뿴 ?옄猷뚭?� ?젙?긽?꽦?쓣 留뚯”?븳?떎(?삉?뒗 異붿꽭 ?젙?긽?꽦?쓣 留뚯”?븳?떎).
install.packages("tseries")
library(tseries)
adf.test(TP_adjusted)
# -> 怨꾩젅?꽦 ?슂?씤?쓣 ?젣嫄고븳 T-P?뒗 ?떒?쐞洹쇱씠 議댁옱?븯吏� ?븡?쓬. (李⑤텇 ?븘?슂 ?뾾?쓬)
adf.test(TOC_adjusted)
# -> 怨꾩젅?꽦 ?슂?씤?쓣 ?젣嫄고븳 TOC?뒗 ?떒?쐞洹쇱씠 議댁옱?븿. 利?, 異붿꽭媛� 議댁옱. (李⑤텇 ?븘?슂)
adf.test(TP_comp$seasonal)
adf.test(TOC_comp$seasonal)
# -> T-P??� TOC?쓽 怨꾩젅?꽦 ?슂?씤??� 紐⑤몢 異붿꽭媛� 議댁옱?븯吏� ?븡?쓬.
## 李⑤텇?쓣 ?넻?빐 ?젙?긽?꽦 ?솗?씤
TOC_diff1 <- diff(TOC_adjusted, differences = 1)
plot.ts(TOC_diff1, main = "TOC - seasonal factor 1李? 李⑤텇") 
adf.test(TOC_diff1)

## ACF, PACF
# T-P
acf(TP_adjusted, lag.max = 4800)  # lag 3 ?뿉?꽌 ?젅?떒媛? --> MA(2)
pacf(TP_adjusted, lag.max = 4800)  # lag 3 ?뿉?꽌 ?젅?떒媛? --> AR(2)
# --> 怨꾩젅?꽦 ?슂?씤?쓣 ?젣嫄고븳 T-P : ARIMA(2,0,2) ?
acf(TP_comp$seasonal, lag.max = 4800) # 媛먯냼異붿꽭
pacf(TP_comp$seasonal, lag.max = 4800) # lag 2 ?뿉?꽌 ?젅?떒媛? --> AR(1)
# --> T-P?쓽 怨꾩젅?꽦 ?슂?씤 : ARIMA(1,0,0)[48] 
# TOC
acf(TOC_diff1, lag.max = 4800) # lag 1 ?뿉?꽌 ?젅?떒媛? --> MA(0)
pacf(TOC_diff1, lag.max = 4800) # lag 2 ?뿉?꽌 ?젅?떒媛? --> AR(1)
# --> 怨꾩젅?꽦 ?슂?씤?쓣 ?젣嫄고븳 TOC : ARIMA(0,1,1) ?
acf(TOC_comp$seasonal, lag.max = 4800) # 媛먯냼異붿꽭
pacf(TOC_comp$seasonal, lag.max = 4800) # lag 2 ?뿉?꽌 ?젅?떒媛? --> AR(1) 
# --> TOC?쓽 怨꾩젅?꽦 ?슂?씤 : ARIMA(1,0,0)[48]
# ----> T-P : ARIMA(2,0,2)(1,0,0)[48]
# ----> TOC : ARIMA(0,1,1)(1,0,0)[48]

## ?옄?룞?쑝濡? ARIMA 紐⑦삎 ?솗?씤
auto.arima(TP)
auto.arima(TOC)

## Time series modeling
TP_arima <- arima(TP, order=c(2,0,2), seasonal=list(order=c(1,0,0),period=48))
TP_arima
TOC_arima <- arima(TOC, order=c(0,1,1), seasonal=list(order=c(1,0,0),period=48))
TOC_arima

## Forecasting
TP_fcast <- forecast(TP_arima)
TP_fcast
plot(TP_fcast, main = "T-P Forecasts")
TOC_fcast <- forecast(TOC_arima)
TOC_fcast
plot(TOC_fcast, main = "TOC Forecasts")