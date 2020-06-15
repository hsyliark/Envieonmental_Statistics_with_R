## Data loading
water <- read.csv("C:/Users/Nier/Desktop/½Ã°è¿­ºĞ¼®/±¤ÁÖ2-1.csv", sep=",", header=T)
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

## ?‹œê³„ì—´ ?°?´?„°?—?„œ ê³„ì ˆ?„± ?š”?¸ ? œê±?
TP_adjusted <- TP - TP_comp$seasonal
plot.ts(TP_adjusted, main = "TP - seasonal factor")
plot.ts(TP_comp$seasonal, main="Seasonal factor of TP")
TOC_adjusted <- TOC - TOC_comp$seasonal
plot.ts(TOC_adjusted, main = "TOC - seasonal factor")
plot.ts(TOC_comp$seasonal, main="Seasonal factor of TOC")

## Dickey-Fuller unit root test
# H0: ?ë£Œì— ?‹¨?œ„ê·¼ì´ ì¡´ì¬?•œ?‹¤.
# H1: ?‹œê³„ì—´ ?ë£Œê?€ ? •?ƒ?„±?„ ë§Œì¡±?•œ?‹¤(?˜?Š” ì¶”ì„¸ ? •?ƒ?„±?„ ë§Œì¡±?•œ?‹¤).
install.packages("tseries")
library(tseries)
adf.test(TP_adjusted)
# -> ê³„ì ˆ?„± ?š”?¸?„ ? œê±°í•œ T-P?Š” ?‹¨?œ„ê·¼ì´ ì¡´ì¬?•˜ì§€ ?•Š?Œ. (ì°¨ë¶„ ?•„?š” ?—†?Œ)
adf.test(TOC_adjusted)
# -> ê³„ì ˆ?„± ?š”?¸?„ ? œê±°í•œ TOC?Š” ?‹¨?œ„ê·¼ì´ ì¡´ì¬?•¨. ì¦?, ì¶”ì„¸ê°€ ì¡´ì¬. (ì°¨ë¶„ ?•„?š”)
adf.test(TP_comp$seasonal)
adf.test(TOC_comp$seasonal)
# -> T-P??€ TOC?˜ ê³„ì ˆ?„± ?š”?¸??€ ëª¨ë‘ ì¶”ì„¸ê°€ ì¡´ì¬?•˜ì§€ ?•Š?Œ.
## ì°¨ë¶„?„ ?†µ?•´ ? •?ƒ?„± ?™•?¸
TOC_diff1 <- diff(TOC_adjusted, differences = 1)
plot.ts(TOC_diff1, main = "TOC - seasonal factor 1ì°? ì°¨ë¶„") 
adf.test(TOC_diff1)

## ACF, PACF
# T-P
acf(TP_adjusted, lag.max = 4800)  # lag 3 ?—?„œ ? ˆ?‹¨ê°? --> MA(2)
pacf(TP_adjusted, lag.max = 4800)  # lag 3 ?—?„œ ? ˆ?‹¨ê°? --> AR(2)
# --> ê³„ì ˆ?„± ?š”?¸?„ ? œê±°í•œ T-P : ARIMA(2,0,2) ?
acf(TP_comp$seasonal, lag.max = 4800) # ê°ì†Œì¶”ì„¸
pacf(TP_comp$seasonal, lag.max = 4800) # lag 2 ?—?„œ ? ˆ?‹¨ê°? --> AR(1)
# --> T-P?˜ ê³„ì ˆ?„± ?š”?¸ : ARIMA(1,0,0)[48] 
# TOC
acf(TOC_diff1, lag.max = 4800) # lag 1 ?—?„œ ? ˆ?‹¨ê°? --> MA(0)
pacf(TOC_diff1, lag.max = 4800) # lag 2 ?—?„œ ? ˆ?‹¨ê°? --> AR(1)
# --> ê³„ì ˆ?„± ?š”?¸?„ ? œê±°í•œ TOC : ARIMA(0,1,1) ?
acf(TOC_comp$seasonal, lag.max = 4800) # ê°ì†Œì¶”ì„¸
pacf(TOC_comp$seasonal, lag.max = 4800) # lag 2 ?—?„œ ? ˆ?‹¨ê°? --> AR(1) 
# --> TOC?˜ ê³„ì ˆ?„± ?š”?¸ : ARIMA(1,0,0)[48]
# ----> T-P : ARIMA(2,0,2)(1,0,0)[48]
# ----> TOC : ARIMA(0,1,1)(1,0,0)[48]

## ??™?œ¼ë¡? ARIMA ëª¨í˜• ?™•?¸
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