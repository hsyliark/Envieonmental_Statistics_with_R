water <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(낙동강 K-means SOM clustering)_MannKendall추가/분석자료/분석7/SMKTT and LOWESS analysis_Tributaries.csv", sep=",", header=T)

# reference1 : https://m.blog.naver.com/PostView.nhn?blogId=yoosunbi&logNo=221067045984&proxyReferer=https:%2F%2Fwww.google.com%2F
# reference2 : https://freshrimpsushi.tistory.com/1217
# reference3 : http://blog.naver.com/PostView.nhn?blogId=kimber3&logNo=221026715897
# reference4 : https://www.statology.org/mann-kendall-trend-test-r/

install.packages("lmtest")
library(lmtest)
install.packages("Kendall")
library(Kendall)

out1 <- lm(BOD~BOD_lag, data=water)
plot(rstudent(out1),main="Residuals of BOD")
dwtest(out1)
acf(water$BOD)
pacf(water$BOD)
MannKendall(water$BOD)
SeasonalMannKendall(ts(water$BOD))
plot(ts(water$BOD), main="BOD time series plot with LOWESS curve")
lines(lowess(time(ts(water$BOD)),ts(water$BOD)), col='blue')

out2 <- lm(COD~COD_lag, data=water)
plot(rstudent(out2),main="Residuals of COD")
dwtest(out2)
acf(water$COD)
pacf(water$COD)
MannKendall(water$COD)
SeasonalMannKendall(ts(water$COD))
plot(ts(water$COD), main="COD time series plot with LOWESS curve")
lines(lowess(time(ts(water$COD)),ts(water$COD)), col='blue')

out3 <- lm(T.N~T.N_lag, data=water)
plot(rstudent(out3),main="Residuals of T-N")
dwtest(out3)
acf(water$T.N)
pacf(water$T.N)
MannKendall(water$T.N)
SeasonalMannKendall(ts(water$T.N))
plot(ts(water$T.N), main="T-N time series plot with LOWESS curve")
lines(lowess(time(ts(water$T.N)),ts(water$T.N)), col='blue')

out4 <- lm(DTN~DTN_lag, data=water)
plot(rstudent(out4),main="Residuals of DTN")
dwtest(out4)
acf(water$DTN)
pacf(water$DTN)
MannKendall(water$DTN)
SeasonalMannKendall(ts(water$DTN))
plot(ts(water$DTN), main="DTN time series plot with LOWESS curve")
lines(lowess(time(ts(water$DTN)),ts(water$DTN)), col='blue')

out5 <- lm(NO3.N~NO3.N_lag, data=water)
plot(rstudent(out5),main="Residuals of NO3-N")
dwtest(out5)
acf(water$NO3.N)
pacf(water$NO3.N)
MannKendall(water$NO3.N)
SeasonalMannKendall(ts(water$NO3.N))
plot(ts(water$NO3.N), main="NO3-N time series plot with LOWESS curve")
lines(lowess(time(ts(water$NO3.N)),ts(water$NO3.N)), col='blue')

out6 <- lm(NH3.N~NH3.N_lag, data=water)
plot(rstudent(out6),main="Residuals of NH3-N")
dwtest(out6)
acf(water$NH3.N)
pacf(water$NH3.N)
MannKendall(water$NH3.N)
SeasonalMannKendall(ts(water$NH3.N))
plot(ts(water$NH3.N), main="NH3-N time series plot with LOWESS curve")
lines(lowess(time(ts(water$NH3.N)),ts(water$NH3.N)), col='blue')

out7 <- lm(T.P~T.P_lag, data=water)
plot(rstudent(out7),main="Residuals of T-P")
dwtest(out7)
acf(water$T.P)
pacf(water$T.P)
MannKendall(water$T.P)
SeasonalMannKendall(ts(water$T.P))
plot(ts(water$T.P), main="T-P time series plot with LOWESS curve")
lines(lowess(time(ts(water$T.P)),ts(water$T.P)), col='blue')

out8 <- lm(DTP~DTP_lag, data=water)
plot(rstudent(out8),main="Residuals of DTP")
dwtest(out8)
acf(water$DTP)
pacf(water$DTP)
MannKendall(water$DTP)
SeasonalMannKendall(ts(water$DTP))
plot(ts(water$DTP), main="DTP time series plot with LOWESS curve")
lines(lowess(time(ts(water$DTP)),ts(water$DTP)), col='blue')

out9 <- lm(PO4.P~PO4.P_lag, data=water)
plot(rstudent(out9),main="Residuals of PO4-P")
dwtest(out9)
acf(water$PO4.P)
pacf(water$PO4.P)
MannKendall(water$PO4.P)
SeasonalMannKendall(ts(water$PO4.P))
plot(ts(water$PO4.P), main="PO4-P time series plot with LOWESS curve")
lines(lowess(time(ts(water$PO4.P)),ts(water$PO4.P)), col='blue')

out10 <- lm(Chl.a~Chl.a_lag, data=water)
plot(rstudent(out10),main="Residuals of Chlorophyll-a")
dwtest(out10)
acf(water$Chl.a)
pacf(water$Chl.a)
MannKendall(water$Chl.a)
SeasonalMannKendall(ts(water$Chl.a))
plot(ts(water$Chl.a), main="Chlorophyll-a time series plot with LOWESS curve")
lines(lowess(time(ts(water$Chl.a)),ts(water$Chl.a)), col='blue')



