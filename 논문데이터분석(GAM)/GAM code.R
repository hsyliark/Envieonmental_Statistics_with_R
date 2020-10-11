# Reference : https://bookdown.org/cardiomoon/gam/
# Reference : https://be-favorite.tistory.com/59


# Chapter 1

# Install packages
# gam 패키지 불필요

library(mgcv)
library(ggplot2)
library(moonBook)
library(ztable)
library(survival)
devtools::install_github("cardiomoon/ggGam") # 업데이트 불필요
library(ggGam)


# Chapter 2

# start GAM 

# example 1
data(mcycle,package="MASS")
str(mcycle)
require(ggplot2)
p <- ggplot(data=mcycle,aes(x=times,y=accel))+geom_point()
p
p + stat_smooth(method=lm)
fit <- lm(accel~times,data=mcycle)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
require(mgcv)
m <- gam(accel ~ s(times), data=mcycle)
plot(m)
plot(m,residuals=TRUE,pch=1,shade=TRUE,seWithMean=TRUE,shift=coef(m)[1])
ggGam(m)
summary(m)

# example 2
set.seed(1)
x <- seq(0, pi * 2, 0.1)
y <- 2*sin(x) + rnorm(n = length(x), mean = 0, sd = sd(2*sin(x) / 2))
data1 <- data.frame(y,x)
ggplot(data1,aes(x=x,y=y))+geom_point()
m1 <- gam(y~s(x,bs="cc"),data=data1)
ggGam(m1)
par(mfrow=c(1,2))
basisFun(m1,which=2,lty=1)
basisFun(m1,which=4,lty=1)
par(mfrow=c(1,1))
coef(m1)


# 기저함수(basis function)와 평활모수(smoothing parameter)

# checking basis function
k3 <- gam(accel ~ s(times, k=3),data=mcycle)
k20 <- gam(accel ~ s(times, k=20),data=mcycle)
ggGam(k3, point=TRUE) + ggtitle("k=3")
ggGam(k20, point=TRUE) + ggtitle("k=20")

# checking smoothing parameter
m <- gam(accel ~ s(times),data=mcycle, method = "REML")
m$sp
sp1 <- gam(accel ~ s(times),data=mcycle, sp=0.1)
sp2 <- gam(accel ~ s(times),data=mcycle, sp=0.0001)
ggGam(sp1) + ggtitle("sp=0.1")
ggGam(sp2) + ggtitle("sp=0.0001")


# Multiple GAMs
library(gamair)
data(mpg, package="gamair")
DT::datatable(mpg)

model1 <- gam(hw.mpg ~ s(weight), data=mpg, method="REML")
ggGam(model1)

model2 <- gam(hw.mpg ~ s(weight)+s(length), data=mpg, method="REML")
ggGam(model2)

model3 <- gam(hw.mpg ~ s(weight)+length, data=mpg, method="REML")
ggGam(model3)

model4 <- gam(hw.mpg ~ s(weight)+s(length,sp=1000), data=mpg, method="REML")
ggGam(model4)

model5 <- gam(hw.mpg ~ s(weight)+fuel, data=mpg, method="REML")
plot(model5,residuals=TRUE,pch=1,shade=TRUE,seWithMean = TRUE,shift=coef(model5)[1],pages=1,all.terms=TRUE)
ggGam(model5,by=fuel)
ggGam(model5,by=fuel,facet=TRUE)
ggGam(model5)
ggGamCat(model5)

model6 <- gam(hw.mpg ~ s(weight, by=fuel), data=mpg, method="REML")
ggGam(model6,by=fuel)

model7 <- gam(hw.mpg ~ s(weight, by=fuel) + fuel, data=mpg, method="REML")
ggGam(model7,by=fuel)

model8 <- gam(hw.mpg ~ s(weight) + s(length) + s(price), data=mpg, method="REML")
ggGam(model8)

model9 <- gam(hw.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, 
              data=mpg, method="REML")
ggGam(model9)
ggGam(model9,by="fuel")
ggGam(model9,by="drive",se=FALSE)
ggGam(model9,by="style",se=FALSE)
model10 <- gam(hw.mpg ~ s(weight,by = drive) + s(length, by = drive) + s(price, by= drive)+drive,
               data=mpg, method="REML")
ggGam(model10,by=drive,se=FALSE)



# Chapter 3

# model interpretation
require(mgcv)
data(mpg,package="gamair")
require(ggGam)
m <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
         data=mpg, method="REML")
summary(m)
require(ggplot2)
ggGam(m)+geom_hline(yintercept=20,col="red")

# model checking
par(mfrow=c(2,2))
gam.check(m)
par(mfrow=c(1,1))
m1 <- gam(hw.mpg ~ s(weight, k=12) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
          data=mpg, method="REML") 
par(mfrow=c(2,2))
gam.check(m1)
par(mfrow=c(1,1))
m2 <- gam(hw.mpg ~ s(weight, k=15) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
          data=mpg, method="REML")
par(mfrow=c(2,2))
gam.check(m2)
par(mfrow=c(1,1))
m3 <- gam(hw.mpg ~ s(weight, k=18) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
          data=mpg, method="REML")
par(mfrow=c(2,2))
gam.check(m3)
par(mfrow=c(1,1))
m4 <- gam(hw.mpg ~ s(weight, k=40) + s(rpm) + s(price) + s(comp.ratio) + s(width) + fuel,
          data=mpg, method="REML")
par(mfrow=c(2,2))
gam.check(m4)
par(mfrow=c(1,1))

# checking concurvity (설명변수끼리 곡선관계를 가짐)
concurvity(m,full=TRUE) # 전체적인 concurvity
concurvity(m,full=FALSE) # 쌍별(pairwise) concurvity

# example (CO2 data)
co2 <- read.csv("D:/Workplace/Environmental_Statistics_with_R/논문데이터분석(GAM)/manua_loa_co2.csv",stringsAsFactors=FALSE)
str(co2)
co2$time <- as.integer(as.Date(co2$Date,format="%d/%m/%Y"))
ggplot(co2,aes(x=time,y=co2))+geom_line()
co2df <- co2[co2$year>=2000,]
m1 <- gam(co2 ~ s(time),data=co2df, method="REML")
ggGam(m1)
summary(m1)
par(mfrow=c(2,2))
gam.check(m1)
par(mfrow=c(1,1))
m2 <- gam(co2 ~ s(month, bs="cc",k=12)+ s(time),data=co2, method="REML")
ggGam(m2,point=FALSE)
summary(m2)
par(mfrow=c(2,2))
gam.check(m2)
par(mfrow=c(1,1))
gam.Dx(m2) # ggplot2를 이용한 진단그래프
co2$yhat <- predict(m2,newdata=co2) # 구축모형을 이용한 예측값 산출
ggplot(co2,aes(x=time,y=co2))+
  geom_point()+
  geom_line(aes(y=yhat),col="red")



# Chapter 4

data(meuse,package="sp") # spatial data
head(meuse)
require(mgcv)
mod <- gam(cadmium ~ s(x,y), data=meuse, method = "REML") # interaction
summary(mod)
par(mfrow=c(2,2))
gam.check(mod)
par(mfrow=c(1,1))
mod2 <- gam(cadmium ~ s(x,y) + s(elev) + s(dist), data=meuse, method = "REML")
summary(mod2)
par(mfrow=c(2,2))
gam.check(mod2)
par(mfrow=c(1,1))
plot(mod2,select=1) # contour plot
plot(mod2, scheme=1, select=1) # 3d
plot(mod2, scheme=2, select=1) # 노란색은 큰 예측값을, 붉은색은 낮은 예측값
vis.gam(x=mod2,                 # GAM 모형이름
        view=c("x","y"),      # 변수이름
        plot.type="persp")  # plot 종류
vis.gam(mod2, view=c("x","y"), plot.type="contour")
vis.gam(mod2, view=c("x","y"),plot.type="contour",
        too.far=0.1, main="too.far=0.1") # too.far : 실제 데이터로부터 너무 멀어지므로 그리지 말아야할 예측치를 지정
vis.gam(mod2, view=c("x","y"),plot.type="contour",
        too.far=0.05,main="too.far=0.05")
vis.gam(mod2, view=c("x","y"),
        plot.type="persp", se=2) # se : 예측표면으로부터 표준오차의 몇배 떨어진 표면을 그릴 것인지 지정
