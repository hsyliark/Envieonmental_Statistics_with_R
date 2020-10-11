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
