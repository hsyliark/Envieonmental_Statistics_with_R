# Reference : https://bookdown.org/cardiomoon/gam/
# Reference : https://be-favorite.tistory.com/59


# Chapter 1

# Install packages
library(mgcv)
library(ggplot2)
library(moonBook)
library(ztable)
library(survival)
devtools::install_github("cardiomoon/ggGam") # Don't use gam package!
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

# basis function, smoothing parameter

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

# checking concurvity 
concurvity(m,full=TRUE) # overall concurvity
concurvity(m,full=FALSE) # pairwise concurvity

# example (CO2 data)
co2 <- read.csv("manua_loa_co2.csv",stringsAsFactors=FALSE)
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
gam.Dx(m2) 
co2$yhat <- predict(m2,newdata=co2) 
ggplot(co2,aes(x=time,y=co2))+
  geom_point()+
  geom_line(aes(y=yhat),col="red")



# Chapter 4 (interaction)

# example 1
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
plot(mod2, select=1) # contour plot
plot(mod2, scheme=1, select=1) # 3d
plot(mod2, scheme=2, select=1) 
vis.gam(x=mod2, view=c("x","y"), plot.type="persp") 
vis.gam(mod2, view=c("x","y"), plot.type="contour")
vis.gam(mod2, view=c("x","y"), plot.type="contour", too.far=0.1, main="too.far=0.1")
vis.gam(mod2, view=c("x","y"), plot.type="contour", too.far=0.05, main="too.far=0.05")
vis.gam(mod2, view=c("x","y"), plot.type="persp", se=2) 
vis.gam(mod2, view=c("x","y"), plot.type="persp", theta = 220)
vis.gam(mod2, view=c("x","y"), plot.type="persp", phi = 55)
vis.gam(mod2, view=c("x","y"), plot.type="persp", r = 0.1)
vis.gam(mod2, view = c("x","y"), plot.type = "contour", color = "gray")
vis.gam(mod2, view = c("x","y"), plot.type = "contour", contour.col = "blue")
vis.gam(mod2, view = c("x","y"), plot.type = "contour", nlevels = 20)
plot(mod2, pages=1)
plot(mod2, scheme=1, pages=1)
plot(mod2, scheme=2, pages=1)
vis.gam(mod, view=c("x","y"), plot.type="persp", se=2)
vis.gam(mod, view=c("x","y"), plot.type="persp", se=2, theta=135)
vis.gam(mod, view=c("x","y"), plot.type="contour", too.far=0.25)

# example 2
data(mpg, package="gamair")
mod3 <- gam(hw.mpg ~ s(weight,by=fuel) + fuel, data=mpg, method="REML")
plot(mod3, pages=1, all.terms=TRUE, seWithMean = TRUE, shift=coef(mod3)[1], shade=TRUE)
require(ggGam)
ggGam(mod3, by=fuel)
mod3a <- gam(hw.mpg ~ s(weight,fuel,bs="fs"), data=mpg, method="REML")
summary(mod3a)
plot(mod3a, pages=1, all.terms=TRUE, seWithMean = TRUE, shift=coef(mod3a)[1], shade=TRUE)
ggGam(mod3a, by=fuel)

# example 3
mod4 <- gam(copper ~ s(dist, by = landuse) + landuse, data = meuse, method = "REML") 
mod4a <- gam(copper ~ s(dist, landuse, bs = "fs"), data = meuse, method = "REML") 
plot(mod4,pages=1, all.terms=TRUE, seWithMean = TRUE, shift=coef(mod3a)[1], shade=TRUE)
plot(mod4a,pages=1, all.terms=TRUE, seWithMean = TRUE, shift=coef(mod3a)[1], shade=TRUE)
vis.gam(mod4,view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod4a,view = c("dist", "landuse"), plot.type = "persp")

# example 4
# tensor smooth (different unit)
tensorMod <- gam(cadmium ~ te(x, y, elev), data = meuse, method = "REML")
summary(tensorMod)
plot(tensorMod)
tensorMod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev), data = meuse, method = "REML")
summary(tensorMod2)
plot(tensorMod2, pages=1)
vis.gam(tensorMod2, theta=100, phi=30)



# Chapter 5 (logistic)

# require packages
require(mgcv)
require(ggplot2)
require(ggpubr)
require(egg)
require(moonBook)
require(ztable)
require(ggGam)

# example 1
data(wesdr,package="gamair")
str(wesdr)
ggCompare <- function(data,y,group) {
            yvar <- as.character(substitute(y))
            groupvar <- as.character(substitute(group))
            if(!is.factor(data[[groupvar]])) data[[groupvar]] <- factor(data[[groupvar]])
            p1 <- ggplot(data,aes_string(x=yvar,fill=groupvar))+
              geom_density(alpha=0.5,size=0.3)+theme_article()+
              theme(legend.position="None")
            p2 <- ggplot(data,aes_string(x=groupvar,y=yvar,fill=groupvar))+
              geom_boxplot(alpha=0.5)+theme_article()+
              theme(legend.position="None")
            ggpubr::ggarrange(p1,p2,ncol=2) }
ggCompare(wesdr,y=dur,group=ret)
ggCompare(wesdr,y=gly,group=ret)
ggCompare(wesdr,y=bmi,group=ret)
z <- ztable(mytable(ret~.,data=wesdr))
print(z,type="html")
m <- gam(ret~s(dur)+s(gly)+s(bmi)+ti(dur,gly)+ti(dur,bmi)+ti(gly,bmi),select=TRUE,
         data=wesdr,family=binomial,method="REML")
ow <- options(warn=-1) ## avoid complaint about zlim 
plot(m,pages=1,scheme=1,zlim=c(-3,3))
options(ow)
summary(m)
vis.gam(m,view=c("gly","bmi"),se=1,color="bw",theta=-40,phi=40)
plot(m,select=1,shade=TRUE,seWithMean = TRUE,shift=coef(m)[1])
par(mfrow=c(2,2))
gam.check(m)
par(mfrow=c(1,1))
ggGam(m)



# Chapter 6 (survival data)

# require packages
require(mgcv)
require(survival)
require(ggplot2)

# example 1 (primary biliary cirrhosis)
data(pbc,package="survival")
str(pbc)
pbc$status1 <- as.numeric(pbc$status==2)
pbc$stage <- factor(pbc$stage)
b0 <- gam(time ~ trt+sex+stage+s(sqrt(protime))+s(platelet)+
          s(age)+s(bili)+s(albumin)+s(sqrt(ast))+s(alk.phos), 
          weights=status1, family=cox.ph, data=pbc, method="REML")
summary(b0)
anova(b0)
plot(b0,pages=1,all.terms=TRUE,seWithMean=TRUE,shift=coef(b0)[1],shade= TRUE)
b <- gam(time ~ trt+sex+s(sqrt(protime))+s(platelet)+s(age)+s(bili)+s(albumin),
        weights=status1, family=cox.ph, data=pbc, method="REML")
par(mfrow=c(2,2))
gam.check(b)
par(mfrow=c(1,1))
df <- data.frame(x=b$linear.predictors,y=residuals(b))
ggplot(df,aes(x=x,y=y))+geom_point()+
  labs(x="linear predictor",y="residuals")+theme_bw()
summary(b)
anova(b)
plot(b,pages=1,seWithMean=TRUE,shift=coef(b0)[1],shade=TRUE)

# drawSurv function
drawSurv <- function(model,data,np=100,timevar="time",until=NULL,id=list()){
  if(is.null(until)) until=max(model$model[[timevar]],na.rm=TRUE)
  if(length(id)==0) id=list(id=1:nrow(data))
  for(i in 1:nrow(data)){
    newd <- data.frame(matrix(0,np,0))
    for (n in names(data)) newd[[n]] <- rep(data[[n]][i],np)
    newd$time <- seq(0,until,length=np)
    fv <- predict(model,newdata=newd,type="response",se=TRUE)
    newd$fit=fv$fit
    # newd$ymax=fv$fit+se*fv$se.fit
    # newd$ymin=fv$fit-se*fv$se.fit
    se <- fv$se.fit/fv$fit
    newd$ymax=exp(log(fv$fit)+se)
    newd$ymin=exp(log(fv$fit)-se)
    idname=names(id)[1]
    newd[[idname]]=id[[1]][i]
    if(i==1){
      final=newd
    } else{
      final=rbind(final,newd)
    }
  }
  final[[idname]]=factor(final[[idname]])
  final
  ggplot(data=final,aes_string(x="time",y="fit",fill=idname,group=idname))+
    geom_line(aes_string(color=idname))+
    geom_ribbon(aes_string(ymax="ymax",ymin="ymin"),alpha=0.3)+
    ylim(c(0,1)) + ylab("cumulative survival")+xlab("days")+
    theme_bw()+
    theme(legend.position = "top")
}
drawSurv(b,data=pbc[c(10,66,5),],id=list(id=c(10,66,25)))

# averageData function 1
averageData <- function(data){
  newd=list()    
  for(i in 1:ncol(data)){
    if(is.numeric(data[[i]])) {
      newd[[i]]=mean(data[[i]],na.rm=TRUE)
    } else if(is.factor(data[[i]])){
      newd[[i]]=levels(data[[i]])[1]
    } else{
      newd[[i]]=sort(unique(data[[i]]))[1]
    }
  }
  names(newd)=names(data)
  df=as.data.frame(newd)
  df
}
drawSurv(b,data=averageData(pbc))

# averageData function 2
averageData <- function(data,newValue=list()){
  newd=list()    
  for(i in 1:ncol(data)){
    if(is.numeric(data[[i]])) {
      newd[[i]]=mean(data[[i]],na.rm=TRUE)
    } else if(is.factor(data[[i]])){
      newd[[i]]=levels(data[[i]])[1]
    } else{
      newd[[i]]=sort(unique(data[[i]]))[1]
    }
  }
  names(newd)=names(data)
  df=as.data.frame(newd)
  df
  if(length(newValue)>0){
    no=length(newValue[[1]])
    for(i in 1:no){
      if(i==1) {
        final=df
      } else{
        final=rbind(final,df)
      }
    }
    final[[names(newValue)[1]]]=newValue[[1]]
    df=final
  }
  df
}
dfSex <- averageData(pbc,list(sex=c("m","f")))
dfSex
drawSurv(b,data=dfSex,id=list(sex=c("m","f")))
dfBili <- averageData(pbc,list(bili=c(1,10)))
dfBili
drawSurv(b,data=dfBili,id=list(bili=c(1,10)))



# Chapter 7

