# Reference : https://bookdown.org/cardiomoon/gam/
# Reference : https://be-favorite.tistory.com/59
# Reference : https://cran.r-project.org/web/packages/plsmselect/vignettes/plsmselect.html


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

library(survival)
library(ggplot2)
library(mgcv)

x=table(pbcseq$id)
df=as.data.frame(table(x))
ggplot(df,aes(x=x,y=Freq))+
  geom_col(fill="grey80",color="black")+
  labs(x="Follow-up", y="N")+
  theme_bw()

app <- function(x,t,to) {
  ## wrapper to approx for calling from apply...
  y <- if (sum(!is.na(x))<1) rep(NA,length(to)) else
    approx(t,x,to,method="constant",rule=2)$y
  if (is.factor(x)) factor(levels(x)[y],levels=levels(x)) else y
} ## app

tdpois <- function(dat,event="z",et="futime",t="day",status="status1",
                   id="id") {
  ## dat is data frame. id is patient id; et is event time; t is
  ## observation time; status is 1 for death 0 otherwise;
  ## event is name for Poisson response.
  if (event %in% names(dat)) warning("event name in use")
  require(utils) ## for progress bar
  te <- sort(unique(dat[[et]][dat[[status]]==1])) ## event times
  sid <- unique(dat[[id]])
  inter <- interactive()
  if (inter) prg <- txtProgressBar(min = 0, max = length(sid), initial = 0,
                                   char = "=",width = NA, title="Progress", style = 3)
  ## create dataframe for poisson model data
  dat[[event]] <- 0; start <- 1
  dap <- dat[rep(1:length(sid),length(te)),]
  for (i in 1:length(sid)) { ## work through patients
    di <- dat[dat[[id]]==sid[i],] ## ith patient's data
    tr <- te[te <= di[[et]][1]] ## times required for this patient
    ## Now do the interpolation of covariates to event times...
    um <- data.frame(lapply(X=di,FUN=app,t=di[[t]],to=tr))
    ## Mark the actual event...
    if (um[[et]][1]==max(tr)&&um[[status]][1]==1) um[[event]][nrow(um)] <- 1 
    um[[et]] <- tr ## reset time to relevant event times
    dap[start:(start-1+nrow(um)),] <- um ## copy to dap
    start <- start + nrow(um)
    if (inter) setTxtProgressBar(prg, i)
  }
  if (inter) close(prg)
  dap[1:(start-1),]
} ## tdpois

pbcseq$status1 <- as.numeric(pbcseq$status==2) ## deaths 
pb <- tdpois(pbcseq) ## conversion
pb$tf <- factor(pb$futime) ## add factor for event time

b0 <- bam(z ~ tf - 1 + trt + sex +stage+s(sqrt(protime))+s(platelet)+
            s(age)+s(bili)+s(albumin)+s(sqrt(ast))+s(alk.phos), 
          family=poisson, data=pb, discrete=TRUE,nthreads=2)

anova(b0)

b <- bam(z ~ tf - 1 + trt + s(sqrt(protime))+s(platelet)+
           s(age)+s(bili)+s(albumin)+s(sqrt(ast)), 
         family=poisson, data=pb, discrete=TRUE,nthreads=2)

anova(b)

chaz <- tapply(fitted(b),pb$id,sum) ## cum. hazard by subject 
d <- tapply(pb$z,pb$id,sum) ## censoring indicator
mrsd <- d - chaz ## Martingale residuals
drsd <- sign(mrsd)*sqrt(-2*(mrsd + d*log(chaz))) ## deviance

plot(b,pages=1,scale=0,scheme=1)

te <- sort(unique(pb$futime)) ## event times
di <- pbcseq[pbcseq$id==25,] ## data for subject 25
## interpolate to te using app from ?cox.pht...
pd <- data.frame(lapply(X=di,FUN=app,t=di$day,to=te))
pd$tf <- factor(te)
X <- predict(b,newdata=pd,type="lpmatrix")
eta <- drop(X%*%coef(b)); H <- cumsum(exp(eta))
J <- apply(exp(eta)*X,2,cumsum)
se <- diag(J%*%vcov(b)%*%t(J))^.5 
plot(stepfun(te,c(1,exp(-H))),do.points=FALSE,ylim=c(0.7,1),
     ylab="S(t)",xlab="t (days)",main="",lwd=2) 
lines(stepfun(te,c(1,exp(-H+se))),do.points=FALSE) 
lines(stepfun(te,c(1,exp(-H-se))),do.points=FALSE) 
rug(pbcseq$day[pbcseq$id==25]) ## measurement times

require(ggGam)
data=pbcseq[pbcseq$id %in% c(25),]
drawFUSurv(b,data)

er <- pbcseq[pbcseq$id==25,]
plot(er$day,er$protime,xlab="day",ylab="Prothrombin Time")
lines(te,pd$protime)
plot(er$day,er$platelet,xlab="day",ylab="Platelet")
lines(te,pd$platelet)

drawFUData(b,data,which=c("protime","platelet","bili","albumin","ast"))

pbc$status1 <- as.numeric(pbc$status==2)
bb = gam(time ~ trt +sex + s(sqrt(protime))+s(platelet)+ s(age)+s(bili)+s(albumin),
         weights=status1, family=cox.ph, data=pbc, method="REML")
drawSurv(bb,data=pbc[c(25),],id=list(id=c(25)))+
  theme(legend.position="None")+
  ggtitle("Prediction with baseline data")+
  ylim(c(0.7,1))
drawFUSurv(b,data)+theme(legend.position="None")+
  ggtitle("Prediction with follow-up data")+
  ylim(c(0.7,1))




## GAM LASSO

library(plsmselect)
library(purrr)
data(simData)

## Create model matrix X corresponding to linear terms
## (necessary for the formula option of gamlasso below)
simData$X = model.matrix(~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=simData)[,-1]

## The formula approach
gfit = gamlasso(Yg ~ X +
                  s(z1, k=5, bs="ts") +
                  s(z2, k=5, bs="ts") +
                  s(z3, k=5, bs="ts") +
                  s(z4, k=5, bs="ts"),
                data = simData,
                seed = 1)

## The term specification approach
gfit = gamlasso(response = "Yg",
                linear.terms = paste0("x",1:10),
                smooth.terms = paste0("z",1:4),
                data = simData,
                linear.penalty = "l1",
                smooth.penalty = "l1",
                num.knots = 5,
                seed = 1)

# mgcv::gam object:
class(gfit$gam)
# glmnet::cv.glmnet object
class(gfit$cv.glmnet)
summary(gfit)
## Plot the estimates of the smooth effects:
plot(gfit$gam, pages=1)
## Plot fitted versus observed values:
plot(simData$Yg, predict(gfit), xlab = "Observed values", ylab = "Fitted Values")

## Create model matrix X corresponding to linear terms
## (necessary for the formula option of gamlasso below)
simData$X = model.matrix(~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=simData)[,-1]

## Poisson response. Formula approach.
pfit = gamlasso(Yp ~ X + 
                  s(z1, bs="ts", k=5) + 
                  s(z2, bs="ts", k=5) + 
                  s(z3, bs="ts", k=5) + 
                  s(z4, bs="ts", k=5),
                data = simData,
                family = "poisson",
                seed = 1)

## Poisson response. Term-specification approach.
pfit = gamlasso(response = "Yp",
                linear.terms = paste0("x",1:10),
                smooth.terms = paste0("z",1:4),
                data = simData,
                linear.penalty = "l1",
                smooth.penalty = "l1",
                family = "poisson",
                num.knots = 5,
                seed = 1)

coef(pfit$cv.glmnet, s="lambda.min")

par(mfrow=c(1,2))
plot(pfit$gam, select=1) # estimate of smooth term z1
plot(pfit$gam, select=2) # estimate of smooth term z2

plot(predict(pfit, type="response"), exp(simData$lp), xlab="predicted count", ylab="true expected count")

## Create model matrix X corresponding to linear terms
## (necessary for the formula option of gamlasso below)
simData$X = model.matrix(~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=simData)[,-1]

## Bernoulli trials response
bfit = gamlasso(Yb ~ X + 
                  s(z1, bs="ts", k=5) + 
                  s(z2, bs="ts", k=5) + 
                  s(z3, bs="ts", k=5) + 
                  s(z4, bs="ts", k=5),
                data = simData,
                family = "binomial",
                seed = 1)

## The term specification approach
bfit = gamlasso(response = "Yb",
                linear.terms = paste0("x",1:10),
                smooth.terms = paste0("z",1:4),
                data = simData,
                family="binomial",
                linear.penalty = "l1",
                smooth.penalty = "l1",
                num.knots = 5,
                seed = 1)

summary(bfit)
plot(bfit$gam, pages=1)
pred.prob <- predict(bfit, type="response")
true.prob <- exp(simData$lp)/(1+exp(simData$lp))
plot(pred.prob, true.prob, xlab="predicted probability", ylab="true probability")

## Create model matrix X corresponding to linear terms
## (necessary for the formula option of gamlasso below)
simData$X = model.matrix(~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=simData)[,-1]

## Binomial counts response. Formula approach.
bfit2 = gamlasso(cbind(success,failure) ~ X + 
                   s(z1, bs="ts", k=5) + 
                   s(z2, bs="ts", k=5) + 
                   s(z3, bs="ts", k=5) + 
                   s(z4, bs="ts", k=5),
                 data = simData,
                 family = "binomial",
                 seed = 1)

## Binomial counts response. Term specification approach
bfit2 = gamlasso(c("success","failure"),
                 linear.terms=paste0("x",1:10),
                 smooth.terms=paste0("z",1:4),
                 data=simData,
                 family = "binomial",
                 linear.penalty = "l1",
                 smooth.penalty = "l1",
                 num.knots = 5,
                 seed=1)

summary(bfit2)
plot(bfit2$gam, pages=1)
pred.prob <- predict(bfit2, type="response")
true.prob <- exp(simData$lp)/(1+exp(simData$lp))
plot(pred.prob, true.prob, xlab="predicted probability", ylab="true probability")

## Create model matrix X corresponding to linear terms
## (necessary for the formula option of gamlasso below)
simData$X = model.matrix(~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=simData)[,-1]

# Censored time-to-event response. Formula approach.
cfit = gamlasso(time ~ X +
                  s(z1, bs="ts", k=5) +
                  s(z2, bs="ts", k=5) +
                  s(z3, bs="ts", k=5) +
                  s(z4, bs="ts", k=5),
                data = simData,
                family = "cox",
                weights = "status",
                seed = 1)

# Censored time-to-event response. Term specification approach.
cfit = gamlasso(response = "time",
                linear.terms = paste0("x",1:10),
                smooth.terms = paste0("z",1:4),
                data = simData,
                linear.penalty = "l1",
                smooth.penalty = "l1",
                family = "cox",
                weights="status",
                num.knots = 5,
                seed = 1)

## Obtain and plot predicted cumulative baseline hazard:
H0.pred <- cumbasehaz(cfit)

time.seq <- seq(0, 60, by=1)
plot(time.seq, H0.pred(time.seq), type="l", xlab = "Time", ylab="",
     main = "Predicted Cumulative \nBaseline Hazard")

## Obtain predicted survival at days 1,2,3,...,60:
S.pred <- predict(cfit, type="response", new.event.times=1:60)

## Plot the survival curve for sample (subject) 17:
plot(1:60, S.pred[17,], xlab="time (in days)", ylab="Survival probability", type="l")


