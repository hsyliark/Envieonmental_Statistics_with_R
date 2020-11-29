### MLR

#### response vs fitted_values (train data (2010-2019))

```{r}
pred.mlr <- predict(gfit.step,newdata=ex2,type="response")
data.mlr <- data.frame(response=ex2$TC,fitted_values=pred.mlr,
                       time=ex2$time)
sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/
       length(data.mlr$response)) # RMSE =   
ggplot(data.mlr, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC train data] response vs fitted_values 
          (Multiple Linear Regression)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.mlr$time,data.mlr$response,type='o',col='blue',
     xlab="time",ylab="TC",main="[Uchi TC train data] 
     response vs fitted_values (Multiple Linear Regression)",
     ylim=c(-100000,500000))
lines(data.mlr$time,data.mlr$fitted_values,type='o',col='red')
legend(0,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```

#### response vs fitted_values (test data (2020))

```{r}
ex2_1 <- read.csv("C:/Users/HSY/Desktop/우치(2020).csv",
                  sep=",",header=T)
ex2_1 <- ex2_1[,-1]
ex2_1 <- as.data.frame(ex2_1)
pred.mlr1 <- predict(gfit.step,newdata=ex2_1,type="response")
data.mlr1 <- data.frame(response=ex2_1$TC,fitted_values=pred.mlr1,
                        time=ex2_1$time)
sqrt(sum((data.mlr1$response-data.mlr1$fitted_values)^2)/
       length(data.mlr1$response)) # RMSE =    
ggplot(data.mlr1, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC test data] response vs fitted_values 
          (Multiple Linear Regression)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.mlr1$time,data.mlr1$response,type='o',col='blue',
     xlab="time",ylab="TC",main="[Uchi TC test data] 
     response vs fitted_values (Multiple Linear Regression)",
     ylim=c(-100000,500000))
lines(data.mlr1$time,data.mlr1$fitted_values,type='o',col='red')
legend(3650,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```



### GLM

#### response vs fitted_values (train data (2010-2019))

```{r}
pred.glm <- predict(m.step,newdata=ex2,type="response")
data.glm <- data.frame(response=ex2$TC,fitted_values=pred.glm,
                       time=ex2$time)
sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/
       length(data.glm$response)) # RMSE =    
ggplot(data.glm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC train data] response vs fitted_values 
          (Generalized Linear Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.glm$time,data.glm$response,type='o',col='blue',
     xlab="time",ylab="TC",main="[Uchi TC train data] 
     response vs fitted_values (Generalized Linear Model)",
     ylim=c(-100000,500000))
lines(data.glm$time,data.glm$fitted_values,type='o',col='red')
legend(0,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```

#### response vs fitted_values (test data (2020))

```{r}
ex2_1 <- read.csv("C:/Users/HSY/Desktop/우치(2020).csv",
                  sep=",",header=T)
ex2_1 <- ex2_1[,-1]
ex2_1 <- as.data.frame(ex2_1)
pred.glm1 <- predict(m.step,newdata=ex2_1,type="response")
data.glm1 <- data.frame(response=ex2_1$TC,fitted_values=pred.glm1,
                        time=ex2_1$time)
sqrt(sum((data.glm1$response-data.glm1$fitted_values)^2)/
       length(data.glm1$response)) # RMSE =  
ggplot(data.glm1, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC test data] response vs fitted_values 
          (Generalized Linear Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.glm1$time,data.glm1$fitted_values,type='o',col='red',
     xlab="time",ylab="TC",main="[Uchi TC test data] 
     response vs fitted_values (Generalized Linear Model)",
     ylim=c(-100000,500000))
lines(data.glm1$time,data.glm1$response,type='o',col='blue')
legend(3650,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```



### GAM

#### response vs fitted_values (train data (2010-2019))

```{r}
pred.gam <- predict(mm.shrink,newdata=ex2,type="response")
data.gam <- data.frame(response=ex2$TC,fitted_values=pred.gam,
                       time=ex2$time)
sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/
       length(data.gam$response)) # RMSE =     
ggplot(data.gam, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC train data] response vs fitted_values 
          (Generalized Additive Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.gam$time,data.gam$response,type='o',col='blue',
     xlab="time",ylab="TC",main="[Uchi TC train data] 
     response vs fitted_values (Generalized Additive Model)",
     ylim=c(-100000,500000))
lines(data.gam$time,data.gam$fitted_values,type='o',col='red')
legend(0,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```

#### response vs fitted_values (test data (2020))

```{r}
ex2_1 <- read.csv("C:/Users/HSY/Desktop/우치(2020).csv",
                  sep=",",header=T)
ex2_1 <- ex2_1[,-1]
ex2_1 <- as.data.frame(ex2_1)
pred.gam1 <- predict(mm.shrink,newdata=ex2_1,type="response")
data.gam1 <- data.frame(response=ex2_1$TC,fitted_values=pred.gam1,
                        time=ex2_1$time)
sqrt(sum((data.gam1$response-data.gam1$fitted_values)^2)/
       length(data.gam1$response)) # RMSE =   
ggplot(data.gam1, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC test data] response vs fitted_values 
          (Generalized Additive Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.gam1$time,data.gam1$fitted_values,type='o',col='red',
     xlab="time",ylab="TC",main="[Uchi TC test data] 
     response vs fitted_values (Generalized Additive Model)",
     ylim=c(-100000,500000))
lines(data.gam1$time,data.gam1$response,type='o',col='blue')
legend(3650,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```



### TVCM

#### response vs fitted_values (train data (2010-2019))

```{r}
pred.tvcm <- predict(vc.shrink,newdata=ex2,type="response")
data.tvcm <- data.frame(response=ex2$TC,fitted_values=pred.tvcm,
                        time=ex2$time)
sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/
       length(data.tvcm$response)) # RMSE =     
ggplot(data.tvcm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC train data] response vs fitted_values 
          (Time Varying Coefficient Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.tvcm$time,data.tvcm$response,type='o',col='blue',
     xlab="time",ylab="TC",main="[Uchi TC train data] 
     response vs fitted_values (Time Varying Coefficient Model)",
     ylim=c(-100000,500000))
lines(data.tvcm$time,data.tvcm$fitted_values,type='o',col='red')
legend(0,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```

#### response vs fitted_values (test data (2020))

```{r}
ex2_1 <- read.csv("C:/Users/HSY/Desktop/우치(2020).csv",
                  sep=",",header=T)
ex2_1 <- ex2_1[,-1]
ex2_1 <- as.data.frame(ex2_1)
pred.tvcm1 <- predict(vc.shrink,newdata=ex2_1,type="response")
data.tvcm1 <- data.frame(response=ex2_1$TC,fitted_values=pred.tvcm1,
                         time=ex2_1$time)
sqrt(sum((data.tvcm1$response-data.tvcm1$fitted_values)^2)/
       length(data.tvcm1$response)) # RMSE =   
ggplot(data.tvcm1, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("[Uchi TC test data] response vs fitted_values 
          (Time Varying Coefficient Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.tvcm1$time,data.tvcm1$fitted_values,type='o',col='red',
     xlab="time",ylab="TC",main="[Uchi TC test data] 
     response vs fitted_values (Time Varying Coefficient Model)",
     ylim=c(-100000,500000))
lines(data.tvcm1$time,data.tvcm1$response,type='o',col='blue')
legend(3650,500000,c("fitted_values","response"),lwd=c(1,1),
       col=c("red","blue"))
```