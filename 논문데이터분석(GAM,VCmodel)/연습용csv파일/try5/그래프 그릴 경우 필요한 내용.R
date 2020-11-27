# MLR
ex1_1 <- read.csv("C:/Users/HSY/Desktop/광산(2020).csv",sep=",",header=T)
ex1_1 <- ex1_1[,-1]
ex1_1 <- as.data.frame(ex1_1)
pred.mlr <- predict(fit.step,newdata=ex1_1,type="response")
data.mlr <- data.frame(response=ex1_1$TC,fitted_values=pred.mlr,time=ex1_1$time)
sqrt(sum((data.mlr$response-data.mlr$fitted_values)^2)/length(data.mlr$response)) # RMSE = 20221.02   
ggplot(data.mlr, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Multiple Linear Regression)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.mlr$time,data.mlr$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Multiple Linear Regression)")
lines(data.mlr$time,data.mlr$response,type='o',col='blue')
legend(3650,120000,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))

# GLM
ex1_1 <- read.csv("C:/Users/HSY/Desktop/광산(2020).csv",sep=",",header=T)
ex1_1 <- ex1_1[,-1]
ex1_1 <- as.data.frame(ex1_1)
pred.glm <- predict(m.step,newdata=ex1_1,type="response")
data.glm <- data.frame(response=ex1_1$TC,fitted_values=pred.glm,time=ex1_1$time)
sqrt(sum((data.glm$response-data.glm$fitted_values)^2)/length(data.glm$response)) # RMSE = 28138.29  
ggplot(data.glm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Generalized Linear Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.glm$time,data.glm$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Generalized Linear Model)")
lines(data.glm$time,data.glm$response,type='o',col='blue')
legend(3650,150000,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))

# GAM
ex1_1 <- read.csv("C:/Users/HSY/Desktop/광산(2020).csv",sep=",",header=T)
ex1_1 <- ex1_1[,-1]
ex1_1 <- as.data.frame(ex1_1)
pred.gam <- predict(mm.shrink,newdata=ex1_1,type="response")
data.gam <- data.frame(response=ex1_1$TC,fitted_values=pred.gam,time=ex1_1$time)
sqrt(sum((data.gam$response-data.gam$fitted_values)^2)/length(data.gam$response)) # RMSE = 24703.02  
ggplot(data.gam, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Generalized Additive Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.gam$time,data.gam$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Generalized Additive Model)")
lines(data.gam$time,data.gam$response,type='o',col='blue')
legend(3650,150000,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))

# TVCM
ex1_1 <- read.csv("C:/Users/HSY/Desktop/광산(2020).csv",sep=",",header=T)
ex1_1 <- ex1_1[,-1]
ex1_1 <- as.data.frame(ex1_1)
pred.tvcm <- predict(vc.shrink,newdata=ex1_1,type="response")
data.tvcm <- data.frame(response=ex1_1$TC,fitted_values=pred.tvcm,time=ex1_1$time)
sqrt(sum((data.tvcm$response-data.tvcm$fitted_values)^2)/length(data.tvcm$response)) # RMSE = 92907.23 
ggplot(data.tvcm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Time Varying Coefficient Model)") +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
par(mfrow=c(1,1))
plot(data.tvcm$time,data.tvcm$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Time Varying Coefficient Model)")
lines(data.tvcm$time,data.tvcm$response,type='o',col='blue')
legend(3650,5e+05,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))





ggplot(data.mlr, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Multiple Linear Regression)") +
  coord_cartesian(xlim=c(0,100000),ylim=c(0,100000)) +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
ggplot(data.glm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Generalized Linear Model)") +
  coord_cartesian(xlim=c(0,100000),ylim=c(0,100000)) +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
ggplot(data.gam, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Generalized Additive Model)") +
  coord_cartesian(xlim=c(0,100000),ylim=c(0,100000)) +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)
ggplot(data.tvcm, aes(x=response, y=fitted_values)) +
  geom_point() + geom_rug() +
  ggtitle("response vs fitted_values (Time Varying Coefficient Model)") +
  coord_cartesian(xlim=c(0,100000),ylim=c(0,100000)) +
  geom_abline(intercept=0,slope=1,color='blue',size=1.5)





par(mfrow=c(1,1))
plot(data.mlr$time,data.mlr$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Multiple Linear Regression)",ylim=c(-40000,5e+05))
lines(data.mlr$time,data.mlr$response,type='o',col='blue')
legend(3650,5e+05,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))
plot(data.glm$time,data.glm$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Generalized Linear Model)",ylim=c(-40000,5e+05))
lines(data.glm$time,data.glm$response,type='o',col='blue')
legend(3650,5e+05,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))
plot(data.gam$time,data.gam$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Generalized Additive Model)",ylim=c(-40000,5e+05))
lines(data.gam$time,data.gam$response,type='o',col='blue')
legend(3650,5e+05,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))
plot(data.tvcm$time,data.tvcm$fitted_values,type='o',col='red',xlab="time",ylab="TC",main="response vs fitted_values (Time Varying Coefficient Model)",ylim=c(-40000,5e+05))
lines(data.tvcm$time,data.tvcm$response,type='o',col='blue')
legend(3650,5e+05,c("fitted_values","response"),lwd=c(1,1),col=c("red","blue"))
