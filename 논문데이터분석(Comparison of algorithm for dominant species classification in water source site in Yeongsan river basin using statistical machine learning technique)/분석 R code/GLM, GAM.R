counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data.frame(treatment, outcome, counts) # showing data

glm.D93 <- glm(counts ~ outcome + treatment, family = poisson(link=log)) # Poisson Regression Model (Variance = Mean)
summary(glm.D93)
vif(glm.D93)
step(glm.D93,direction="both") # stepwise regression

glm.D94 <- glm(counts ~ outcome + treatment, family = negative.binomial(2))

library(MASS)
glm.D94 <- glm.nb(counts ~ outcome + treatment, link=log) # Negative Binomial Regression Model (Variance > Mean)
summary(glm.D94)
step(glm.D94,direction="both")
vif(glm.D94)

library(mgcv)
library(gam)
data(kyphosis)
kyphosis_a <- kyphosis[,2:3]
gam.D93 <- gam::gam(Start ~ s(Age) + s(Number), data=kyphosis, family=poisson, link=log)
summary(gam.D93)
step.Gam(gam.D93,direction="both",scope=gam.scope(kyphosis_a)) # stepwise regression in GAM

gam.D94 <- gam::gam(Start ~ s(Age) + s(Number), data=kyphosis, family=nb, link=log)
summary(gam.D94)
step.Gam(gam.D94,direction="both",scope=gam.scope(kyphosis_a))
