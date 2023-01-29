counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data.frame(treatment, outcome, counts) # showing data 


# Generalized Linear Model

# reference : https://rfriend.tistory.com/490
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson(link=log)) # Poisson Regression Model (Variance = Mean)
summary(glm.D93)
vif(glm.D93)
step(glm.D93,direction="both") # stepwise regression

glm.D94 <- glm(counts ~ outcome + treatment, family = negative.binomial(2))

# reference : https://www.youtube.com/watch?v=Scr2uQqLkjI
library(MASS)
glm.D94 <- glm.nb(counts ~ outcome + treatment, link=log) # Negative Binomial Regression Model (Variance > Mean)
summary(glm.D94)
step(glm.D94,direction="both")
vif(glm.D94)



# Zero-Inflated
# reference : https://m.blog.naver.com/ollehw/221581563165
library(ggplot2)
library(pscl)
library(boot)

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

ggplot(zinb, aes(count)) + geom_histogram()

zip_model <- zeroinfl(count ~ child + camper + persons, data = zinb, link="logit", dist="poisson")
summary(zip_model)
library(mpath)
be.zeroinfl(zip_model, data=zinb, dist="poisson", alpha=0.05, trace=TRUE) # stepwise regression

zin_model <- zeroinfl(count ~ child + camper + persons, data = zinb, link="logit", dist="negbin")
summary(zin_model)
be.zeroinfl(zin_model, data=zinb, dist="negbin", alpha=0.05, trace=TRUE) # stepwise regression



# Generalized Additive Model

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
