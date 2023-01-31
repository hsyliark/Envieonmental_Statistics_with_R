cell_all <- read.csv("C:/Users/User/Desktop/논문데이터/cell_all.csv",sep=",",header=T)
cell_all <- cell_all[cell_all$spot=="J1" | cell_all$spot=="J2",]
cell_all <- cell_all[cell_all$spot=="J1",]

X <- cell_all[,8:24]
X_scale <- scale(X)
cell_all_scale <- cbind(cell_all[,1:7],X_scale)

# Generalized Linear Model

# reference : https://rfriend.tistory.com/490
glm.D93 <- glm(blue ~ BOD + COD + TN + TP + TOC +
                 SS + EC + pH + DO + temperature +
                 turbidity + transparency + Chla + LowWaterLevel + inflow +
                 discharge + reservoir, data=cell_all_scale, family = poisson(link=log)) # Poisson Regression Model (Variance = Mean)
summary(glm.D93)
step.glm.D93 <- step(glm.D93,direction="both") # stepwise regression
library(car)
vif(step.glm.D93)
predict(step.glm.D93, newdata=cell_all_scale, type="response")






# reference : https://www.youtube.com/watch?v=Scr2uQqLkjI
library(MASS)
glm.D94 <- glm.nb(blue ~ BOD + COD + TN + TP + TOC +
                    SS + EC + pH + DO + temperature +
                    turbidity + transparency + Chla + LowWaterLevel + inflow +
                    discharge + reservoir, data=cell_all_scale, link=log) # Negative Binomial Regression Model (Variance > Mean)

glm.D94 <- glm(blue ~ BOD + COD + TN + TP + TOC +
                 SS + EC + pH + DO + temperature +
                 turbidity + transparency + Chla + LowWaterLevel + inflow +
                 discharge + reservoir, data=cell_all_scale, family = negative.binomial(1000))
summary(glm.D94)
step.glm.D94 <- step(glm.D94,direction="both")
vif(step.glm.D94)
predict(step.glm.D94, newdata=cell_all_scale, type="response")


# Zero-Inflated
# reference : https://m.blog.naver.com/ollehw/221581563165
library(ggplot2)
library(pscl)
library(boot)

ggplot(cell_all_scale, aes(blue)) + geom_histogram()

zip_model <- zeroinfl(blue ~ BOD + COD + TN + TP + TOC +
                        SS + EC + pH + DO + temperature +
                        turbidity + transparency + Chla + LowWaterLevel + inflow +
                        discharge + reservoir, data=cell_all_scale, link="logit", dist="poisson")
summary(zip_model)
library(mpath)
step.zip_model <- be.zeroinfl(zip_model, data=cell_all_scale, dist="poisson", alpha=0.05, trace=TRUE) # stepwise regression
vif(step.zip_model)
predict(step.zip_model, newdata=cell_all_scale, type="response")

zin_model <- zeroinfl(blue ~ BOD + COD + TN + TP + TOC +
                        SS + EC + pH + DO + temperature +
                        turbidity + transparency + Chla + LowWaterLevel + inflow +
                        discharge + reservoir, data=cell_all_scale, link="logit", dist="negbin")
summary(zin_model)
step.zin_model <- be.zeroinfl(zin_model, data=cell_all_scale, dist="negbin", alpha=0.05, trace=TRUE) # stepwise regression
vif(step.zin_model)
predict(step.zin_model, newdata=cell_all_scale, type="response")




# Generalized Additive Model

library(mgcv)
library(gam)

cell_all_scale_a <- cell_all_scale[,8:24]
gam.D93 <- gam::gam(blue ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(temperature) +
                      s(turbidity) + s(transparency) + s(Chla) + s(LowWaterLevel) + s(inflow) +
                      s(discharge) + s(reservoir), data=cell_all_scale, family=poisson, link=log)
summary(gam.D93)
step.gam.D93 <- step.Gam(gam.D93,direction="both",scope=gam.scope(cell_all_scale_a)) # stepwise regression in GAM
vif(step.gam.D93)
predict(step.gam.D93, newdata=cell_all_scale, type="response")

gam.D94 <- gam::gam(blue ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                      s(SS) + s(EC) + s(pH) + s(DO) + s(temperature) +
                      s(turbidity) + s(transparency) + s(Chla) + s(LowWaterLevel) + s(inflow) +
                      s(discharge) + s(reservoir), data=cell_all_scale, family=nb, link=log)
summary(gam.D94)
step.gam.D94 <- step.Gam(gam.D94,direction="both",scope=gam.scope(cell_all_scale_a))
vif(step.gam.D94)
predict(step.gam.D94, newdata=cell_all_scale, type="response")
