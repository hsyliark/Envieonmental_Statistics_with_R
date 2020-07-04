water <- read.csv("C:/Users/Nier/Desktop/LASSO 결과정리(scree 이용)/PCA에 Rain, Flow 포함/csv파일/10_함평.csv", sep=",", header=T)
water1 <- water
water_scale <- scale(water)
water1_scale <- scale(water1)

## Correlation analysis

install.packages('corrplot') 
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)
X <- round(cor(water_scale, method='spearman'),4)
pairs(X) # Correlation plot 1
corrplot(X) # Correlation plot 2
X # Correlation matrix
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(water_scale, method='spearman')
p.mat

# Using ggplot2, ggcorrplot
# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(X, hc.order=T, type="lower", lab=T) +
  ggtitle("Correlation plot(Spearman) for water quality data(all site)") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(X, hc.order=T, type="lower", p.mat=p.mat) +
  ggtitle("Correlation plot(Spearman) for water quality data(all site) with significance level") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))


## Principal Component Analysis

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)

# Descriptive statistics
describe(water1_scale)

# Correlation
round(cor(water1_scale),3) # pearson
round(cor(water1_scale, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(water1_scale)
cortest.bartlett(cor(water1_scale, method="spearman"), n=nrow(water1_scale))

# Number of principal components 
water_pca <- prcomp(water1_scale, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

# Component matrix 
PCA <- principal(water1_scale, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(water1_scale, nfactor=3, rotate="varimax", score=T) # varimax rotate 
PCA_rot
biplot(PCA_rot, main="Biplot")

## 3D biplot
# reference : https://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
install.packages("rgl")
library(rgl)
plot3d(PCA_rot$scores)
text3d(PCA_rot$scores, texts=rownames(water1_scale))
text3d(PCA_rot$loadings, texts=rownames(PCA_rot$loadings), col="blue")
coords <- NULL
for (i in 1:nrow(PCA_rot$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),PCA_rot$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=3)
# reference : https://iwatobipen.wordpress.com/2015/10/23/make-3d-pca-plot/
# reference : https://cran.r-project.org/web/packages/pca3d/vignettes/pca3d.pdf
install.packages("pca3d")
library(pca3d)
water_pca <- prcomp(water1_scale, center=T, scale.=T)
pca3d(water_pca, biplot=T)
snapshotPCA3d(file="fitst_plot.png")

## Other biplot
# reference1 : https://rkabacoff.github.io/datavis/Other.html#d-scatterplot
# reference2 : https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
# reference3 : http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
install.packages("ggfortify")
library(ggfortify)
install.packages('ggplot2')
library(ggplot2)
water_pca <- prcomp(water_scale, center=T, scale.=T)
autoplot(water_pca, data = water_scale, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + ggtitle("biplot")
# reference4 : http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
res.pca <- PCA(water_scale, graph = FALSE)  
fviz_screeplot(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

## Lasso Regression
# reference : https://rpago.tistory.com/59
X <- as.matrix(water_scale[,-2])
y <- as.matrix(water_scale[,2])


# reference : https://niceguy1575.tistory.com/64

install.packages("glmnet")
library(glmnet)
install.packages("plotmo")
library(plotmo)

set.seed(1)
cv.lasso <- cv.glmnet(X, y, alpha=1)
plot(cv.lasso)

set.seed(1)
mod <- glmnet(X, y)
cvfit <- cv.glmnet(X, y)
glmcoef <- coef(mod, cvfit$lambda.min)
plot_glmnet(mod, label=T, s=cvfit$lambda.min)
glmcoef


# LASSO coefficient with name
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)

set.seed(1)
lasso_model <- glmnet(X, y, alpha=1)
beta <- coef(lasso_model)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id="coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- log(lasso_model$lambda[tmp$variable+1])
tmp$norm <- apply(abs(beta[-1,]), 2, sum)
ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color=coef)) +
  geom_line(size=1) +
  xlab("Lambda (log scale)") +
  ylab("Coefficients") +
  guides(color=guide_legend(title=""),
         linetype=guide_legend(title="")) +
  theme_bw() +
  theme(legend.key.width = unit(3, "lines"))

# predict
install.packages("glmnet")
library(glmnet)
set.seed(1)
cv.lasso <- cv.glmnet(X, y, alpha=1)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
pred_y <- predict(cv.lasso, newx=X, s="lambda.min")
plot(y, pch=20, main="observation(black) v.s predict(red)")
lines(pred_y, col="red")

# NSE, Bias, MAE
install.packages("hydroGOF")
library(hydroGOF)
my_bias <- function(sim, obs) {
  bias = sum(sim-obs)/length(sim)
  return(bias)
}
obs <- y
sim <- pred_y
NSE(sim, obs)
my_bias(sim, obs)
mae(sim, obs)


# Elastic-net regression
# reference : https://daviddalpiaz.github.io/r4sl/elastic-net.html
install.packages("caret")
library(caret)
set.seed(42)
cv_10 = trainControl(method = "cv", number = 10)
hit_elnet = train(COD ~ ., data = water_scale, method = "glmnet", trControl = cv_10)
hit_elnet
plot(hit_elnet)

sh <- 10^seq(10,-2,length=100)
X <- as.matrix(water_scale[,-6])
y <- as.matrix(water_scale[,6])
best.elastic <- glmnet(X, y, alpha=1, lambda=sh)
predict(best.elastic, s=0.01781786, type="coefficients")



## Granger causality test
# reference : http://intothedata.com/02.scholar_category/timeseries_analysis/granger_causality/

# Install packages
install.packages("lmtest")
library(lmtest)

# Drawing graph
par(mfrow=c(1,2))
attach(water1_scale)
plot.ts(pH, main="????(pH)")
plot.ts(BOD, main="????(BOD)")
par(mfrow=c(1,1))

# Install packages
install.packages("forecast")
require(forecast)

# KPSS test (Finding difference for stationarity)
ndiffs(water1_scale$pH, alpha=0.05, test=c("kpss")) # 1
ndiffs(water1_scale$BOD, alpha=0.05, test=c("kpss")) # 1

# difference
diff_pH <- diff(water1_scale$pH, 1)
diff_BOD <- diff(water1_scale$BOD, 1)
par(mfrow=c(1,2))
plot.ts(diff_pH, main="????(pH) ????1 ????")
plot.ts(diff_BOD, main="????(BOD) ????1 ????")
par(mfrow=c(1,1))

grangertest(diff_BOD ~ diff_pH, order=1)
# result ~ cause
