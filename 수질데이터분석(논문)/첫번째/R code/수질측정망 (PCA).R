## 2019년 10월 수질측정망 데이터 불러들이기 및 전처리
oct_water <- read.csv("X:/수질측정망 자료 통계분석/2019년 10월 측정망 전처리.csv", header=T, sep=',')
water <- oct_water[,-(1:2)] # 변수종류 : 수온, pH, DO, BOD, COD, SS, EC, T-N, T-P, TOC 
water_scale <- scale(water) # 표준정규분포 Z 변환


## Principal Component Analysis

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)

# Descriptive statistics
describe(water)

# Correlation
round(cor(water),3) # pearson
round(cor(water, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(water)
cortest.bartlett(cor(water), n=nrow(water))

# Number of principal components (주성분분석)
water_pca <- prcomp(water, center=T, scale.=T)
water_pca
screeplot(water_pca, type="l")
biplot(water_pca, main="Biplot")
summary(water_pca)
water_pca$sdev^2 # Eigenvalue with respect to principal components

# Component matrix (주성분회전)
PCA <- principal(water, nfactor=3, rotate="none", score=T) # The factor is the number of PC
PCA
PCA_rot <- principal(water, nfactor=3, rotate="varimax", score=T) # varimax rotate 
PCA_rot


## Visualization

PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + 
    geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min((max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x]))))
  datapc <- transform(datapc, v1 = 0.7 * mult * (get(x)), v2 = 0.7 * mult * (get(y)))
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                           size = 5, vjust=1, color=colors[3])
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                              arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, 
                              color=colors[4])
  plot 
  }
fit <- prcomp(water, center=T, scale.=T)
PCbiplot(fit, colors=c("black", "black", "red", "yellow"))

# Reference : https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
install.packages("ggplot2")
install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
pca <- prcomp(water, center=T, scale.=T)
autoplot(pca, data=water, colour='red', loadings=TRUE, loadings.colour='blue',
         loadings.label=TRUE, loadings.label.size=3)

# ggbiplot
install.packages("devtools")
library(devtools)
install.packages("ggbiplot")
library(ggbiplot)
g <- ggbiplot(water, choices = c(1, 2), obs.scale = 1, var.scale = 1,
              groups = dt_group, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)