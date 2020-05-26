## Import data
water5 <- read.csv("C:/Users/Nier/Desktop/수질데이터분석(논문)/2번째/분석자료 송부/5year/csv 파일/0.전체지점(5년).csv", header=T, sep=',')
water5_1 <- water5[,-(1:3)]
water5_1$logTC <- log(water5_1$TC)
water5_1$logFC <- log(water5_1$FC)
water5_1$logRain <- log(water5_1$Rain)
water5_1 <- water5_1[,-11]
water5_1 <- water5_1[,-17]
water5_1 <- water5_1[,-17] # water5_1 : 원데이터 (TC, FC, Rain 에 log 취함, 기초통계량 산정)
water5_2 <- scale(water5_1) # water5_2 : 표준화 (상관분석)
water5_3 <- cbind(water5_2[,-(17:18)],water5_1[,17:18]) # water5_3 : 표준화 (logTC, logFC 제외, 회귀분석)

 

## Principal Component Analysis

# Install and Attach required library
install.packages("psych") # for descriptive statistics
library(psych)

# Descriptive statistics
describe(water5[,-(1:3)])

# Correlation
round(cor(water5_2),3) # pearson
round(cor(water5_2, method="spearman"),3) # spearman

# KMO and Bartlett's test
KMO(water5_2)
cortest.bartlett(cor(water5_2), n=nrow(water5_2))

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

# Visualization
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



## Factor Analysis
# reference : https://datacookbook.kr/39

# 주성분 요인법을 이용한 요인분석
# 데이터 읽기
med.data <- read.table("D:/myTemp/mvadata/medFactor.txt", header=T)
head(med.data)
summary(med.data)

#초기 요인분석 실행
install.packages(c("psych","GPArotation"))
library(psych)
library(GPArotation)
med.factor <- principal(med.data, rotate="none")
names(med.factor)
med.factor$values
plot(med.factor$values, type="b")

med.Varimax <- principal(med.data, nfactors = 3, rotate="varimax")
med.Varimax

med.Varimax <- principal(med.data, nfactors = 2, rotate="varimax")
biplot(med.Varimax)

# 사각회전 OBLIMIN (maximum likelihood estimation)
stats.fact <- factanal(med.data, factors = 3, rotation="oblimin")
stats.fact


## 추가참고자료
# PCA : https://rfriend.tistory.com/61?category=601862
# FA : https://rfriend.tistory.com/62
