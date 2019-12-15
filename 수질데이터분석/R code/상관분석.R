### Reference : http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

## Reading data
youngsanpo_1 <- read.csv("C:/Users/HSY/Desktop/논문분석요청/전처리 후 데이터/상관분석(영산포-1).csv",
                         header=T, sep=',')

## Correlation analysis

install.packages('corrplot') 
library(corrplot)
X <- round(cor(youngsanpo_1[,3:13]),4)
pairs(X) # Correlation plot 1
corrplot(X) # Correlation plot 2
X # Correlation matrix
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(youngsanpo_1[,3:13])
p.mat

## Using ggplot2, ggcorrplot
install.packages('ggplot2')
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(X) # method = "square" (default)
ggcorrplot(X, method = "circle") # method = "circle"

# Reordering the correlation matrix
# using hierarchical clustering
ggcorrplot(X, hc.order=T, outline.col="white")

# Types of correlogram layout
# Get the lower triangle
ggcorrplot(X, hc.order=T, type="lower", outline.col="white")
# Get the upper triangle
ggcorrplot(X, hc.order=T, type="upper", outline.col="white")

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(X, hc.order=T, type="lower",
           outline.col="white", ggtheme=ggplot2::theme_gray,
           colors=c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(X, hc.order=T, type="lower", lab=T)

# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(X, hc.order=T, type="lower", p.mat=p.mat)
# Leave blank on no significant coefficient
ggcorrplot(X, p.mat=p.mat, hc.order=T, type="lower", insig="blank")
