### Reference : http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

## Reading data
youngsanpo_1 <- read.csv("D:/Workplace/Environmental_Statistics_with_R/수질데이터분석(논문)/첫번째/전처리 후 데이터/데이터 (2014~2018)/상관분석(영산포-1) (2014~2018).csv",
                         header=T, sep=',')

## Correlation analysis

install.packages('corrplot') 
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)
X <- round(cor(youngsanpo_1[,3:13]),4)
pairs(X) # Correlation plot 1
corrplot(X) # Correlation plot 2
X # Correlation matrix
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(youngsanpo_1[,3:13])
p.mat

## Using ggplot2, ggcorrplot
ggcorrplot(X) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# method = "square" (default)
ggcorrplot(X, method = "circle") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# method = "circle"

# Reordering the correlation matrix
# using hierarchical clustering
ggcorrplot(X, hc.order=T, outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Types of correlogram layout
# Get the lower triangle
ggcorrplot(X, hc.order=T, type="lower", outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Get the upper triangle
ggcorrplot(X, hc.order=T, type="upper", outline.col="white") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(X, hc.order=T, type="lower",
           outline.col="white", ggtheme=ggplot2::theme_gray,
           colors=c("#6D9EC1", "white", "#E46726")) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(X, hc.order=T, type="lower", lab=T) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))

# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(X, hc.order=T, type="lower", p.mat=p.mat) +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
# Leave blank on no significant coefficient
ggcorrplot(X, p.mat=p.mat, hc.order=T, type="lower", insig="blank") +
  ggtitle("Correlation plot") +   
  theme(plot.title = element_text(family = "serif", 
                                  face = "bold", hjust = 0.5, 
                                  size = 15, color = "black"))
