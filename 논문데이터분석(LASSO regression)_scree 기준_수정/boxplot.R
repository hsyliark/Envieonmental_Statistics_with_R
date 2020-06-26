## reference : https://woosa7.github.io/R-boxplot-ggplot/

water <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(LASSO regression)_모델적합도, boxplot/자료, 결과정리/PCA에 Rain, Flow 포함/csv파일/boxplot1.csv", sep=",", header=T)

install.packages("ggplot2")
library(ggplot2)

ggplot(water, aes(factor(point), Flow)) + 
  geom_boxplot(aes(fill=(point))) +
  ggtitle("Distribution of Flow") +
  xlab("지점") + ylab("Flow")
