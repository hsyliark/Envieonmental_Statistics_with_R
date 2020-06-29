water <- read.csv("C:/Users/Nier/Desktop/군집분석.csv", sep=",", header=T)
water_name <- water[,1]
rownames(water) <- water_name
water <- water[,-1]
water_scale <- scale(water)

install.packages("superheat")
library(superheat)
superheat(water_scale, scale = TRUE, left.label.text.size=3,
          bottom.label.text.size=3, bottom.label.size = .05,
          row.dendrogram = TRUE )
