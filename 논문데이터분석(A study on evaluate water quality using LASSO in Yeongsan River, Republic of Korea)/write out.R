### Reading data
water1 <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(Elastic 추가)/분석자료/csv자료/3_광주1(5년).csv", sep=",", header=T)
water1_date <- water1[,1:3]
water1_quality <- water1[,-(1:3)]

## normalization
water1_quality$logTC <- log(water1_quality$TC)
water1_quality$logFC <- log(water1_quality$FC)
water1_quality_1 <- water1_quality[,-11]
water1_quality_1 <- water1_quality_1[,-14]
water_scale <- scale(water1_quality_1)
water1_scale <- cbind(water1_date, water_scale)

water_scale <- as.data.frame(water_scale)

water_scale_1 <- water_scale[,-(1:4)]
water_scale_1 <- water_scale_1[,-(10:11)] # Delete (pH, DO, EC, Temp, Flow, Rain) for PCA



setwd('C:/Users/Nier/Desktop/논문데이터분석(A study on checking water quality using LASSO in Yeongsan River  Republic of Korea)/분석자료/표준화')
write.csv(water1_scale, file='12_무안1(5년)_표준화.csv', row.names=F)

all_site <- read.csv("C:/Users/Nier/Desktop/논문데이터분석(A study on checking water quality using LASSO in Yeongsan River  Republic of Korea)/통계관련자료/전체지점.csv", sep=",", header=T)
all_site <- all_site[,-11]
all_site <- all_site[,-14]
all_site <- all_site[,-(18:19)]
all_site_scale <- as.data.frame(scale(all_site))