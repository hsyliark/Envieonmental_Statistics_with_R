
############################################## 1. 자료 생성 (이미 완료함) -> 2번으로 가기
library(raster)
nation <- shapefile("광역시도_한반도만")
head(nation@data)
seoul = subset(nation, CTP_ENG_NM=="Seoul")
plot(seoul)

# nation_d_R <- raster("nation_d_R.tif") #dissove된 한국지도
godo_R  <- raster("godo_R.tif")
road_R  <- raster("road_R.tif") #도로와의 거리(Road)
tavg_R <- raster("tavg_R.tif")
prcp_R <- raster("prcp_R.tif")

r2 <- crop(godo_R, extent(seoul))
godo_c <- mask(r2, seoul)
r2 <- crop(road_R, extent(seoul))
road_c <- mask(r2, seoul)
r2 <- crop(tavg_R, extent(seoul))
tavg_c<- mask(r2, seoul)
r2 <- crop(prcp_R, extent(seoul))
prcp_c<- mask(r2, seoul)

predictors <- stack(godo_c, road_c, tavg_c, prcp_c)

MT.p <- shapefile("p_ver3_KCB2010")
plot(MT.p)
MT.p_c <- MT.p[seoul,]
plot(seoul)
# plot(MT.p_c, add=T)

# save(predictors, MT.p, MT.p_c, file = "predictors.Rdata")

############################################## 2. 모델 제작 및 지도화(시각화)
######### 
load("predictors.Rdata")
predictors
MT.p_c
#MT.p

library(dismo)
mx.model <- maxent(predictors, MT.p_c)
mx.model

## 
mx.map <- predict(predictors, mx.model, progress='text')
plot(mx.map)


