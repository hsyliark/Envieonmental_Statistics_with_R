
# 본 프로그램은 2018년~2019년에 수행된『차세대 수질·수생태계 예측모델 개발 및 적용성 평가(II) 
# - 국내 수질·수생태계 모니터링 자료를 활용한 수생태모델 활용성 평가' 과제의 결과물입니다.
# 제작연월: 2019. 10월 
# 연락처: 경희대학교 생태정보학실험실(02-961-0946)

#####################################################################
################################### 1. 자료 불러오기

setwd("C:/Projects/NIER/하천생태모델/2018_자료기반/자료기반모델_실습")

## 풍부도자료(Abundance): 개체수
sp.DB <- read.csv("C:/Users/3층 전산강의실/Desktop/modeling/수생태_저서_생물.csv", 
                  stringsAsFactors=F)

## 환경 자료
Env.DB <- read.csv("C:/Users/3층 전산강의실/Desktop/modeling/수생태_저서_환경.csv", 
                   stringsAsFactors=F)

##### 생물 자료(풍부도) 확인 및 DB생성
names(sp.DB)
names(sp.DB)[2:length(names(sp.DB))] #실제 생물 열
used.sp.DB <- sp.DB[2:length(names(sp.DB))] 

# 명칭 확인
rownames(used.sp.DB) <- sp.DB[,1]

##### 환경 자료확인
names(Env.DB)
names(Env.DB)[1] #조사 지점 정보
names(Env.DB)[2] #하천차수
names(Env.DB)[3] #수온
names(Env.DB)[4:14] #수질1
names(Env.DB)[15:21] #토지피복
names(Env.DB)[23] #경사각
names(Env.DB)[29] #고도
names(Env.DB)[25:28] #기상조건
names(Env.DB)[30:32] #수리수문

##### 사용할 환경변수 지정
var.DB <- Env.DB[c(2,#하천차수
                   4:8,11,13:14,#수질- DO, pH, Conductivity, Turbidity, BOD, / T-N, / T-P, Chl-a
                   15,17,#토지피복- 1시가화건조지역, 3산림지역
                   29,#고도
                   25:28,#기상조건
                   30:32#수리수문- 수폭,수심,유속
)]

names(var.DB) #사용 환경변수 명칭
length(names(var.DB)) #사용 환경변수 개수

Total.DB <- cbind(used.sp.DB, var.DB)

#####################################################################
################################### 2. 모델 설정
##### 어류
# 사용모델: MaxEnt
# 자료형태: P/A

##### 필요 package
install.packages('dismo')
install.packages('rJava') # rJava

# install.packages('ROCR')
# install.packages('caret')
# install.packages('mgcv')
library(dismo)
system.file("java", package="dismo")
library(ROCR)
library(caret)
library(mgcv)

## Install Java for 64-bit
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_231') # for 64-bit version
library(rJava)


###### 모의 생물 종 선택
#모델을 제작하고자 하는 생물 종 선택
#형성된 자료의 열을 기준으로 생성
target.sp = 1 # 

names(used.sp.DB)[target.sp] #대상 생물 확인


#####################################################################
################################### 3. 자료의 전처리 과정
targetDB <- Total.DB 
total.num.sp <- dim(used.sp.DB)[2] # DB 내 포함된 생물종 수

### 전처리 과정 
#1. 사용 Data만 선택
m1 <- targetDB[,c(target.sp,(1+total.num.sp):dim(targetDB)[2])]
print(colnames(m1)[1]) #모의 생물 종 명칭

#2. 자료 내 NA 제거
m2 <- na.omit(m1)
names(m2)[1] <- "pop" #모의를 위한 종명칭 변경

#3. 생물 자료 정수화
m3 <- m2
m3[,1] <- round(m3[,1],0) # 정수화

#4. 출현여부(P/A) 변환 (1/0)
m4 <- m3
for(i in 1:length(m4[,1])){
  if(m3[i,1] > 0){
    m4[i,1] <- 1
  } else 
    m4[i,1] <- 0
}# for i

#5. 환경 변수 표준화(scale))
m5 <- m4 
#str(e1)
m5[,2:(dim(m4)[2])] <- scale(m4[,2:(dim(m4)[2])]) # 표준화: z= (Mean - X)/sd

#6. 환경 변수 정보(var.info) 저장
target.env.DB <- m4[,2:(dim(m4)[2])] # scale 하기 바로 직전 DB
total.var.name <- names(target.env.DB)
### 표준화 정보 
var.info <- as.data.frame(matrix(NA, nrow=length(total.var.name), ncol=3))
names(var.info) <- c("name", "mean", "sd")
for( i in 1:length(total.var.name)){
  var.info[i,1] <- total.var.name[i]
  var.info[i,2] <-mean(target.env.DB[,i], na.rm=T)
  var.info[i,3] <-sd(target.env.DB[,i], na.rm=T)
}



#####################################################################
################################### 4. 모델링
# 모델용 DB
model.DB <-  m5 

# model.DB 구성
dim(model.DB) # 20개 변수(pop + 19개), 41개 지점(site)

# 사용 DB 내 환경변수 열 지정
var.num <- c(2:dim(m5)[2])
names(model.DB)[var.num] # 사용 환경 변수명

##### 훈련(Training) 및 검증(Test) 자료 분할
DB_P <- subset(model.DB, model.DB$pop > 0) #출현 DB
DB_A <- subset(model.DB, model.DB$pop == 0) #비출현 DB
# 분할(훈련:검증 = 8:2)
set.seed(6808)
no.p <- sample(1:dim(DB_P)[1], dim(DB_P)[1]*0.8, replace=F) 
no.a <- sample(1:dim(DB_A)[1], dim(DB_A)[1]*0.8, replace=F)

train_DB <- rbind(DB_P[no.p,], DB_A[no.a,])
test_DB <-  rbind(DB_P[-no.p,], DB_A[-no.a,])
dim(train_DB)
dim(test_DB)

##### MaxEnt model 제작
Mx.DB <- train_DB[c(1,var.num)]
Mx.test_DB <- test_DB

set.seed(6808)
Mx.model <-  maxent(Mx.DB[c(var.num)],Mx.DB[,1])
e.Mx<- evaluate(subset(test_DB[c(1,var.num)], test_DB$pop > 0), subset(test_DB[c(1,var.num)], test_DB$pop == 0), Mx.model)
t.Mx <- threshold(e.Mx, 'spec_sens')

#####변수중요도
Mx.varimp <- plot(Mx.model) 

##### 모델 성능 평가
# 모델 검증 결과(test) 
Mx.test <- predict(Mx.model, Mx.test_DB, type='class')

### 1) AUC
Mx_p1 <- ROCR::prediction(as.numeric(as.vector(Mx.test)), Mx.test_DB$pop)
Mx_pp1 <- performance(Mx_p1, 'tpr', 'fpr')
Mx_auc1 <- performance(Mx_p1, "auc")
Mx_auc <- Mx_auc1@y.values
print(paste0("AUC : ",round(Mx_auc[[1]],3)))

### 2) ACC
test_matrix <- Mx.test_DB[,1]
for(i in 1:length(test_matrix)){
  if(Mx.test[i] > t.Mx){test_matrix[i] <- 1} else {test_matrix[i] <- 0}
}
Mx_test_ACC <- confusionMatrix(factor(Mx.test_DB[,"pop"]),factor(test_matrix))$overall[1]
print(paste0("ACC : ",round(Mx_test_ACC,3)))

# AUC, ACC 모음
Mx.mvalue.df <- data.frame(matrix(NA, nrow=1,ncol=2))
names(Mx.mvalue.df) <- c("AUC", "ACC")
Mx.mvalue.df[1,1] <- round(Mx_auc[[1]],3) #AUC
Mx.mvalue.df[1,2] <- round(Mx_test_ACC,3) #ACC
# Mx.mvalue.df

##### 입력자료에 대한 최종 서식확률 계산
Mx.result <- predict(Mx.model, model.DB[-1])


#####################################################################
################################### 5. 결과 정리 및 저장하기
#정리하기

# Mx.result #서식확률 예측결과
# Mx.model$importance[,2] #모델 내 종합된 변수 중요도
# Mx.mvalue.df #모델 평가값

#평가 대상 생물
names(Total.DB)[target.sp]

#저장하기
temp.result <- as.data.frame(Mx.result)
names(temp.result) <- c("출현확률")
write.csv(temp.result, paste0("(결과)어류_모의결과(",names(Total.DB)[target.sp],").csv"))

temp.varimp <- as.data.frame(Mx.varimp)  ## Mx.varimp로 수정
names(temp.varimp) <- names(Total.DB)[target.sp]
write.csv(temp.varimp, paste0("(결과)어류_변수중요도(",names(Total.DB)[target.sp],").csv"))

temp.eval <- as.data.frame(t(Mx.mvalue.df))
names(temp.eval) <- names(Total.DB)[target.sp]
write.csv(temp.eval, paste0("(결과)어류_모델평가값(",names(Total.DB)[target.sp],").csv"))


#####################################################################
################################### 6. 부분의존성그림(PDP) 그리기 (1~5분 소요)

###### 부분의존성 그림 설정
# 부분의존성 그림 내 대상 환경 변수 설정
names(Env.DB) #사용환경명과 동일하게 선택
PDP.variables = c("godo_R","tavg_R","Conductivity", "Turbidity", "BOD", "T.N", "T.P","수폭m", "수심cm", "유속cm.s")


##### PDP 옵션
Mx.model
PDP.variables # "2. 모델 설정" 단계에서 정한 주요 환경 변수


##### 그림 크기 설정
par(mar=c(4,3,1,1))
par(mfrow=c(3, 4)) # 3x4로 그림 배열 


##### 주요 환경 변수에 대해 모델 DB(Mx.DB) 내 위치(열) 찾기
for(i in 1:length(PDP.variables)){
  if(i == 1){ 
    PDP.var.num <- (1:length(names(Mx.DB[-1])))[names(Mx.DB[-1]) == PDP.variables[i]]
  } else {
    PDP.var.num <- c(PDP.var.num, (1:length(names(Mx.DB[-1])))[names(Mx.DB[-1]) == PDP.variables[i]])
  }
} # for i
PDP.var.num
#변수 확인
names(var.DB)[PDP.var.num] == PDP.variables # 모두 True


##### 그림 내 x축 범위 정하기 (Min, Max)
length(PDP.var.num) #주요 환경 변수 개수
names(var.DB)[PDP.var.num]
x.limit <- list()
range(na.omit(var.DB[,PDP.var.num[1]]))
x.limit[[1]] <- c(0, 800)
range(na.omit(var.DB[,PDP.var.num[2]]))
x.limit[[2]] <- c(5, 15)
#
range(na.omit(var.DB[,PDP.var.num[3]]))
x.limit[[3]] <- c(0,25000)
range(na.omit(var.DB[,PDP.var.num[4]]))
x.limit[[4]] <- c(0,350)
range(na.omit(var.DB[,PDP.var.num[5]]))
x.limit[[5]] <- c(0,12)
range(na.omit(var.DB[,PDP.var.num[6]]))
x.limit[[6]] <- c(0,12)
range(na.omit(var.DB[,PDP.var.num[7]]))
x.limit[[7]] <- c(0,1.2)
range(na.omit(var.DB[,PDP.var.num[8]]))
x.limit[[8]] <- c(0,1200)
range(na.omit(var.DB[,PDP.var.num[9]]))
x.limit[[9]] <- c(0,120)
range(na.omit(var.DB[,PDP.var.num[10]]))
x.limit[[10]] <- c(0,120)


##### PDP 그리기
for(j in 1:length(PDP.var.num)){ # 1) PD값 구하기
  temp <- model.DB[-1]
  xx <- unique(temp[,PDP.var.num[j]])
  yy <- numeric(length(xx)) 
  
  for(num in 1:length(xx)){
    temp[,j] <- xx[num]
    preds <- predict(Mx.model, temp) 
    yy[num] <- mean(preds)
  }
  
  # 2) GAM모델 용 DB 제작
  gam.iv.DB <- data.frame("y"=yy, "x"=(xx*var.info[PDP.var.num[j],3]+var.info[PDP.var.num[j],2] )) 
  
  if(length(unique(gam.iv.DB$x))>10){
    gam.iv <- predict(gam(y ~ s(x), data=gam.iv.DB), se=T) 
  } else {
    gam.iv <- predict(gam(y ~ s(x, k=length(unique(gam.iv.DB$x))), data=gam.iv.DB), se=T) 
  }
  
  fit <- gam.iv$fit
  se <- gam.iv$se.fit # 신뢰구간
  lcl <- fit - 1.96*se
  ucl <- fit + 1.96*se
  
  lin.bind <- data.frame("x"=gam.iv.DB$x, fit)
  pol.bind <- data.frame("x"=gam.iv.DB$x, ucl, lcl)
  
  x.pol <- c(gam.iv.DB[order(gam.iv.DB$x),2],gam.iv.DB[order(gam.iv.DB$x, decreasing=T),2])
  y.pol <- c(pol.bind[order(pol.bind$x),2], pol.bind[order(pol.bind$x, decreasing=T),3]) 
  
  ### y축 범위 재설정
  ylimit <- c(0.2, 0.8)
  
  # 3) PDP 그리기
  plot(c(0.5,0.5), xlim=c(x.limit[[j]][1], 
                          x.limit[[j]][2]), 
       ylim=ylimit, type="n", yaxt="n", xlab=PDP.variables[j], ylab="")
  grid()
  polygon(x.pol, y.pol, col="gray70",border=NA) #"black")
  lines(lin.bind[order(lin.bind$x),], col="black", lwd=2)
  axis(2, las=2)
}# for j


#####################################################################
################################### 7. 모델을 이용한 서식 확률 예측 (신규자료 바탕)

##### 신규자료 불러오기
# new.env <- read.csv("수생태_어류_환경.csv", stringsAsFactors=F)
new.env <- na.omit(var.DB)

##### 표준화
## 기존 모델 내 변수 순서와 신규 자료 내 변수 순서 맞춤
for(k in 1:length(names(new.env))){
  new.num <-(1:length(names(new.env)))[var.info[,1]== names(new.env)[k]]
  if(k == 1){
    new.num2 <- new.num  
  } else{
    new.num2 <- c(new.num2, new.num)
  }   
}
names(new.env)[new.num2] # 모델 내 변수 순서로 정렬
order.env <- new.env[new.num2]

## scale(표준화) : z = (Mean - X)/sd
scaled.env <- order.env
for(j in 1:length(new.num2)){
  scaled.env[,j] <- (order.env[,j] - var.info[j,2])/(var.info[j,3])
}

##### 모델 예측
Mx.pred <- predict(Mx.model, scaled.env)
pred.result <- data.frame("서식확률"=Mx.pred)

# 저장하기
write.csv(pred.result, "(R결과)어류_MaxEnt_신규모의결과.csv")

