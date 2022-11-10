data1 <- seq(1,10,1)
data2 <- c(13,7,5,12,9,15,6,11,9,7,9)

mean(data1) ; mean(data2)

boxplot(data2)
range(data2)

install.packages("reshape2")
library(reshape2)
tips
head(tips)
mean(tips$total_bill)
mean(tips$tip)
p_tip <- (tips$tip/tips$total_bill)*100
tips <- cbind(tips,p_tip)
tips
mean(tips$p_tip)
sd(tips$p_tip)
str(tips)
table(tips$day)
table(tips$time)
table(tips$day,tips$time)
table(tips$sex)
table(tips$size)
table(tips$sex,tips$size)
boxplot(tips$total_bill ~ tips$sex)
boxplot(tips$tip ~ tips$sex)
boxplot(tips$tip ~ tips$day)
tips$day <- factor(tips$day, order=TRUE, levels=c("Thur","Fri","Sat","Sun"))
install.packages("fBasics")
library(fBasics)
kurtosis(tips$p_tip)
boxplot(tips$p_tip)
tips$N_tip <- scale(tips$tip)
mean(tips$N_tip)
sd(tips$N_tip)
tips
summary(tips)

cat("\f")

getwd()

install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)
df <- read_excel("Festival810.xlsx", sheet=1)
head(df)
table(df$gender)
nrow(df)
str(df)

install.packages("ggplot2")
library(ggplot2)
df$gender <- factor(df$gender, levels=c("Male","Female"))
table(df$gender)
t.test(df$day1 ~ df$gender)

aggregate(day1 ~ gender, data=df, mean)
aggregate(day1 ~ gender, data=df, sd)
boxplot(df$day1 ~ df$gender)
df$day1[611] <- 2.02

aggregate(day1 ~ gender, data=df, mean)
aggregate(day1 ~ gender, data=df, sd)
boxplot(df$day1 ~ df$gender)

var.test(df$day1 ~ df$gender)

install.packages("dplyr")
library(dplyr)

tmp <- group_by(df, gender)
summarise(tmp, M=mean(day1), SD=sd(day1))

df %>% 
  group_by(gender) %>% 
  summarise(mean(day1), sd(day1), min(day1), max(day1))

install.packages("psych")
library(psych)

describe(df$day1)
describeBy(df$day1, group=df$gender)

by(df$day1, df$gender, mean)
by(df$day1, df$gender, sd)

var.test(df$day1 ~ df$gender)
t.test(df$day1 ~ df$gender, var.equal=TRUE)

shapiro.test(df$day1)

by(df$day1, df$gender, shapiro.test)

RF <- read_excel("RExam.xlsx")
head(RF)

table(RF$uni)
var.test(RF$exam ~ RF$uni)
var.test(RF$computer ~ RF$uni)
var.test(RF$lectures ~ RF$uni)
var.test(RF$numeracy ~ RF$uni)
t.test(RF$exam ~ RF$uni, var.equal=TRUE)
t.test(RF$computer ~ RF$uni, var.equal=TRUE)
t.test(RF$lectures ~ RF$uni, var.equal=TRUE)
t.test(RF$numeracy ~ RF$uni, var.equal=FALSE)

library(reshape2)
data(tips)
head(tips)

table(tips$sex)
table(tips$smoker)
table(tips$time)

by(tips$tip, tips$sex, shapiro.test)
by(tips$tip, tips$smoker, shapiro.test)
by(tips$tip, tips$time, shapiro.test)

var.test(tips$tip ~ tips$sex) # p-value = 0.01117 
var.test(tips$tip ~ tips$smoker) # p-value = 0.8403
var.test(tips$tip ~ tips$time) # p-value = 0.1001

t.test(tips$tip ~ tips$sex, var.equal=FALSE) # p-value = 0.1378
t.test(tips$tip ~ tips$smoker, var.equal=TRUE) # p-value = 0.9266
t.test(tips$tip ~ tips$time, var.equal=TRUE) # p-value = 0.0578

install.packages("MASS")
library(MASS)
data(package="MASS")
data(Cars93)
head(Cars93)
str(Cars93)

Car <- Cars93

table(Car$Origin)
var.test(Car$Price ~ Car$Origin) # p-value = 0.01387

library(psych)
describeBy(Car$Price,Car$Origin)
t.test(Car$Price ~ Car$Origin, var.equal=FALSE) # p-value = 0.3428

table(Car$AirBags)
by(Car$Price, Car$AirBags, shapiro.test)

install.packages("car")
library(car)
car::leveneTest(Car$Price, Car$AirBags, center=mean) 
describeBy(Car$Price,Car$AirBags)
tmp <- aov(Car$Price ~ Car$AirBags)
summary(tmp)

kruskal.test(Car$Price ~ Car$AirBags)

install.packages("DescTools")
library(DescTools)

PostHocTest(tmp, method="lsd")
plot(PostHocTest(tmp, method="lsd"))
PostHocTest(tmp, method="bonferroni")
plot(PostHocTest(tmp, method="bonferroni"))
PostHocTest(tmp, method="hsd")
plot(PostHocTest(tmp, method="hsd"))
PostHocTest(tmp, method="scheffe")
plot(PostHocTest(tmp, method="scheffe"))
PostHocTest(tmp, method="duncan")
plot(PostHocTest(tmp, method="duncan"))

# Duncan
install.packages("agricolae")
library(agricolae)
duncan.test(tmp, "Car$AirBags", alpha = 0.05, console = TRUE)
# bonferroni
library(agricolae)
LSD <- LSD.test(tmp, "Car$AirBags", alpha = 0.05, p.adj=c("bonferroni"), group=F)
LSD
# scheffe
install.packages("doBy")
library(doBy)
scheffe.test(tmp, "Car$AirBags", alpha = 0.05, console = TRUE)

table(Car$DriveTrain)
by(Car$Price, Car$DriveTrain, shapiro.test)

car::leveneTest(Car$Price, Car$DriveTrain, center=mean) 
describeBy(Car$Price,Car$DriveTrain)
tmp <- aov(Car$Price ~ Car$DriveTrain)
summary(tmp)
oneway.test(Car$Price ~ Car$DriveTrain, var.equal = FALSE)

kruskal.test(Car$Price ~ Car$DriveTrain) # p-value = 0.001261

library(reshape2)
data(tips)
head(tips)
str(tips)

table(tips$day)
by(tips$tip, tips$day, shapiro.test) # p-value = 8.842e-09, p-value = 0.02633, p-value = 1.388e-05

kruskal.test(tips$tip ~ tips$day) # p-value = 0.03566

library(car)
car::leveneTest(tips$tip, tips$day, center=mean) # p-value = 0.6716 
describeBy(tips$tip, tips$day)
tmp <- aov(tips$tip ~ tips$day)
summary(tmp) # p-value = 0.174
oneway.test(tips$tip ~ tips$day, var.equal = TRUE) # p-value = 0.1736

head(Car)
table(Car$Type)
tmp <- aov(Car$Price ~ Car$Type)
summary(tmp)
duncan.test(tmp, "Car$Type", alpha = 0.05, console = TRUE)

tmp2 <- aov(Price ~ Type + AirBags + Type*AirBags, data=Car)
summary(tmp2)
PostHocTest(tmp2, method="duncan")

library(dplyr)
Car %>% 
  group_by(Type, AirBags) %>% 
  summarise(mean(Price))

table(Car$Type)

Car$C[Car$Type=="Compact"] <- 1
Car$C[Car$Type=="Large"] <- 2
Car$C[Car$Type=="Midsize"] <- 3
Car$C[Car$Type=="Small"] <- 4
Car$C[Car$Type=="Sporty"] <- 5
Car$C[Car$Type=="Van"] <- 6

table(Car$AirBags)

Car$B[Car$AirBags=="Driver & Passenger"] <- 1
Car$B[Car$AirBags=="Driver only"] <- 2
Car$B[Car$AirBags=="None"] <- 3

Car$G <- Car$C*10+Car$B
table(Car$G)

tmp22 <- aov(Car$Price ~ Car$G)
summary(tmp22)
duncan.test(tmp22, "Car$G", alpha = 0.05, console = TRUE)

tips
table(tips$day)
table(tips$time)
table(tips$day,tips$time)
by(tips$tip, tips$day, shapiro.test)
by(tips$tip, tips$time, shapiro.test)
tmp33 <- aov(tips$tip ~ tips$day + tips$time + tips$day*tips$time)
summary(tmp33)

library(stats)
friedman.test(formula = tip ~ day + time + day*time, data = tips)

df
library(ggplot2)
ggplot(df, aes(x=day1)) +
  geom_histogram(aes(y=..density..), binwidth=0.25, colour="blue", fill="red") +
  labs(x="Hygine (Day1 of Festival)", y="Density") +
  stat_function(fun=dnorm, args=list(mean(df$day1, na.rm=T),
                                     sd(df$day1, na.rm=T)),
                colour="black", size=1)

ggplot(df, aes(x=gender, y=day1)) +
  geom_boxplot() + 
  labs(x="Gender", y="Festival Day1")

ggplot(df, aes(x=day1, y=day2)) +
  geom_point(colour="green")

ggplot(df, aes(day1)) +
  geom_density()

cat("\f")
rm(list=ls())

Entry <- c(3,2,5,2,2,2,7,2,4,7,5,3,4,4,7,5,4,9,2,6,3,4,4,4,6,4,6,2,8,5)
Partner <- c(4,1,5,1,2,2,7,4,5,5,3,1,2,2,6,4,2,1,3,5,4,3,3,2,0,1,3,0,1,0)
dose <- c(rep(1,9),rep(2,8),rep(3,13))
dose <- factor(dose, levels=1:3, labels=c("Placebo","Low Dose","High Dose"))
Libido <- data.frame(dose, Entry, Partner)
str(Libido)

tmp <- aov(Entry ~ dose, data=Libido)
summary(tmp)
tmp1 <- aov(Entry ~ dose + Partner + dose*Partner, data=Libido)
summary(tmp1)
tmp2 <- aov(Entry ~ Partner + dose + dose*Partner, data=Libido)
summary(tmp2)
plot(Entry, Partner)
cor(Entry, Partner)

library(car)
Anova(tmp1, type="2")
Anova(tmp1, type="3")
Anova(tmp2, type="2")
Anova(tmp2, type="3")

pre <- c(77, 56, 64, 60, 58, 72, 67, 78, 67, 79)
post <- c(99, 80, 78, 65, 59, 67, 65, 85, 74, 80)
test <- data.frame(pre, post)
t.test(pre, post, data=test, paired=T)
mean(test$pre) ; sd(test$pre)
mean(test$post) ; sd(test$post)

Pair_R <- aov(post~pre)
summary(Pair_R)

No <- 1:18
RiverA <- c(51.0, 37.8, 14.7, 17.8, 13.2, 12.4, 6.0, 11.8, 18.9, 12.7, 11.6, 6.6, 18.8, 
            39.3, 31.5, 12.7, 9.7, 6.0)
RiverB <- c(107.8, 8.1, 13.1, 16.3, 11.7, 8.1, 8.6, 32.3, 36.0, 36.1, 25.4, 48.7, 17.1,
            0, 0, 0, 0, 0)
length(RiverA) ; length(RiverB)
data1 <- data.frame(No=No, A=RiverA, B=RiverB)
library(reshape2)
data1_Re <- melt(data1, id.vars=c("No"))[1:31,2:3]
colnames(data1_Re) <- c("River", "SS")
by(data1_Re$SS, data1_Re$River, shapiro.test)
wilcox.test(data1_Re$SS ~ data1_Re$River)
var.test(data1_Re$SS ~ data1_Re$River)
t.test(data1_Re$SS ~ data1_Re$River, var.equal=FALSE)

PreSS <- c(9.2, 14.3, 11.2, 21.9, 16.6, 14.5, 21.3, 21.4, 35.2, 17.2, 13.4, 
         6.9)
PostSS <- c(6.0, 11.8, 18.9, 12.7, 11.6, 6.6, 18.8, 39.3, 31.5, 12.7, 9.7, 
          6.0)
No <- length(PreSS)
diff <- PostSS - PreSS
data2 <- data.frame(No, PreSS, PostSS, diff)
shapiro.test(diff)
wilcox.test(PostSS, PreSS, data=data2, paired=TRUE)
t.test(PostSS, PreSS, data=data2, paired=TRUE)

No <- 1:10
Pre <- c(64,58,62,66,64,64,94,60,72,58)
Post <- c(88,70,76,78,80,60,92,66,70,56)
Run <- c(1,1,1,1,1,2,2,2,2,2)
Smoke <- c(2,2,1,1,2,2,1,2,2,2)
Gender <- c(1,1,1,1,1,2,2,2,2,2)
Height <- c(168,183,185,185,175,168,157,157,173,170)
Weight <- c(63.5,65.8,72.6,86.2,70.3,81.6,82.1,54.4,78.5,56.7)
Ex <- c(2,2,3,1,2,3,2,2,2,2)
data3 <- data.frame(No,Pre,Post,Run,Smoke,Gender,Height,Weight,Ex)

library(psych)
describe(data3)
shapiro.test(data3$Post - data3$Pre) # p-value = 0.2205
t.test(Post, Pre, data=data3, paired=TRUE)

data3$Diff <- data3$Post - data3$Pre
data3
by(data3$Diff, data3$Run, shapiro.test)
wilcox.test(data3$Diff ~ data3$Run)
var.test(data3$Diff ~ data3$Run)
t.test(data3$Diff ~ data3$Run, var.equal=TRUE)

data3$BMI <- data3$Weight / (data3$Height/100)^2
data3$BMI.G[data3$BMI >= 30.0] <- 4
data3$BMI.G[data3$BMI >= 25.0 & data3$BMI < 30.0] <- 3
data3$BMI.G[data3$BMI >= 20.0 & data3$BMI < 25.0] <- 2
data3$BMI.G[data3$BMI < 20.0] <- 1

tmp <- aov(Diff ~ Run + BMI.G + Run*BMI.G, data=data3)
summary(tmp)

A <- c(10,12,15,22,21)
B <- c(22,16,28,23,0)
No <- 1:5
df1 <- data.frame(No, A, B)
df1_m <- melt(df1, id.vars = "No")[-10,-1]
colnames(df1_m) <- c("Group", "Score")
by(df1_m$Score, df1_m$Group, shapiro.test)
var.test(df1_m$Score ~ df1_m$Group)
library(psych)
describeBy(df1_m$Score, df1_m$Group)
t.test(df1_m$Score ~ df1_m$Group, var.equal=T)
wilcox.test(df1_m$Score ~ df1_m$Group)

df <- data3
df
by(df$Diff, df$Run, shapiro.test)
wilcox.test(df$Diff ~ df$Run)

kw <- read.csv("C:/R_DATA/kwtest.csv")
kw
table(kw$soya)
by(kw$sperm, kw$soya, shapiro.test)
describeBy(kw$sperm, kw$soya)
kruskal.test(kw$sperm ~ kw$soya)

df
table(df$BMI.G)
by(df$Diff, df$BMI.G, shapiro.test)
describeBy(df$Diff, df$BMI.G)
kruskal.test(df$Diff ~ df$BMI.G)

head(airquality,3)
table(airquality$Month)
by(airquality$Temp, airquality$Month, shapiro.test)
describeBy(airquality$Temp, airquality$Month)
kruskal.test(airquality$Temp ~ as.factor(airquality$Month))
tmp <- aov(airquality$Temp ~ as.factor(airquality$Month))
summary(tmp)
library(DescTools)
PostHocTest(tmp, method="lsd")
plot(PostHocTest(tmp, method="lsd"))
PostHocTest(tmp, method="bonferroni")
plot(PostHocTest(tmp, method="bonferroni"))
PostHocTest(tmp, method="hsd")
plot(PostHocTest(tmp, method="hsd"))
PostHocTest(tmp, method="scheffe")
plot(PostHocTest(tmp, method="scheffe"))
PostHocTest(tmp, method="duncan")
plot(PostHocTest(tmp, method="duncan"))
library(agricolae)
duncan.test(tmp, "as.factor(airquality$Month)", alpha = 0.05, console = TRUE)
library(agricolae)
LSD <- LSD.test(tmp, "as.factor(airquality$Month)", alpha = 0.05, p.adj=c("bonferroni"), group=F)
LSD
library(doBy)
scheffe.test(tmp, "as.factor(airquality$Month)", alpha = 0.05, console = TRUE)

library(MASS)
head(Cars93)
str(Cars93)
CroT <- table(Cars93$DriveTrain, Cars93$AirBags)
margin.table(CroT, 1) 
margin.table(CroT, 2) 
chisq.test(CroT)
Ctab <- xtabs(~ Cars93$DriveTrain + Cars93$AirBags)
Ctab
prop.table(Ctab)
prop.table(Ctab,1)
prop.table(Ctab,2)
chisq.test(Ctab)

install.packages("gmodels")
library(gmodels)
CrossTable(CroT,expected=TRUE,prop.r=TRUE, 
           prop.c=TRUE,prop.t=TRUE,prop.chisq=TRUE)

install.packages("vcd")
library(vcd)
CT <- with(Cars93, table(DriveTrain, AirBags))
mosaic(CT, gp=gpar(fill=c("red", "blue", "green")), direction="v", 
       main="Mosaic plot of DriveTrain & AirBags")

Consultant <- c(1:16)
Aptitude <- c(45, 81, 65, 87, 68, 91, 77, 61, 55, 66, 82, 93, 76, 83, 61, 74)
Performance <- c(56, 74, 56, 81, 75, 84, 68, 52, 57, 82, 73, 90, 67, 79, 70, 66)
Personality <- c(9, 15, 11, 15, 14, 19, 12, 10, 9, 14, 15, 14, 16, 18, 15, 12)
FarMC <- data.frame(Consultant, Aptitude, Performance, Personality)
head(FarMC, 3)

with(FarMC, plot(Aptitude, Performance, pch=19))
with(FarMC, cor(Aptitude, Performance))
cor(FarMC)
plot(FarMC)
install.packages("GGally")
library(GGally)
ggpairs(FarMC)

FacMC <- FarMC[,2:4]
ggpairs(FacMC)

install.packages("corrgram")
library(corrgram)

airquality
summary(airquality)
cor(airquality, use="complete.obs")
cor(airquality, use="pairwise.complete.obs")
mean(airquality$Ozone, na.rm=T)
cor(FacMC, method="pearson")
cor(FacMC, method="spearman")

install.packages("UsingR")
library(UsingR)
library(ggplot2)
data(package="UsingR")
data(father.son)
with(father.son, plot(fheight, sheight))
ggplot(father.son, aes(x=fheight, y=sheight)) +
  geom_point(col="red") +
  labs(x="Father's height", y="Son's height") +
  geom_smooth(method="lm")
tmp <- lm(sheight ~ fheight, data=father.son)
summary(tmp)
with(father.son, cor(fheight, sheight))

install.packages("lm.beta")
library(lm.beta)
tmp2 <- lm.beta(tmp)
summary(tmp2)

library(reshape2)
tips
str(tips)
tmp <- lm(tip ~ ., data=tips)
summary(tmp)
tmp3 <- step(tmp, direction="both")
summary(tmp3)
tmp2 <- lm.beta(tmp3)
summary(tmp2)
