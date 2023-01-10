J1 <- read.csv("C:/Users/User/Desktop/논문데이터/J1 spot.csv",sep=",",header=T)
J2 <- read.csv("C:/Users/User/Desktop/논문데이터/J2 spot.csv",sep=",",header=T)
T1 <- read.csv("C:/Users/User/Desktop/논문데이터/T1 spot.csv",sep=",",header=T)
T2 <- read.csv("C:/Users/User/Desktop/논문데이터/T2 spot.csv",sep=",",header=T)

library(psych)

describe(T2$Chl_a, trim = 0.05)
shapiro.test(T2$Chl_a)

table(T2$dominant)

spot1 <- rbind(J1,J2)
spot2 <- rbind(T1,T2)
spot <- rbind(spot1,spot2)
spot$spot <- as.factor(spot$spot)

library(ggplot2)

ggplot(spot, aes(x=spot, y=reservoir, fill=spot)) + geom_boxplot()

library(ggcorrplot)

# Compute a correlation matrix
corr <- cor(T2[,c(4:20)], method="spearman")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corr)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower", p.mat = p.mat)



library(kohonen)

data_all_matrix <- as.matrix(T2[,c(4:20)])

set.seed(1234)

som_grid <- somgrid(xdim=15, ydim=20, topo="hexagonal")
som_model <- som(data_all_matrix, grid=som_grid)

coolBlueHotRed <- function(n, alpha=1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

par(mfrow=c(1,1))
for (i in 1:17) {
  plot(som_model, type="property", property=getCodes(som_model)[,i], 
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)}
par(mfrow=c(1,1))
