J1 <- read.csv("C:/Users/User/Desktop/논문데이터/J1 spot.csv",sep=",",header=T)
J2 <- read.csv("C:/Users/User/Desktop/논문데이터/J2 spot.csv",sep=",",header=T)
T1 <- read.csv("C:/Users/User/Desktop/논문데이터/T1 spot.csv",sep=",",header=T)
T2 <- read.csv("C:/Users/User/Desktop/논문데이터/T2 spot.csv",sep=",",header=T)
cell_all <- read.csv("C:/Users/User/Desktop/논문데이터/cell_all.csv",sep=",",header=T)

cell_all <- cell_all[cell_all$year != 2022,]


library(psych)

describe(cell_all$BOD, trim = 0.05)
shapiro.test(cell_all$BOD)

library(tseries)

describe(cell_all$Reservoir, trim = 0.05)
jarque.bera.test(cell_all$Reservoir)

table(T2$dominant)

spot1 <- rbind(J1,J2)
spot2 <- rbind(T1,T2)
spot <- rbind(spot1,spot2)
spot$spot <- as.factor(spot$spot)

library(ggplot2)

ggplot(spot, aes(x=spot, y=Reservoir, fill=spot)) + geom_boxplot() +
  labs(x ="spot", y = "Reservoir(10,000㎥)") +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"))

library(ggcorrplot)

# Compute a correlation matrix
corr <- cor(cell_all[,c(9:25)], method="spearman")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corr)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

ggcorrplot(corr, hc.order = TRUE, method = "circle",  type = "lower",
           lab = TRUE)
ggcorrplot(corr, hc.order = TRUE, method = "circle",
           type = "lower", p.mat = p.mat)


# reference : https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(corrplot)
library(ggcorrplot)

# Compute a correlation matrix
corr <- cor(cell_all[,c(9:25)], method="spearman")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corr)
testRes = cor.mtest(mtcars, conf.level = 0.95)

corrplot.mixed(corr, order = 'AOE')
corrplot(corr, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('RdBu', 10))
corrplot(corr, order = 'hclust', addrect = 2)
corrplot(corr, method = 'square', diag = FALSE, order = 'hclust', addCoef.col = 'black',
         addrect = 3, rect.col = 'black', rect.lwd = 3, tl.pos = 'd')
corrplot(corr, method = 'shade', order = 'AOE', addCoef.col = 'black',
         diag = FALSE)
corrplot(corr, method = 'square', order = 'FPC', type = 'lower', 
         addCoef.col = 'black', diag = FALSE)
corrplot(corr, method = 'square', order = 'FPC', type = 'lower', 
         p.mat = p.mat, diag = FALSE)



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
