## EDA

# reference : https://velog.io/@suzin/R-%EA%B8%B0%EC%B4%88-%EB%8D%B0%EC%9D%B4%ED%84%B0-%ED%8C%8C%EC%95%85%ED%95%98%EB%8A%94-%EB%B0%A9%EB%B2%95-1.-%EB%8D%B0%EC%9D%B4%ED%84%B0

library(tidyverse)

summary(diamonds)

library(prettyR) 
mean(diamonds$carat) 
mean(diamonds$carat, trim = 0.05) 
median(diamonds$carat) 
prettyR::Mode(diamonds$carat) 
max(diamonds$carat) 
min(diamonds$carat) 
range(diamonds$carat) 
diff(range(diamonds$carat)) 
IQR(diamonds$carat)
var(diamonds$carat) 
sd(diamonds$carat)
mad(diamonds$carat) 

library(e1071) 
e1071::skewness(diamonds$carat) 
e1071::kurtosis(iamonds$carat) 

diamonds %>% 
   dplyr::summarise(n = n(),
                    Mean        = mean(carat),
                    TrimmedMean = mean(carat, trim = 0.05),
                    Mediam      = median(carat),
                    Mode        = as.numeric(prettyR::Mode(carat)),
                    Range       = diff(range(carat)),
                    IQR         = IQR(carat),
                    VAR         = var(carat),
                    SD          = sd(carat),
                    MAD         = mad(carat),
                    SKEW        = e1071::skewness(carat),
                    KURT        = e1071::kurtosis(carat))

library(psych)

describe(diamonds$carat, trim = 0.05)


## Boxplot

# reference : http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

# Convert the variable dose from a numeric to a factor variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

library(ggplot2)
# Basic box plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
p + scale_x_discrete(limits=c("0.5", "2"))

# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

# Change box plot line colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_boxplot()
p

# Use custom color palettes
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()

# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
# Change box plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()
p

# Use custom color palettes
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
# Use grey scale
p + scale_fill_grey() + theme_classic()

p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend

p + scale_x_discrete(limits=c("2", "0.5", "1"))

# Change box plot colors by groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot()
# Change the position
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot(position=position_dodge(1))
p

# Add dots
p + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))
# Change colors
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Basic box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(fill="gray")+
  labs(title="Plot of length per dose",x="Dose (mg)", y = "Length")+
  theme_classic()
# Change  automatically color by groups
bp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
  geom_boxplot()+
  labs(title="Plot of length  per dose",x="Dose (mg)", y = "Length")
bp + theme_classic()

# Continuous colors
bp + scale_fill_brewer(palette="Blues") + theme_classic()
# Discrete colors
bp + scale_fill_brewer(palette="Dark2") + theme_minimal()
# Gradient colors
bp + scale_fill_brewer(palette="RdBu") + theme_minimal()






## Correlation analysis

# reference : http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

library(ggcorrplot)

# Compute a correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)
head(corr[, 1:6])

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(mtcars)
head(p.mat[, 1:4])

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)
# method = "circle"
ggcorrplot(corr, method = "circle")
# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")
# Get the upeper triangle
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           outline.col = "white")
# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = TRUE,
           type = "lower", p.mat = p.mat)
# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")


## Density plot

# reference : http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization

set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)
head(df)

library(ggplot2)
# Basic density
p <- ggplot(df, aes(x=weight)) + 
  geom_density()
p
# Add mean line
p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", size=1)

# Change line color and fill color
ggplot(df, aes(x=weight))+
  geom_density(color="darkblue", fill="lightblue")
# Change line type
ggplot(df, aes(x=weight))+
  geom_density(linetype="dashed")

library(plyr)
mu <- ddply(df, "sex", summarise, grp.mean=mean(weight))
head(mu)

# Change density plot line colors by groups
ggplot(df, aes(x=weight, color=sex)) +
  geom_density()
# Add mean lines
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")
p

# Use custom color palettes
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()

# Change density plot fill colors by groups
ggplot(df, aes(x=weight, fill=sex)) +
  geom_density()
# Use semi-transparent fill
p<-ggplot(df, aes(x=weight, fill=sex)) +
  geom_density(alpha=0.4)
p
# Add mean lines
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")

# Use custom color palettes
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
# Use grey scale
p + scale_fill_grey() + theme_classic()

p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend

# Histogram with density plot
ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
# Color by groups
ggplot(df, aes(x=weight, color=sex, fill=sex)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 

p<-ggplot(df, aes(x=weight))+
  geom_density()+facet_grid(sex ~ .)
p
# Add mean lines
p+geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
             linetype="dashed")

# Basic density
ggplot(df, aes(x=weight, fill=sex)) +
  geom_density(fill="gray")+
  geom_vline(aes(xintercept=mean(weight)), color="blue",
             linetype="dashed")+
  labs(title="Weight density curve",x="Weight(kg)", y = "Density")+
  theme_classic()
# Change line colors by groups
p<- ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")+
  labs(title="Weight density curve",x="Weight(kg)", y = "Density")

p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_classic()

# Continuous colors
p + scale_color_brewer(palette="Paired") + theme_classic()
# Discrete colors
p + scale_color_brewer(palette="Dark2") + theme_minimal()
# Gradient colors
p + scale_color_brewer(palette="Accent") + theme_minimal()








