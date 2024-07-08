### Clear workspace
dev.off()     # clear all plots
rm(list=ls()) # clear global Environmental
cat("\f")     # clear Console


#------------------------------------------------------------------------------------#

### Paired test 

## Import csv file 
water1 <- read.csv("C:/Users/User/Desktop/ex1_Paired.csv", sep=",", header=T) 

## difference
water1$diff <- water1$before - water1$after
sum(water1$diff)

## Checking normality
# Q-Q plot
qqnorm(water1$diff) ; qqline(water1$diff, col=2)
# Shapiro-Wilk test
shapiro.test(water1$diff) # H0 : normality

## Paired test
# Paired T-test (parametric)
t.test(water1$before, water1$after, paired=T, alternative='greater') # H1 : muA > muB
t.test(water1$before, water1$after, paired=T, alternative='less') # H1 : muA < muB
# Paired Wilcoxon signed-rank test (non-parametric)
wilcox.test(water1$before, water1$after, paired=T, alternative='greater') # H1 : muA > muB
wilcox.test(water1$before, water1$after, paired=T, alternative='less') # H1 : muA < muB

# cannot compute exact p-value with ties
wilcox.test(water1$before, water1$after, paired=T, exact=FALSE, alternative='greater') 


#------------------------------------------------------------------------------------#

### Independent sample test 

## Import csv file 
water2 <- read.csv("C:/Users/User/Desktop/ex2_Independent.csv", sep=",", header=T)

## Checking normality
# Kolmogorov-Smirnov test 
ks.test(water2$BOD[water2$group == "A"],
        water2$BOD[water2$group == "B"]) # H0 : 'A' and 'B' come from the same distribution. 
# Shapiro-Wilk test
shapiro.test(water2$BOD[water2$group == "A"]) # H0 : normality
shapiro.test(water2$BOD[water2$group == "B"]) # H0 : normality

## If normality... 
# Variance equality test (F test)
var.test(water2$BOD[water2$group == "A"],
         water2$BOD[water2$group == "B"]) # H0 : Two variances are equal. 

sum(water2$BOD[water2$group == "A"])
sum(water2$BOD[water2$group == "B"])

# Student's test (variance equal)
t.test(water2$BOD ~ water2$group, var.equal=T, paired=F, alternative='greater') 
t.test(water2$BOD ~ water2$group, var.equal=T, paired=F, alternative='less') 
t.test(water2$BOD ~ water2$group, var.equal=T, paired=F, alternative='two.sided')
# Welch's test (variance not equal)
t.test(water2$BOD ~ water2$group, var.equal=F, paired=F, alternative='greater') 
t.test(water2$BOD ~ water2$group, var.equal=F, paired=F, alternative='less') 
t.test(water2$BOD ~ water2$group, var.equal=F, paired=F, alternative='two.sided')

## If non-normality... 
# Mann-Whitney U test (= Wilcoxon rank-sum test)
wilcox.test(water2$BOD ~ water2$group, paired=F, alternative='greater')
wilcox.test(water2$BOD ~ water2$group, paired=F, alternative='less')
wilcox.test(water2$BOD ~ water2$group, paired=F, alternative='two.sided')

