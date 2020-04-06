## Making data
before_BOD <- c(2.5, 5.5, 4.0, 3.3, 5.4, 3.3, 2.2) # 예년동월('10~'19)
after_BOD <- c(2.0, 6.1, 3.1, 1.7, 6.7, 5.0, 1.6) # 2020년 2월
BOD <- data.frame(before_BOD, after_BOD)
rownames(BOD) <- c('우치','광산','황룡강3-1','지석천4','죽산','고막원천2-1','무안2')

## Paired T-test (BOD)
# difference
BOD$diff <- BOD$before_BOD - BOD$after_BOD

# Checking normality
# Q-Q plot
qqnorm(BOD$diff); qqline(BOD$diff,col=2)
# Shapiro-Wilk test
shapiro.test(BOD$diff)

# Paired t-test
t.test(BOD$before_BOD,BOD$after_BOD,paired=T,alternative="greater")




## Making data
before_TP <- c(0.079, 0.259, 0.105, 0.139, 0.167, 0.086, 0.048) # 예년동월('10~'19) 
after_TP <- c(0.079, 0.190, 0.062, 0.067, 0.120, 0.098, 0.051) # 2020년 2월
TP <- data.frame(before_TP, after_TP)
rownames(TP) <- c('우치','광산','황룡강3-1','지석천4','죽산','고막원천2-1','무안2') 

## Paired T-test (TP)
# difference
TP$diff <- TP$before_TP - TP$after_TP

# Checking normality
# Q-Q plot
qqnorm(TP$diff); qqline(TP$diff,col=2)
# Shapiro-Wilk test
shapiro.test(TP$diff)

# Paired t-test
t.test(TP$before_TP,TP$after_TP,paired=T,alternative="greater")
