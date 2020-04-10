## Test with BOD
before_BOD <- c(3.8, 3.8, 3.9, 6.2, 5.4, 5.2, 4.4, 2.2) # 예년동월(2010~2019)
after_BOD <- c(2.9, 3.4, 2.9, 6.6, 6.0, 6.9, 5.0, 2.5) # 2020년 3월
BOD <- data.frame(before_BOD, after_BOD)
rownames(BOD) <- c('우치','황룡강3-1','지석천4','광산','나주',
                   '죽산','고막원천2-1','무안2')
# difference
BOD$diff <- BOD$before_BOD - BOD$after_BOD
# Checking normality
# Q-Q plot
qqnorm(BOD$diff); qqline(BOD$diff,col=2)
# Shapiro-Wilk test
shapiro.test(BOD$diff)
# Paired t-test
t.test(BOD$before_BOD,BOD$after_BOD,paired=T,alternative="less")

## Test with T-P
before_TP <- c(0.101, 0.137, 0.202, 0.260, 0.207,
               0.144, 0.114, 0.060) # 예년동월(2010~2019)
after_TP <- c(0.107, 0.077, 0.087, 0.147, 0.128,
              0.105, 0.105, 0.036) # 2020년 3월
TP <- data.frame(before_TP, after_TP)
rownames(TP) <- c('우치','황룡강3-1','지석천4','광산','나주',
                   '죽산','고막원천2-1','무안2')
# difference
TP$diff <- TP$before_TP - TP$after_TP
# Checking normality
# Q-Q plot
qqnorm(TP$diff); qqline(TP$diff,col=2)
# Shapiro-Wilk test
shapiro.test(TP$diff)
# Paired t-test
t.test(TP$before_TP,TP$after_TP,paired=T,alternative="greater")
