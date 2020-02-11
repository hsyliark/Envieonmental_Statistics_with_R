## Making data
# 우치, 황룡강3-1, 광산, 지석천4, 나주, 무안2, 탐진강3, 주암댐, 요천-1 순으로 입력
before_BOD <- c(2.9, 4.4, 6.1, 2.2, 6.2, 1.6, 2.9, 1.0, 1.6) # 2019년 12월
after_BOD <- c(1.4, 2.8, 4.7, 2.1, 5.1, 1.6, 1.2, 0.7, 1.2) # 2020년 01월
BOD <- data.frame(before_BOD, after_BOD)
rownames(BOD) <- c('우치', '황룡강3-1', '광산', '지석천4', '나주', 
                   '무안2', '탐진강3', '주암댐', '요천-1')

# difference
BOD$diff <- BOD$before_BOD - BOD$after_BOD

# Checking normality
# Q-Q plot
qqnorm(BOD$diff); qqline(BOD$diff,col=2)
# Shapiro-Wilk test
shapiro.test(BOD$diff)

# Paired t-test
t.test(BOD$before_BOD,BOD$after_BOD,paired=T,alternative="greater")
