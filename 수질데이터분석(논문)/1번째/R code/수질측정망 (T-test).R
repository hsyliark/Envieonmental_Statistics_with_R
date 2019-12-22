## Making data
# 우치, 황룡강3-1, 광산, 지석천4, 나주, 무안2, 탐진강3, 주암댐, 요천-1 순으로 입력
before_BOD <- c(1.3, 1.8, 3.9, 1.0, 4.0, 1.3, 1.2, 1.2, 0.7) # 2019년 10월
after_BOD <- c(3.4, 3.7, 5.0, 1.6, 4.8, 0.7, 1.7, 0.8, 0.9) # 2019년 11월
BOD <- data.frame(before_BOD,after_BOD)
rownames(BOD) <- c('우치', '황룡강3-1', '광산', '지석천4', '나주', 
                   '무안2', '탐진강3', '주암댐', '요천-1')

# difference
BOD$diff <- BOD$before_BOD - BOD$after_BOD

# Checking normality
# Q-Q plot
qqnorm(BOD$diff); qqline(BOD$diff,col=2)
# Shapiro-Wilk test
shapiro.test(BOD$diff)

#paired t-test
t.test(BOD$before_BOD,BOD$after_BOD,paired=T,alternative="less")
