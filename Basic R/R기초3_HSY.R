# 패키지 설치하기와 로딩하기 ----
install.packages("readxl")
install.packages("writexl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gmodels")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(gmodels)


# 작업공간 ----
setwd("d:/")
getwd()


# 데이터 읽어오기 ----
# 예제 데이터 : ggplot2::diamonds
diamonds


# 자료의 종류 ----
# (1) 범주형 자료(categorical data) = 질적 자료 : 문자, 숫자(숫자의 의미가 없음)
# (2) 수치형 자료(numerical data) = 양적 자료 : 숫자(숫자의 의미가 있음)


# 탐색적 데이터 분석(EDA : Exploratory Data Analysis)
# 표, 그래프, 기술통계량


# 참고 : 데이터의 품질
# (1) 입력오류               : 수정, 삭제
# (2) 결측치(missing value)  : 삭제, 대체(imputation)
# (3) 이상치(outlier)        : 그냥 쓴다, 삭제, 변환, 대체
# (4) 데이터 결합


# 1. 범주형 자료의 분석 ----
# diamonds : cut, color, clarity

# (1) 표 = 빈도표(Frequency Table)
# i.  빈도
# ii. 백분율(Percent)
diamonds %>% 
  dplyr::count(cut, sort = TRUE) %>% 
  dplyr::mutate(percent = round((n/sum(n))*100, digits = 1)) %>% 
  writexl::write_xlsx(path = "cut.xlsx")

# (2) 데이터 시각화(Data Visualization)
# i.  막대 그래프(Bar Plot)
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x=cut)) +
  ggplot2::geom_bar(fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Quality of Diamonds",
                x     = "Cut",
                y     = "Frequency") +
  ggplot2::theme(plot.title = element_text(size = 20, 
                                           face = "bold", 
                                           color = "purple",
                                           hjust = 0.5),
                 axis.title.x = element_text(size = 15,
                                             face = "italic"),
                 axis.title.y = element_text(size = 15,
                                             face = "bold.italic",
                                             angle = 0,
                                             vjust = 0.5)) +
  ggplot2::geom_text(stat = "count", aes(x = cut, label = ..count..),
                     vjust = -0.2) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::ggsave(filename = "cut.jpeg",
                  width    = 5,
                  height   = 5,
                  units    = "in")


# ii. 원 그래프(Pie Chart) : 비추천


# 2. 수치형 자료 분석 ----
# carat, table, depth, price, x, y, z
# (1) 빈도표
# i.  구간의 빈도
# ii. 구간의 백분율
# carat -> carat_group
diamonds %>% 
  dplyr::mutate(carat_group = cut(carat,
                                  breaks = seq(from = 0, to = 6, by = 2),
                                  right  = FALSE,
                                  labels = c("Light", "Medium", "Heavy"))) -> diamonds
View(diamonds)


diamonds %>% 
  dplyr::count(carat_group, sort = TRUE) %>% 
  dplyr::mutate(percent = (n/sum(n))*100)


# (2) 데이터 시각화
# i.   히스토그램(Histogram)
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = carat)) +
  ggplot2::geom_histogram()

# 구간의 너비 : binwidth
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = carat)) +
  ggplot2::geom_histogram(binwidth = 0.5)

# 구간의 개수 : bins
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = carat)) +
  ggplot2::geom_histogram(bins = 20, fill = "red")


# ii.  상자그림(Boxplot)
# 이상치(outlier) 유무 파악
diamonds %>% 
  ggplot2::ggplot(mapping = aes(y = carat)) +
  ggplot2::geom_boxplot(outlier.color = "red")


# iii. 바이올린(Violin)
# diamonds %>% 
#  ggplot2::ggplot(mapping = aes(x = carat)) +
#  ggplot2::geom_violin()


# (3) 기술(요약)통계량
# i. 중심 = 대표값 : 평균, 절사평균, 중위수, 최빈수
diamonds %>% 
  dplyr::summarise(Mean        = mean(carat),
                   TrimmedMean = mean(carat, trim = 0.05),
                   Median      = median(carat))

# 결측치(Missing Value) : NA(Not Available)
age <- c(10, 20, NA, 30)
mean(age)
mean(age, na.rm = TRUE)

# ii. 다름 = 산포 = 퍼짐
# 범위, 사분위범위(IQR), 표준편차, 중위수절대편차
diamonds %>% 
  dplyr::summarise(Range = max(carat) - min(carat),
                   IQR   = IQR(carat),
                   SD    = sd(carat),
                   MAD   = mad(carat))


# 3. 범주형 자료(2개의 열) ----
# (1) 표 = 교차표(Cross Table) = 분할표(Contingency Table)
# 빈도, 전체 백분율, 행 백분율, 열 백분율
# gmodels::CrossTable(x = data$범주형자료,
#                   y = data$범주형자료)
gmodels::CrossTable(x = diamonds$cut,
                    y = diamonds$color)


# (2) 데이터 시각화 : 누적 막대그래프
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = cut, fill = color)) +
  ggplot2:: geom_bar(position = "fill")


# 4. 범주별 수치형 자료의 분석 ----
# 범주형 자료 : cut
# 수치형 자료 : carat

# (1) 범주별 데이터 시각화
# i. 범주별 히스토그램
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = carat)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(~cut)

# ii. 범주별 상자그램
diamonds %>% 
  ggplot2::ggplot(mapping = aes(y = carat)) +
  ggplot2::geom_boxplot(outlier.color = "green") +
  ggplot2::facet_wrap(~cut)

diamonds %>% 
  ggplot2::ggplot(mapping = aes(y = carat, fill = cut)) +
  ggplot2::geom_boxplot(outlier.color = "red")

# iii. 범주별 바이올린
diamonds %>% 
  ggplot2::ggplot(mapping = aes(y = carat, x = cut, fill = cut)) +
  ggplot2::geom_violin()


# (2) 범주별 기술통계량
diamonds %>% 
  dplyr::group_by(cut) %>% 
  dplyr::summarise(n      = n(), 
                   Median = median(carat),
                   MAD    = mad(carat)) %>% 
  dplyr::arrange(Median)

























































