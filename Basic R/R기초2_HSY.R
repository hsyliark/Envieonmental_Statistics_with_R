# 패키지 설치하기와 로딩하기 ----
install.packages("readxl")
install.packages("writexl")
install.packages("tidyverse")
install.packages("ggplot2")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)


# 작업공간 ----
setwd("d:/")
getwd()


# 데이터 읽어오기 ----
# 예제 데이터 : ggplot2::diamonds


# 1. 데이터 보기 ----
# (1) data
# console에 데이터 출력
# 데이터가 크면 일부만 console에 출력
diamonds


# (2) head(data, n = 6)
head(diamonds)
head(diamonds, n = 3)


# (3) tail(data, n = 6)
tail(diamonds)
tail(diamonds, n = 3)


# (4) View(data)
View(diamonds)


# 2. 데이터의 중요한 속성 ----
# (1) 행의 개수
# nrow(data)
nrow(diamonds)


# (2) 열의 개수
# ncol(data)
ncol(diamonds)


# (3) 열의 이름
# colnames(data)
colnames(diamonds)
colnames(diamonds)[3]


# 3. slicing ----

# (1) 열
# dplyr::select()
diamonds %>%
  dplyr::select(carat, price) -> d2


# cut ~ x : 연달아 있음
diamonds %>% 
  dplyr::select(cut:x) 

# price 열만 제거하기
diamonds %>% 
  dplyr::select(-price)

# carat, price, x 열을 제거
diamonds %>% 
  dplyr::select(-c(carat, price, x))

# 열의 이름 중에 특정한 패턴이 있는 경우
# i. 열의 이름 중에 특정한 문자가 있는 경우
diamonds %>%
  dplyr::select(contains("c"))

# ii. 열의 이름 중에 특정한 문자로 시작하는 경우
diamonds %>%
  dplyr::select(starts_with("c"))

# iii. 열의 이름 중에 특정한 문자로 끝나는 경우
diamonds %>%
  dplyr::select(ends_with("e"))


# (2) 행
# dplyr::filter(조건)
# i. carat이 2 이상인 데이터
diamonds %>% 
  dplyr::filter(carat >= 2)

# ii. cut이 "Fair"인 데이터
diamonds %>% 
  dplyr::filter(cut == "Fair")

# iii. carat은 2 이상이고 cut은 "Fair"인 데이터
diamonds %>% 
  dplyr::filter(carat >= 2, cut == "Fair")
diamonds %>% 
  dplyr::filter(carat >= 2 & cut == "Fair")

# iv. carat은 2 이상 또는 cut은 "Fair"인 데이터
diamonds %>% 
  dplyr::filter(carat >= 2 | cut == "Fair")















