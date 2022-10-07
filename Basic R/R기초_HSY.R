# 1. 연산자 (Operator) ----
# 1.1 산술 연산자 ----
# +, -, *, /, **, ^, %%, %/%
# 새로운 열을 만들어 낼 때 사용함
3 + 4     # 더하기
3 - 4     # 빼기
3 * 4     # 곱하기
3 / 4     # 나누기
3 ** 4    # 거듭제곱
3 ^ 4     # 거듭제곱
13 %% 4   # 나머지
13 %/% 4  # 몫


# 1.2 할당 연산자 ----
#  <-, ->, =
#  저장하는 기능
# <-, -> : 일반적인 저장 기능
# =      : 함수 안에 있는 argument를 저장하는 기능
x <- rnorm(n = 100, mean = 100, sd = 10)
density(x)
plot(density(x))


# 1.3 비교 연산자 ----
# >, >=, <, <=, ==< !=, !
# 조건에 맞는(행 기준) 데이터를 잘라내기 
3 > 4     # greater than
3 >= 4    # greater than equal
3 < 4     # less than
3 <= 4    # less than equal
3 == 4    # equal to
3 != 4    # not equal
!(3 == 4) # not


# 1.4 논리 연산자 ----
# &, |
#  두 가지 이상의 조건을 줄 때 사용함
# & : and 기능 : 2개 이상의 조건을 모두 만족하는 데이터를 잘라내기
# | : or 기능 : 2개 이상의 조건 중 하나 이상 만족하는 데이터를 잘라내기
(3 > 4) & (5 > 4)
(3 > 4) | (5 > 4)


# 2. 데이터의 유형 ----
# 2.1 수치형(Numeric) ----
# (1) 정수형(integer)
# (2) 실수형(double)
x1 <- 10
# x1 : data
# x1의 데이터 유형 : numeric : integer

x2 <- 10.3
# x2 : data
# x1의 데이터 유형 : numeric : double

# 2.2 문자형 ----
# Character
x3 <- '문제가 문제가 아니라 문제를 대하는 태도가 문제다.'
# x3 : data
# x3의 데이터 유형 : character

x4 <- "인생은 고통의 바다이다."
# x4 : data
# x4의 데이터 유형 : character


# 2.3 논리형 ----
# Logical
x5 <- TRUE
# x5 : data
# x5의 데이터 유형 : logical

x6 <- FALSE
# x6 : data
# x6의 데이터 유형 : logical


# 3. 데이터 ----
# 3.1 vector ----
# 하나의 열로 구성되어 있음. 1차원 구조
# 데이터 분석의 가장 기본 단위
# 하나의 데이터 유형으로만 이루어짐

# (1) vector 만들기
# i. c(값1, 값2, ...)
#  c : concatenate or combine
# numeric, character, logical vector
# 값들 간의 규칙이 없을 때에 주로 사용함
# 집단으로 되어 있지 않음
sight <- c(1.2, 0.9, 1.0, 1.5)
# sight : data : vector : numeric : 4 elements

bt <- c("O", "A", "A", "A")
# bt : data : vector : character : 4 elements

smoke <- c(TRUE, FALSE, FALSE, TRUE)
# smoke : data : vector : logical : 4 elements


# ii. :
# numeric vector만 만들 수 있음
# 1씩 증가되거나 1씩 감소되는 숫자들로 이루어진 vector를 만들 때 사용
# start:end

id <- 1:100

id2 <- 100:1


# iii. seq(from = , to = , by = )
# numeric vector만 만들 수 있음
# 모든 증가 또는 감소를 표현
# seq : sequence (순차적)
select_id <- seq(from = 1, to = 100, by = 5)
select_id

select_id2 <- seq(from = 100, to = 1, by = -5)
select_id2


# (2) vector의 slicing
# vector[index]
money <- c(30, 50, 40, 50, 50, 50, 30)
money[1]

# i. 1, 2, 7번째의 값을 한 번에 잘라내기
money[c(1, 2, 7)]

# ii. 2, 3, 4, 5, 6번째의 값을 한 번에 잘라내기
money[2:6]

#  iii. 1, 3, 5, 7번째의 값을 한 번에 잘라내기
money[seq(1, 7, 2)]

# iv. 조건을 만족하는 값을 잘라내기
money[money == 50]


# (3) vector의 연산
v1 <- c(10, 20, 30)
v2 <- c(40, 50, 60)
v3 <- v1 + v2
v3

v4 <- c(100, 200, 300, 400, 500, 600)
v5 <- v1 + v4
v5

v6 <- c(100, 200, 300, 400)
v7 <- v1 + v6
v7


# 3.2 factor ----
# 하나의 열로 구성되어 있음. 1차원 구조
# 데이터 분석의 가장 기본단위
# 하나의 데이터 유형으로만 이루어짐.

# (1) factor 만들기
# factor(vector,
#        labels = 집단의 이름,
#        levels = 집단의 순서,
#        ordered = 집단의 순서)

bt <- c("ab", "o", "ab", "a", "o", "o", "o")
bt_factor <- factor(bt)
bt_factor
table(bt_factor)

bt_factor2 <- factor(bt,
                     labels = c("A", "AB", "O"))
bt_factor2
table(bt_factor2)

bt_factor3 <- factor(bt,
                     levels = c("a", "o", "ab"))
bt_factor3
table(bt_factor3)

bt_factor4 <- factor(bt,
                     levels = c("a", "o", "ab"),
                     labels = c("A", "O", "AB"))
bt_factor4
table(bt_factor4)

bt_factor5 <- factor(bt,
                     levels = c("a", "o", "ab"),
                     labels = c("A", "O", "AB"),
                     ordered = TRUE)
bt_factor5
table(bt_factor5)


# 3.3 data.frame ----
# 행과 열로 구성되어 있음. 2차원 구조.
# (1) data.frame 만들기
# data.frame(vector or factor)
id     <- 1:4
hobby  <- c("게임", "TV시청", "헬스", "넷플릭스")
car    <- c(1, 1, 2, 1)
type   <- c("그랜저", "프라이드", "아반떼", "K8")
type   <- factor(type)
survey <- data.frame(id, hobby, car, type)
survey

# 3.4 list ----
# 가장 유연한 형태의 데이터
# 대부분의 데이터 분석 결과는 list 형태로 구성되어 있음
# 1차원 구조

# (1) list 만들기
result <- list(id, type, survey)
result

# (2) list의 slicing
result[1] # list
result[[1]] # vector

result[2] # list
result[[2]] # factor

result[3] # list
result[[3]] # data.frame


# 4. 데이터 읽어오기 ----
# 4.1 txt ----
# data <- read.table(file = "directory/filename.txt",
#                    sep = " " or "," or "\t",
#                    header = TRUE)
favor <- read.table(file     = "D:/favor.txt",
                    sep      = " ",
                    header   = TRUE,
                    encoding = "EUC-KR")
favor

# 4.2 csv ----
# csv : comma separated value
# data <- read.csv(file = "directory/filename.csv",
#                  header = TRUE)
hope <- read.csv(file   = "D:/hope.csv",
                 sep    = ",",
                 header = TRUE)


# 4.3 excel : xls, xlsx ----
# R의 기본 기능에서는 읽어올 수 없음
# R의 새로운 기능을 추가하고 읽어와야 함
# package

# (1) 패키지 설치하기 : 하드(HDD)
install.packages("readxl")

# (2) 패키지 로딩하기 : 메모리(RAM)
# Rstudio가 실행되는 동안에는 한번만
library(readxl)

# data <- readxl::read_excel(path      = "directory/filename.xlsx",
#                            sheet     = "sheet_name" or sheet_index
#                            col_names = TRUE)

travel01 <- readxl::read_excel(path      = "d:/travel.xlsx",
                               sheet     = "data",
                               col_names = TRUE)
travel01

travel02 <- readxl::read_excel(path      = "d:/travel.xlsx",
                               sheet     = 1,
                               col_names = TRUE)
travel02


# 작업공간(Working Directory)
# (1) 현재 설정된 작업공간 알아내기 : getwd()
getwd()

# (2) 새로운 작업공간 설정하기 : setwd("directory")
setwd("d:/")

travel03 <- readxl::read_excel(path      = "travel.xlsx",
                               sheet     = 1,
                               col_names = TRUE)
travel03


# 5. 데이터 저장하기 ----
# 5.1 txt ----
# write.table(data,
#             file = "directory/filename.txt",
#             sep = " " or "," or "\t",
#             row.names = FALSE)
write.table(travel01,
            file      = "travel_2022_1006_1439.txt",
            sep       = ",",
            row.names = FALSE)


# 5.2 csv ----
# write.csv(data,
#           file = "directory/filename.txt",
#           row.names = FALSE)
write.csv(travel01,
          file      = "travel_2022_1006_1511.csv",
          row.names = FALSE,
          fileEncoding  = "EUC-KR")


# 5.3 excel : xls, xlsx ----
# package : writexl
install.packages("writexl")
library(writexl)

# writexl::write_xlsx(data,
#                     path = "directory/filename.txt")
writexl::write_xlsx(travel01,
                    path = "travel_2022_1006_1523.xlsx")


# 6. RData로 저장하기 ----
# 메모리(RAM)에 있는 R Data를 하드(HDD)에 R Data로 저장하기
# save(data,
#      file = "directory/filename.RData")
save(travel01,
     file = "travel_2022_1006_1537.RData")


# 7. RData 읽어오기 ----
# 하드(HDD)에 있는 RData를 메모리(RAM)에 올리는 기능
# load(file = "directory/filename.RData")
load(file = "travel_2022_1006_1537.RData")
travel01








