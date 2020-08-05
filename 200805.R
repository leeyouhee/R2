# 1. 탐색적 데이터 조회

# 1.1 데이터셋 보기

# [실습] 데이터 가져오기

setwd('C:/Users/09/Desktop/R/R_study/data')
dataset <- read.csv('./data/dataset.csv',
                    header=T, stringsAsFactors=T)

View(dataset)

table(is.na(dataset$resident)) #na 확인

summary(dataset)

# 1. 2 dataset 구조 보기
names(dataset)
attributes(dataset)
str(dataset)

#
dataset$age
dataset$resident
dataset$gender
length(dataset$age)

# 조회 결과 변수 저장
x <- dataset$gender
y <- dataset$price

#산점도 형태로 변수 조회
plot(x, y) #성별과 가격 분포

# [실습] ["칼럼명"] 형식으로 특정 변수 조회
dataset["gender"] #dataset$gender, dataset[,"gender"]
dataset["price"]

# 실습 [index] 형식으로 변수 조회
dataset[2]
dataset[6]
dataset[3,]
dataset[,3]

# 실습 색인 형식ㄱ으로 변수 조회
dataset[c("job", "price")]
dataset[c(2,6)]

dataset[c(1,2,3)]
dataset[c(1:3)]
dataset[c(2,4:6, 3, 1)]
dataset[-c(2)] #2빼고 나머지 칼럼들

# 2. 결측치 처리
# null <- 값이 아직 정해지지 않음, 임시의 값 // NA <- missing value - 값이 있어야하는데 빠진것
summary(dataset$price)
sum(dataset$price) #NA가 있으면 sum이 안됨

# 2.2 결측치 제거
sum(dataset$price, na.rm=T) #na.rm 결측치 제거



a <- seq(1:100)
a[1] <- NA
a
mean(a, na.rm=T)

dim(dataset)

dataset4 <- na.omit(dataset) #dataset에서 na가 없는 행만 추출출
dim(dataset4)

price2 <- na.omit(dataset$price)
plot(dataset$price)
sum(price2)
length(price2)

#2.3 결측치 대체
#실습 결측을 0으로 대체하기
x <- dataset$price #price vector 생성
x[1:30]
dataset$price2 <- ifelse(!is.na(x),x,0)#0으로 대체
dataset$price2[1:30]

#실습 결측데이터 처리(평균으로 대체)
x <- dataset$price 
x[1:30]
dataset$price3= ifelse(!is.na(x),x,round(mean(x,na.rm = T),2)) #평균으로 대체

dataset$price3[1:30]
a <- dataset[c('price','price2','price3')]
a
subset(a, is.na(price)) #특정조건을 만족하는 행을 추출;a에 대해서 price가 na인 것을 추출


#[실습] 범주형 변수의 이상치 확인
table(dataset$gender)
pie(table(dataset$gender)) #파이차트

#[실습] subset() 함수를 이용한 데이터 정제하기

dataset <- subset(dataset, gender ==1 | gender == 2)
#length(dataset#gender) gender변수 데이터 정제
length(dataset$gender) #297개 - 3개 정제됨
pie(table(dataset$gender))
pie(table(dataset$gender), col = c('red', 'blue'))

######################################################
dset <- subset(dataset, price >= 2 & price <= 8)
length(dset$price)
dset$price
plot(dset$price)

#3.2 연속형 변수의 극단치 처리
dataset$price
length(dataset$price)
summary(dataset$price)

#price 변수의 데이터 정제와 시각화
dset <- subset(dataset, price >= 2 & price <= 8)
length(dset$price)
dset$price
stem(dset$price)

#실습 age 변수에서 NA발견
summary(dset$age)
length(dset$age)

#subset 이용
dset1 <- subset(dset, age >= 20 & age <=69)
#na.omit 이용
data_na <- na.omit(dset$age)
length(data_na)

#boxplot(dset1$age)
dset1 <- subset(dset, age >= 20 & age <=69)
length(dset1$age)
dse1t$age
boxplot(dset1$age)

##아래 데이터 프레임의 test 변수의 NA를 평균으로 대체해 보세요.
tt <- data.frame(test=c(1,3,8,4,3,4,NA,9))
tt$test1 <- ifelse(is.na(tt$test), round(mean(tt$test,na.rm = T),2),tt$test)
mean(tt$test, na.rm = T)

#실습
#dataset에 resident 변수의 1 --> 1.서울특별시,
2 --> 2.인천광역시, 3 --> 3.대전광역시, 4 --> 4.대구광역시
5 --> 5.시구군으로 표기하는 resident2 변수를 생성하세요

dset$resident2[dset$resident == 1] <-'1.서울특별시'
dset$resident2[dset$resident == 2] <-'2.인천광역시'
dset$resident2[dset$resident == 3] <-'3.대전광역시'
dset$resident2[dset$resident == 4] <-'4.대구광역시'
dset$resident2[dset$resident == 5] <-'5.시구군'
dset

# a <- dataset2$resident
# aa <- ifelse(is.na(a),"fail",
#              ifelse(a==1,"서울특별시",
#                     ifelse(a==2,"인천광역시",
#                            ifelse(a==3,"대전광역시",
#                                   ifelse(a==4,"대구광역시","시구군")))))
# aa
# dataset2$resident2 <- aa
# dataset2

#dplyr
dataset2 %>%  select(resident, resident2)

[실습] job 칼럼을 대상으로 코딩 변경하기
dset$job2[is.na(dset$job)] <- 'Fail'
dset$job2[dset$job ==1] <- '공무원'
dset$job2[dset$job == 2] <- '회사원'
dset$job2[dset$job == 3] <- '개인사업'
dset

# 4.2 척도 변경을 위한 코딩 변경
#[실습] 나이 변수를 청년층, 중년층, 장년층으로 구분
dset$age2[is.na(dset$age)] <- 'Fail'
dset$age2[dset$age <= 30] <-'청년층'
dset$age2[dset$age > 30] <-'중년층'
dset$age2[dset$age > 55] <-'장년층'
dset

dset$survey2[dset$survey == 5] <- 1
dset$survey2[dset$survey == 4] <- 2
dset$survey2[dset$survey == 1] <- 5
dset$survey2[dset$survey == 2] <- 4

####05-1%%%%%
exam <- read.csv('./data/csv_exam.csv')
exam
dim(exam)
str(exam)
summary(exam)

install.packages('ggplot2')
library(ggplot2)
str(ggplot2::mpg)
mpg <- as.data.frame(ggplot2::mpg) #구조만 데이터프레임으로 변경

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

install.packages('dplyr')
library(dplyr)

df_new <- df_raw
df_new
###############################################################1번 함수 rename
df_new <- rename(df_new, v2 = var2) #컬럼 이름 변경
df_new

df_new %>%  rename(v1 = var1) #
df_new

df <- data.frame(var1 = c(2,4,8),
                 var2 = c(2,6,1))
df
df$var_sum <- df$var1 + df$var2
df

############
mpg$total <- (mpg$cty + mpg$hwy)/2 #통합 연비 변수 생성
head(mpg)
mean(mpg$total)


############
summary(mpg$total)
hist(mpg$total) #히스토그램 생성

#############20이상이면 pass, 아니면 fail
mpg$test <- ifelse(mpg$total >= 20, 'PASS', 'FAIL')
head(mpg,20)
table(mpg$test)

qplot(mpg$test) #barplot은 r 내장 함수이고 qplot은 ggplot2의 내장 함수이다.

##total을 기준으로 a,b,c, 등급 부여
mpg$grade <- ifelse(mpg$total >= 30 ,'A',
                    ifelse(mpg$total >= 20, "B",'C'))
head(mpg,3)

mpg %>%  head(2)
table(mpg$grade)
qplot(mpg$grade)

# 3. 변수명 추정
mpg <- rename(mpg, company = manufacturer)
mpg

subset(exam,class ==1)

#exam에서 class가 1인 경우만 추출하여 출력
###############################################################2번 함수 filter
exam %>%  filter(class ==1) #subset과 동일
filter(exam, class == 1)

exam %>%  filter(class ==2)

exam %>%  filter(class !=1)

exam %>%  filter(class !=3)

exam %>%  filter(class == 1 & math>=50)

exam %>%  filter(class == 2 & english >= 80)

exam %>%  filter(math >= 90 | english >= 90)

exam %>%  filter(english < 90 | science  < 50)
filter(exam, english < 90 | science  < 50)

aa <- c(1,3,5,2,9)
sort(aa,decreasing = T)

###############################################################3번 함수 select
 + 컬럼을 추출하는 함수

exam %>% select(math)
exam %>% select(english)
exam %>% select(math,class)
exam %>% select(-math) #math빼고 추출출

exam %>% 
  filter(class==1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head

###############################################################4번 함수 arrange
exam %>%  arrange(math)#math를 오름차순 정렬
exam %>%  arrange(desc(math))
exam %>%  arrange(class, math)

###############################################################5번 함수 mutate
exam %>% 
  mutate(total = math + english + science) %>%  #total 변수 추가
  head

exam %>% 
  mutate(total = math + english + science,
         mean = (math+english+science)/3) %>%  #total 과 mean 변수 추가
  head

###############################################################6번 함수 summarise
exam %>% summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math), #중앙값
            n = n()) #학생수

mpg %>% 
  group_by(company, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head

mpg %>% 
  group_by(company) %>% 
  filter(class == 'suv') %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>%
  arrange(desc(mean_tot)) %>% 
  head
              
###############################################################7번 함수 join
test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
test2 <- data.frame(id = c(1,2,3,4,5,6),
                    final =  c(7,87,90,12,43,56))
test1
test2

total <- left_join(test1,test2,by = 'id')
total

total1 <- full_join(test1,test2,by = 'id')
total1

#############################연습문제#######################################
데이터 설명

ratings 데이터 : user id, movie id, rating, time stamp

movies 데이터 : movie id, title, genres

user 데이터 : user id, gender, age, occupation, zip code


1.데이터 조사, 탐색적 분석, 전처리 등

#movie
movie_vec <- readLines(con = './data/movies.dat',encoding = 'UTF-8')
movie_f <- gsub('::',';',movie_vec )
str(movie_f)


movie <- read.csv(text=movie_f, header = F, stringsAsFactors = F, sep =';')
View(movie)
        
movie <- rename(movie, movie_id = V1)
movie <- rename(movie, title = V2)
movie <- rename(movie, genres = V3)
head(movie)


#raing
rating_vec <- readLines(con = './data/ratings.dat',encoding = 'UTF-8')
rating_f <- gsub('::',';',rating_vec )
str(movie_f)

rating <- read.csv(text=rating_f, header = F, stringsAsFactors = F, sep =';')
View(rating)

rating <- rename(rating, user_id = V1)
rating <- rename(rating, movie_id = V2)
rating <- rename(rating, rating = V3)
rating <- rename(rating, time_stamp = V4)
head(rating,3)

#user
user_vec <- readLines(con = './data/users.dat',encoding = 'UTF-8')
user_f <- gsub('::',';',user_vec )
str(user_f)

user <- read.csv(text=user_f, header = F, stringsAsFactors = F, sep =';')
View(user)

user <- rename(user, user_id = V1)
user <- rename(user, gender = V2)
user <- rename(user, age = V3)
user <- rename(user, occupation = V4)
user <- rename(user, zip_code = V5)
head(user,3)

2-1) 성별에 따른 평균 평점은?
View(user) 
View(rating)  

#user 테이블에서 성별과 id 뽑기
user1 <- user %>%  select(user_id, gender)
#rating 테이블에서 user_id와 평점 그리고 movie_id 뽑기
rating1 <- rating %>%  select(user_id, rating, movie_id)
#movie 테이블에서 movie_id와 title 뽑기
movie1 <- movie %>%  select(movie_id, title)
#user 테이블과 rating 테이블 합치기
user_rating <- left_join(rating1,user1,id ='user_id')  
table(is.na(user_rating))
head(user_rating,3)  
#movie 테이블과  user_rating 테이블 합치기
movie1$movie_id <- as.integer(movie$movie_id) #movie테이블의 movieid type 변경
movie_user_rating <- left_join(user_rating,movie1,id ='movie_id')
table(is.na(movie_user_rating))
head(movie1)

movie_user_rating_tr <- movie_user_rating %>% 
                          group_by(gender) %>% 
                          summarise(mean_rating = mean(rating))
movie_user_rating_tr

2-2) 여성에게 인기 있는 top20 영화는?(단, 평가자 100명 이상)
female_best <- movie_user_rating %>%
  filter(gender == 'F') %>% 
  group_by(title,movie_id) %>% 
  summarise(mean_mvf = mean(rating), num = n()) %>%
  filter(num >=100) %>%
  arrange(desc(mean_mvf)) %>% 
  head(20)
female_best









                  