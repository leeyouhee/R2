#KoNLP 형태소 분석하는 패키지

install.packages('multilinguer')
install.packages(c('stringer', 'hash', 'tau',
                   'Sejong', 'RSQLite','devtools'), type = 'binary')
install.packages('remotes')
remotes::install_github('haven-jeon/KoNLP', upgrade='never', 
                        INSTALL_opts = c('--no-multiarch'))
library(multilinguer)
library(KoNLP)
library(rJava)
library(stringr)
#출력함수
i <- 40
sprintf('나는 %i살이다.', i) 

print_test <- c(1,5,7)
print_test1 <- c(2,4,6)
print(print_test)
print(print_test,print_test1)

cat(print_test,print_test1)
cat('첫번째는 홀수 :',print_test,'두번째는 짝수 =',print_test1)

#1-1. if()함수
x <- 10
y <- 5
z <- x*y

if(x*y > 40){
  cat('결과는 40이상 \n')
  cat('x*y = ', z,'\n')
  print(z)
}

#학점 구하기
score <- scan()
result <- 'low'

if(score >= 80){
  result <- '완벽'
  print(result)
}
result
cat('your soc is', score,'which is', result)

score <- 90
if(score >= 90){
  result = 'A학점'
}else if(score >= 80){
  result = 'B학점'
}else if(score >= 70){
  result = 'C학점'
}else if(score >= 60){
  result = 'D학점'
}else{
  result = 'F학점'
}
cat('당신의 학점은', result)
print(result)

score <- 70
ifelse(score >= 80,'좋아','별로')

#1-3 which()
subset()
벡터 객체를 대상으로 특정 데이터를 검색하는 사용
조건식을 만족하는 경우 해당 벡터 원소의 인덱스를 반환
데이터 프레임에서 사용

no <- c(1:3)
name <- c('홍','이','김')
score <- c(22,33,44)
exam <- data.frame(학번 = no,
                     이름 = name,
                     점수 = score)
exam$학점 <- c('A','B','C')

exam

#인덱스 3의 전체 데이터를 보고싶다.
exam[3,]
#Boolean indexing
exam$이름 == '이'
exam[exam$이름 == '김',]
#이름이 홍인 사람의 인덱스 및 학번과 점수는?
exam[which(exam$이름 =='이'),]
#조건을 만족하는 이름이 홍인 사람의 인덱스를 알고싶다.
which(exam$이름 == '홍')
#subset(df, clol == '')
subset(exam, 이름 == '홍')
#반복문
for(변수 in 범위){}
i <- c(1:10)
i
d <- c()
for (n in i){
  print(n+10)
  print(n)
  d[n] <- n+1
}
d

#1~30까지 짝수값만 출력하는 반복문
i <- c(1:30)
length(i)
for(n in i){
  if(n%%2 == 0){
    print(n)
  }
}

for(n in 1: length(i)){
  if(n%%2 == 0){
    print(n)
  }
}

#실습
i <- 1:20
i
for(n in 1:length(i)){
  ifelse(n%%2 == 0 , next, print(n))
}

#실습
name <- c('이유희','정두영','김민수')
score <- c(85,95,98)

i <- 1
for(s in score){
  cat(name[i],'->',s,'\n')
}

for(i in 1:length(score)){
  cat(name[i], '->', score[i],'\n')
}

#while(조건문){실행문}
i <- 1
while(i<5){
  print(i)
  i <- i+1
}

i <- 10
while(i>0){
  cat('남은 커피는', i, '잔입니다.','\n')
  i <- i-1
  if(i==0){
    cat('커피가 없습니다.\n','판매를 종료합니다')
  }
}

j <- 10
while(j>=0){
  if(j>0){
    cat('남은 커피는', j, '잔입니다.','\n')
  }else {
    cat('커피가 없습니다.\n')
    cat('판매를 종료합니다.')
  }
  j <- j-1
}

#사용자 정의 함수

f <- function(x,y){
  adding = x+y
  return(adding)
}
f(1,2)

#실습 구구단 출력함수
gugudan <- function(i){
  for(x in 2:i){
    cat('**',x,'단','**\n')
    for( y in 1:9){
      cat(x,'*',y,'=',x*y,'\n')
    }
  }
}
gugudan(4)
#명사추출
seoul_data <- readLines(con = './data/seoul.txt',encoding = 'UTF-8')
head(seoul_data)

sdata <- sapply(seoul_data,KoNLP::extractNoun,USE.NAMES = F)
head(sdata)
#단어로 분리
sdata2 <- unlist(sdata)
head(sdata2)

sdata3 <- gsub('서울시','',sdata2) #서울시 라는 단어를 공백처리하겠다.
sdata3 <- gsub('서울','',sdata3)
sdata3 <- gsub('[0-9]+','',sdata3) # 개수 상관없이 모든 숫자
sdata3 <- gsub('박원순','',sdata3)
sdata3 <- gsub('-','',sdata3)
sdata3 <- gsub('?','',sdata3)
sdata3 <- gsub(' ','',sdata3)
sdata3 <- gsub('\\.','',sdata3) # 마침점 제거
sdata3
head(sdata3,8)

##8월4일 복습
library(multilinguer)
library(RColorBrewer)
library(wordcloud)

jeju <- readLines('../data/jeju.txt')
#명사 추출
jdata <- sapply(jeju,KoNLP::extractNoun,
                USE.NAMES = F)
head(jdata,3)

#단어 분리
jnon <- unlist(jdata)
head(jnon)

#불용어 제거
jdata3 <- gsub('제주','',jnon)
jdata3 <-gsub('제주도','',jdata3)
jdata3 <-gsub('[0-9]+','',jdata3)
jdata3 <-gsub('오전','',jdata3)
jdata3 <-gsub('오후','',jdata3)
jdata3 <-gsub('/','',jdata3)
jdata3 <-gsub('\\.','',jdata3)
jdata3 <-gsub('-','',jdata3)
jdata3 <-gsub('?','',jdata3)
jdata3

jdata3 <- gsub('추천','',jdata3)
jdata3 <- gsub('흑돼지','',jdata3)
jdata3 <- gsub('가게','',jdata3)
jdata3 <- gsub('쪽','',jdata3)
jdata3 <- gsub('^ㅇ','',jdata3)
jdata3 <- gsub('것','',jdata3)
jdata3 <- Filter(function(x){nchar(x)>=2},jdata3)
jdata3 <-gsub('숙소','',jdata3)
jdata3 <-gsub('시간','',jdata3)
jdata3 <-gsub('여행','',jdata3)
jdata3 <-gsub('해안','',jdata3)
jdata3 <-gsub('코스','',jdata3)
jdata3 <-gsub('까지','',jdata3)
jdata3 <-gsub('드라이브','',jdata3)
jdata3 <-gsub('출발','',jdata3)
jdata3 <-gsub('예약','',jdata3)
jdata3 <-gsub('경유','',jdata3)
jdata3 <-gsub('관광지','',jdata3)
jdata3 <-gsub('일정','',jdata3)
jdata3 <-gsub('하게','',jdata3)
jdata3 <-gsub('도착','',jdata3)

#연습문제
1. 다음 조건에 맞게 client 데이터프레임을 생성하고 데이터를 처리해보세요

+ name : '유관순','홍길동','이순신','신사임당'

+ gender : 'F','M','M','F'

+ price : 50,65,45,75

+ 조건1 : 3개의 벡터 객체를 이용하여 client 데이터 프레임 생성

+ 조건2 : price 변수의 값이 65만원 이상이면, 문자열 'Best', 
          65만원 미만이면 'Normal'을 변수 result 컬럼에 추가

+ result 변수를 대상으로 빈도수 구하기

#조건1
name <- c('유관순','홍길동','이순신','신사임당')
gender <- c('F','M','M','F')
price <- c(50,65,45,75)

ABC <- data.frame(name,gender,price)
ABC

#조건2
for(i in 1:nrow(ABC)){
  ifelse(ABC$price[i] >= 65, ABC$result[i] <- 'BEST',ABC$result[i]<- "NORMAL")
}
head(ABC,2)
nrow(ABC)
str(ABC)

##############################################################################
2. 다음 벡터 EMP는 '입사년도이름급여' 순으로 사원의 정보가 기록된 데이터이다.
벡터 EMP를 이용하여 다음과 같은 결과가 나타나도록 함수를 정의해보세요
(함수에 변수 EMP를 전달했을 때 출력결과와 같도록 만드시면 됩니다)

EMP <- c('2014홍길동220','2002이순신300','2010유관순260',"2019왕건500","2019동방신기1000")

<출력결과>
전체 급여 평균 : 456
평균이상 급여 수령자
왕건 => 500
동방신기 => 1000

library(stringr)
library(dplyr)
cal_sla <- function(EMP){
  EMP_new <- str_replace(EMP,'[0-9]{4}','')
  EMP_name <- unlist(str_extract(EMP_new,'[가-힣]{1,}'))
  EMP_sal <- unlist(str_extract(EMP_new,'[0-9]{1,}'))
  
  employee <- data.frame(이름 = EMP_name,급여 = EMP_sal, stringsAsFactors = F)
  employee$급여 <- as.numeric(employee$급여)
  sal_mean <- mean(employee$급여)
  cat('<출력결과>\n')
  cat('전체 급여 평균 : ',sal_mean,'\n')
  cat('평균이상 급여 수령자 \n')
  for(i in 1:length(employee$이름)){
    if(employee$급여[i] >= sal_mean){
           cat(employee$이름[i], ' => ', employee$급여[i],'\n')
    }
  }
}
cal_sla(EMP)
str(employee)
employee$급여

########################
3. 함수 y = f(x)에서 x의 값이 a에서 b까지 변할 때 △x = b - a를 증분이라 하며,
△y = f(b) - f(a)를 y의 증분으로 표현한다.
평균변화율 : △y/△x = (f(b)- f(a))/(b - a)

조건) 함수 f(x) = x^3 + 4에서 x의 값이 1에서 3까지 변할 때 평균변화율을
구하는 함수를 작성해보세요. (평균변화율 = (f(3)-f(1))/(3-1) = 13)

change <- function(x1,x2){
  y1 <- x1^3 +4
  y2 <- x2^3 +4
  a <- x2 - x1
  b <- y2 - y1
  return(b/a)
}
change(3,1)

#####################
4. 실습 : 몬테카를로 시뮬레이션 (runif)

몬테카를로 시뮬레이션은 현실적으로 불가능한 문제의 해답을 얻기 위해서 난수의

확률분포를 이용하여 모의시험으로 근사적 해를 구하는 기법

n번 시행했을 때 동전던지기의 확률을 구하라!

mon <- function(n){
  result = 0
  for(i in 1:n){
    coin <- runif(1)
    if (coin >= 0.5){
      result <- result +1
    }
  }
  prob <- result/n
  return(prob)
}
mon(100)

mks <- function(n){
  set.seed(100)
  result = 0
  for(i in 1:n){
    coin <- runif(1) #runif(n) 0~1 사이의 난수 n개 생성
    if(coin >= 0.5){
      result <- result +1
    }
  }
  prob <- result/n
  return(prob)
}
mks(999)
runif(3)

#8월5일
#탐색적 데이터 조회

setwd('C:/Users/09/Desktop/R/R_study/data')
dataset <- read.csv('dataset.csv',
                    header=T, stringsAsFactors=T)
View(dataset)

#NA 확인
table(is.na(dataset$resident)) 
FALSE  TRUE 
279    21 #NA 21개

#dataset 구조보기
##names() 컬럼명 확인
names(dataset)

#attribute() 컬럼명, class, 행이름
attributes(dataset)

str(dataset)
'data.frame':	300 obs. of  7 variables: #300개 행과 7개 열
 $ resident: int  1 2 NA 4 5 3 2 5 NA 2 ...
 $ gender  : int  1 1 1 2 1 1 2 1 1 1 ...
 $ job     : int  1 2 2 NA 3 2 1 2 1 2 ...
 $ age     : int  26 54 41 45 62 57 36 NA 56 37 ...
 $ position: int  2 5 4 4 5 NA 3 3 5 3 ...
 $ price   : num  5.1 4.2 4.7 3.5 5 5.4 4.1 675 4.4 4.9 ...
 $ survey  : int  1 2 4 2 1 2 4 4 3 3 ...
nrow(dataset)

# 조회 결과 변수 저장
x <- dataset$gender
y <- dataset$price

library(ggplot2)
plot(x,y)
#컬럼명 형식으로 변수 조회
dataset['gender']
dataset['price']
#인덱스로 변수조회
dataset[2,] #2행 모든 데이터 조회
dataset[,3] #3열 모든 데이터 조회
dataset[1] #1열 모든 데이터 조회

# 실습 색인 형식ㄱ으로 변수 조회
dataset[c("job", "price")]
dataset[c(1,2,3)]
dataset[-c(2)] #2빼고 나머지 칼럼들

#결측치 처리
#null 값이 아직 정해지지 않은 임시의 값
#NA 값이 있어야하는데 빠진것
summary(dataset$price)
sum(dataset$price) #결과가 NA로 나옴
sum(na.omit(dataset$price))#결측치 제외하고 추출
sum(dataset$price,na.rm = T) #결측치 제거

#결측치 대체
+결측치를 0으로 대체
x <- dataset$price
summary(x)
x[1:30]
x1 <- ifelse(!is.na(x),x,round(mean(x,na.rm = T),2))
x1[1:30]

#범주형 변수의 이상치 확인
table(dataset$gender)
pie(table(dataset$gender))

#데이터 정제하기
dataset <- subset(dataset,gender ==1 | gender ==2)
str(dataset)
nrow(dataset)
pie(table(dataset$gender), col = c('blue','red'))

##실습
#dataset에 resident 변수의 
1 --> 1.서울특별시,
2 --> 2.인천광역시, 3 --> 3.대전광역시, 4 --> 4.대구광역시
5 --> 5.시구군으로 표기하는 resident2 변수를 생성하세요
d <- c()
dataset$resident2[dataset$resident == 1] <- '1.서울'
dataset$resident2[dataset$resident == 2] <- '2.인천'
dataset$resident2[dataset$resident == 3] <- '3.대전'
dataset$resident2[dataset$resident == 4] <- '4.대구'
dataset$resident2[dataset$resident == 5] <- '5.시구군'
head(dataset)

library(dplyr)
d2 <- dataset %>%  select(resident,resident2)
head(d2)

#[실습] job 칼럼을 대상으로 코딩 변경하기
dataset$job[is.na(dataset$job)] <- 'F'
dataset$job[dataset$job == 1] <- '공무원'
dataset$job[dataset$job == 2] <- '회사원'
dataset$job[dataset$job == 3] <- '개인사업업'
head(dataset)

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

##5
exam <- read.csv('csv_exam.csv')
head(exam)
str(ggplot2::mpg)


df_new <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))

df_new <- data.frame(df_new)
##############dplyr의 일곱가지 함수################
1. rename() - 이름 변경함수
2. filter() - 원하는 조건에 맞는 행 추출
3. select() - 컬럼을 추출
4. arrange() - 정렬
5. mutate() - 변수 추가 함수
6. summarise() - 통계값 함수
7. join - 합치는 함수!
  
  
  
#이름변경 함수
df_new <- rename(df_new,V2 =var2)
head(df_new)
#컬럼 생성하기
df_new$sum <- df_new$var1 + df_new$V2
df_new

###mpg로 연습하기
mpg <- as.data.frame(ggplot2::mpg)
head(mpg,1)
mpg$total <- (mpg$cty + mpg$hwy)/2 #통합 연비 변수 생성
head(mpg)
hist(mpg$total)
###20이상이면 pass, 아니면 fail
mpg$test <- ifelse(mpg$total >= 20, 'PASS', 'FAIL')
head(mpg,20)
table(mpg$test)
qplot(mpg$test)

#연습문제
데이터 설명
ratings 데이터 : user id, movie id, rating, time stamp
movies 데이터 : movie id, title, genres
user 데이터 : user id, gender, age, occupation, zip code

1.데이터 조사, 탐색적 분석, 전처리 등
#movies
movie_vec <- readLines('movies.dat', encoding = 'UTF-8')
head(movies)
mf <- gsub('::',';',movie_vec)
str(mf)
movie <- read.csv(text=mf, header = F, stringsAsFactors = F, sep =';')
str(movie)
View(movie)

movie <- rename(movie, movie_id = V1)
movie <- rename(movie, title = V2)
movie <- rename(movie, genres = V3)
head(movie)

#raitng
rating_vec <- readLines(con = 'ratings.dat',encoding = 'UTF-8')
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
user_vec <- readLines(con = 'users.dat',encoding = 'UTF-8')
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
u1 <- user %>% 
  select(user_id, gender)
r1 <- rating %>% 
  select(user_id,rating,movie_id)
m1 <- movie %>% 
  select(movie_id,title)
ur <- left_join(r1,u1,id='user_id')
table(is.na(ur))
head(ur)
urm <- left_join(ur,m1,id='movie_id')
m1$movie_id <- as.integer(m1$movie_id)
urm <- left_join(ur,m1,id='movie_id')
table(is.na(urm))

urm_meanr <- urm %>% 
  group_by(title,gender) %>% 
  summarise(mean_raing = mean(rating))
urm_meanr

2-2) 여성에게 인기 있는 top20 영화는?(단, 평가자 100명 이상)
female_best <- urm %>% 
  filter(gender == 'F') %>% 
  group_by(title,movie_id) %>% 
  summarise(mean_mf = mean(rating), num = n()) %>% 
  filter(num >= 100) %>% 
  arrange(desc(mean_mf)) %>% 
  head(20)
female_best

###8월6일
library(mlbench)
data("Ozone")
ozo <- Ozone
library(ggplot2)
plot(ozo$V8,ozo$V9)
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp',pch=20 , cex=0.5)

plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch='+', cex=0.7)
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch='+', cex=0.7)
cars
head(cars,3)
plot(cars$speed,cars$dist)
par(mfrow = c(1,2),
    pty = 's')
plot(cars,type = 'b')
plot(cars, type = 'o')

tail(ozo[,c('V6','V7')],10)
#지터 jitter() : 데이터에 약간의 노이즈를 추가함
par(mfrow = c(1,2),
    pty = 's')
plot(ozo$V6,ozo$V7,xlab = 'wind speed',
     ylab = 'Humidity', pch = 15, cex = .5)
plot(jitter(ozo$V6), jitter(ozo$V7),
     xlab = 'wind speed',ylab = 'Humidity',
     pch = 15, cex = .5)

#points() : 이미 생성된 plot()에 점을 추가로 그려줌
data(iris)
with(iris, {
  plot(NULL, xlim = c(0,5), ylim = c(0,10),
       xlab = 'width', ylab = 'length',
       main = 'iris',type = 'n')
  points(Sepal.Width, Sepal.Length, cex = 1 ,pch = "*", col = 'blue')
  points(Sepal.Width, Petal.Length, cex = 1 ,pch = "+", col = 'red')
})

#꺾은선
x <- seq(0,2*pi,0.1)
y <- sin(x)
plot(x,y,cex =0.5, col = 'red')
lines(x,y)
#abline() : 직선
plot(cars,xlim = c(0,25));abline(a = -5, b =3.5,col = 'red')


plot(cars, xlim = c(0,25))
abline(a= -10, b=3.2, col = 'purple')
abline(h=mean(cars$dist), lty = 2, col='gray')
abline(v=mean(cars$speed),lty = 2, col ='light gray')

#curve() : 곡선
curve(cos, 0, 2*pi)
abline(v=pi, lty = 15, col = 'light blue')

plot(cars, cex = 0.5)
text(cars$speed, cars$dist,pos=3,cex=0.5)

plot(cars, cex = 0.5)
identify(cars$speed,cars$dist)

plot(iris$Sepal.Width,iris$Sepal.Length,pch =1,
     xlab = 'width', ylab = 'length', xlim = c(0,5), ylim = c(0,8))
points(iris$Petal.Width,iris$Petal.Length, pch = '+', col = 'red')
legend('bottmright', legend=c('Sepal', 'Petal'), pch=c(1,43),
       col = c('black','red'), bg = 'grey') #벎례

# legend()

plot(iris$Sepal.Width, iris$Sepal.Length, pch = 1,
     xlab = 'Width', ylab = 'length', xlim = c(0,5), ylim = c(0,8))
points(iris$Petal.Width, iris$Petal.Length, pch = "+", col = 'red')
legend('bottomright', legend = c("Sepal","Petal"), pch = c(1,43),
       col = c('black', 'red'), bg = 'grey')

# # 3D scatter plot
# install.packages('scatterplot3d')
# library(scatterplot3d)
# 
# #데이터 불러오기
# iris_setosa <- iris[iris$Species == 'setosa',]
# iris_versicolor <- iris[iris$Species == 'versicolor',]
# iris_virginicar <- iris[iris$Species == 'virginica',]
# 
# #s.p.3d(밑변, 오른쪽변, 왼쪽변)
# 
# iris_d3 <- scatterplot3d(iris$Petal.Length,iris$Sepal.Length,
#                          iris$Sepal.Width,type = 'n', angle = 30)
# iris_d3$points3d(iris_setosa$Sepal.Length,
#                  iris_setosa$Sepal.Length,
#                  iris_setosa$Sepal.Width,
#                  bg = 'red', pch =23)
# iris_d3$points3d(iris_versicolor$Sepal.Length,
#                  iris_versicolor$Sepal.Length,
#                  iris_versicolor$Sepal.Width,
#                  bg = 'green', pch =23)
# iris_d3$points3d(iris_virginica$Sepal.Length,
#                  iris_virginica$Sepal.Length,
#                  iris_virginicar$Sepal.Width,
#                  bg = 'blue', pch =23)

#3D scatter plot
install.packages('scatterplot3d')
library(scatterplot3d)

#데이터 불러오기
iris_setosa <- iris[iris$Species == 'setosa',]
iris_versicolor <- iris[iris$Species == 'versicolor',]
iris_virginica <- iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽 변, 왼쪽 변)

#1. 3차원 프레임(틀) 생성
#2. 산점도 시각화

iris_d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length,
                         iris$Sepal.Width, type = 'n', angle = 30)
iris_d3$points3d(iris_setosa$Petal.Length,
                 iris_setosa$Sepal.Length,
                 iris_setosa$Sepal.Width,
                 bg = 'red', pch = 21)
iris_d3$points3d(iris_versicolor$Petal.Length,
                 iris_versicolor$Sepal.Length,
                 iris_versicolor$Sepal.Width,
                 bg = 'blue', pch = 23)
iris_d3$points3d(iris_virginica$Petal.Length,
                 iris_virginica$Sepal.Length,
                 iris_virginica$Sepal.Width,
                 bg = 'green', pch = 25)

#boxplot
boxplot(mpg$hwy)

#상자수염그림 통계치 출력
boxplot(mpg$hwy)$stats

#12~37벗어나면 NA할당
mpg$hwy <- ifelse(mpg$hwy <12 | mpg$hwy >37, NA, mpg$hwy)

#결측치 확인
table(is.na(mpg$hwy))
install.packages('dplyr')
library(dplyr)
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

###8월7일
#Scrapping
install.packages('rvest')
library(rvest)
library(dplyr)
library(stringr)

#1. url

url_main <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=187940&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='

#2. html scrapping
#setwd('')
getwd()
download.file(url = paste0(url_main,1,''), #공백을 없도록
              destfile = 'bakdu.html', quiet = T) #파일명을 무엇으로 할것이냐
bakdu_review <- read_html('bakdu.html')

bakdu_review

#3. get data###################################################################
##3.1 star score
star_score <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.star_score') %>% 
  html_text() #html text 긁어오는 함수
star_score1 <- str_replace_all(star_score,'\r|\t|\n','')
star_score1

##3.2 review
review_text <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('p') %>%  #말단의 노드에는 점없이해야함
  html_text() #html text 긁어오는 함수
review_text
review_text1 <- str_replace_all(review_text,'\r|\t|\n','')
review_text1

##3.3 user
user <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('dt') %>%  #말단의 노드에는 점없이해야함
  html_text() #html text 긁어오는 함수
user
user1 <- str_replace_all(user,'\r|\t|\n','')
user1

###3.3.1 user_id
user_id <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('em') %>%
  html_text()
user_id <- str_replace_all(user_id,'\r|\t|\n','')
user_id
###3.3.2 date
date <- str_extract_all(user1,'[0-9]{4}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}')
date1 <- unlist(date)
date1

##3.4 like
like <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.btn_area') %>%
  html_nodes('._sympathyButton') %>% 
  html_text()
like
like_count <- str_replace_all(like,'\r|\t|\n','')
like_count

#3.5 unlike
unlike <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.btn_area') %>%
  html_nodes('._notSympathyButton') %>% 
  html_text()
unlike
unlike_count <- str_replace_all(unlike,'\r|\t|\n','')
unlike_count

###############################################################################
#4. loop

star_list <- c()
text_list <- c()
user_id_list <- c()
date_list <- c()
like_list <- c()
unlike_list <- c()

page_no <- bakdu_review %>% 
  html_nodes('.score_total') %>% 
  html_nodes('.total') %>% 
  html_node('em') %>% 
  html_text()

page_no <- gsub(',','',page_no)
page_no <- as.numeric(page_no)
page_no <- ifelse(page_no%%10, page_no%/%10+1,page_no%/%10)
page_no
for(page in 1 : page_no){
  if(page == 747){ #747쪽에 오류가 난다. 그래서 747page를 제외하기 위한 코드
    next
  }
  url <- paste0(url_main,page,'')
  fname <- sprintf('bakdu_review(%d).html',page)
  download.file(url,destfile = fname,quiet = T)
  review_temporary <- read_html(fname)
  
  #평점
  star_score <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.star_score') %>%
    html_text()
  star_score1 <- str_replace_all(star_score,'\r|\t|\n','')
  star_list <- append(star_list,star_score1)
  
  #리뷰
  # text <- review_temporary %>%
  #   html_nodes('.score_result') %>%
  #   html_nodes('.star_reple') %>%
  #   html_node('p') %>%
  #   html_text()
  # text1 <- str_replace_all(text,'\r|\t|\n','')
  # text_list <- append(text_list,text1)
  # text_list
  text <- review_temporary %>% 
    html_nodes(".score_result") %>% 
    html_nodes(".score_reple") %>% 
    html_node('p') %>%  
    html_text()
  text1 <- str_replace_all(text,"\r|\t|\n","")
  text_list <- append(text_list,text1)
  text_list
  
  #사용자
  userid <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.score_reple') %>%
    html_node('em') %>%
    html_text()
  userid1 <- str_replace_all(userid,'\r|\t|\n','')
  user_id_list <- append(user_id_list,userid1)
  
  #날짜
  user <-review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.score_reple') %>%
    html_node('dt') %>%  #말단의 노드에는 점없이해야함
    html_text() #html text 긁어오는 함수
  user1 <- str_replace_all(user,'\r|\t|\n','')
  date <- str_extract_all(user1,'[0-9]{4}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}')
  date1 <- unlist(date)
  date_list <- append(date_list,date1)
  
  #공감
  like <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.btn_area') %>%
    html_nodes('._sympathyButton') %>%
    html_node('strong') %>% 
    html_text()
  like_count <- str_replace_all(like,'\r|\t|\n','')
  like_list <- append(like_list,like_count)
  
  #비공감
  unlike <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.btn_area') %>%
    html_nodes('._notSympathyButton') %>%
    html_node('strong') %>% 
    html_text()
  unlike_count <- str_replace_all(unlike,'\r|\t|\n','')
  unlike_list <- append(unlike_list,unlike_count)
  
  cat("남은 페이지는", page_no-page)
  
}
length(user_id_list)
length(text_list)


str(star_list) 
str(text_list)
str(user_id_list )
str(date_list )
str(like_list )
str(unlike_list )
unlike_list[1]

####데이터 프레임 생성
review <- data.frame(리뷰어= user_id_list, 일시 = date_list,평점 = star_list, 리뷰 = text_list, 공감 = like_list, 비공감 = unlike_list)
View(review)

####파일 저장
#install.packages('writexl')
library(writexl)
.libPaths()
write_xlsx(review, path = 'C:/Users/09/Desktop/R/R_study/data/review.xlsx')

table(review$리뷰어)

#ID와 닉네임 분리하기
# ID만 분리
id<-c()
for(i in 1:length(review$리뷰어)){
  id[i]<-str_extract(review$리뷰어[i],'\\((.*?)\\)')
  id[i]<-gsub('[)(]',"",id[i])
  if(is.na(id[i])){
    id[i]<-review$리뷰어[i]
  }
}

review$리뷰어<-id
View(review)

write_xlsx(review, path = 'C:/Users/09/Desktop/R/R_study/data/review_id.xlsx')

#동일ID 빈도 보기
install.packages('readxl')
library(readxl)
aa <- read.xlsx(file = './data/review_id.xlsx',sheetIndex = 1,encoding =  'UTF-8')
View(aa)
review_order <- head(sort(table(aa),decreasing = T),30)
review_order

barplot(review_order)

#평점의 분포를 알아보

library(stringr)
library()
idx <- which(str_detect(aa$리뷰,'알바'))
aa_alba <- aa[idx,]
str(aa_alba)
aa_alba[aa_alba$평점 == 1,3]
aa_alba[aa_alba$평점 == 10,3]
table(aa[idx,]$평점)
which(aa[idx,]$평점 ==1)

idx_temp <- which(aa[idx,]$평점 == 1)
idx_temp























