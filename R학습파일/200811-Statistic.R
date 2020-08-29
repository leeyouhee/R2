get_blogData <- function(x){
  searchQuery <- paste('query=', x)
  urlStr <- 'https://openapi.naver.com/v1/search/blog.xml?'
  searchStr <- iconv(searchQuery, to ='UTF-8')
  otherStr <- '&display=100&start=1&sort=sim'
  reqURL <- paste(urlStr,searchStr,otherStr,sep = '')
  if(require(httr)){ #require = library 단, 함수내에 변수로 사용가능
    print('package exists')
  }else{
    print('need to install httr package')
  }
  clientID <- 'PnRvqOklqoJ3PUo_jth9'
  clientPW <- 'TRd9zhQk9z'
  apiResult <- GET(reqURL,  add_headers('X-Naver-Client-Id' = clientID,
                                        'X-Naver-Client-Secret' = clientPW))
  blogData <- rawToChar(apiResult$content)
  Encoding(blogData) <- 'UTF-8'
  return(blogData)
}



###############save function##
save(get_blogData, file = 'get_blogData.rdata')

outWords <- c('물놀이','더위')
outWords
nouns %in% outWords


nouns.final <- nouns[!nouns %in% outWords]
nouns.final

##통계##
#기술통계 분석
기술통계란 자료를 요약하는 기초적인 통계랑을 데이터 분석 전에
전체적인 데이터 분포의 이해와 통계적 수치를 제공한다. 이러한 기술 통계량은 모집단의
특성을 유추하는데 이용할 수 있다.

#1.빈도 분석
빈도분석은 설문조사 결과에 대한 가장 기초적인 정보를 제공해주는 분석방법으로
광범위하게 이용된다.
특히 성별이나 직급을 수치화하는 명목척도나 서열척도 같은 범주형 데이터를 대상으로
비율을 측정하는 데에 주로 이용된다.
Ex) 특정 선거 후보가 얼마만큼의 지지(%)를 받고 있는가? 응답자 중 성비(%는?)

#2. 기술통계 분석
기술통계 분석은 빈도분석과 유사하지만 등간척도나 비율척도와 같은 연속적 데이터를 분석할 때
주로 이용한다. 명목척도나 서열척도와 같은 범주형 데이터는 수치에 의미가 없기 때문이다.
분포의 특성은 표본의 평균값, 중앙값, 최빈값 등으로 나타내며,
빈도수, 비율, 표준편차, 분산 등으로 표본의 분포를 알 수 있다.
  +대표값 : mean, sum, median, mode(최빈수), quatile
  +산포도 : Variance, Standard deviation, min/max, range
  +비대칭도 : 첨도(Kurtosis), 왜도(Skewness)

##2.1 척도별 기술통계량
#실습 데이터 셋 가져오기
data <- read.csv('./data/descriptive.csv', header = T)
head(data,3)

dim(data)#행과 열 정보 - 차원보기
length(data) #컬럼의 길이
length(data$survey)
str(data)
str(data$survey)

#데이터 특성
summary(data)
#2.1 명목척도 기술 통계량, 명목 --> factor 따라서 의미있는것은 빈도!
length(data$gender)
summary(data$gender) #최소, 최대, 중위수, 평균 - 의미없음
table(data$gender) #각 성별 빈도수 - outlier 확인 -> 0,5
#이상치 제거
library(dplyr)
data <- subset(data, data$gender == 1 | data$gender ==2) 
x <- table(data$gender)
x
barplot(x)

#구성비율 계산 : prop.table() -> 명목변수들 간의 구성 비율 계산해줌
prop.table(x)
y <- round(prop.table(x)*100,2)
y
cat('male :', y[1],'%', 'female : ', y[2])
#서열척도 : 위계를 갖고 있다
length(data$level)
summary(data$level)
table(data$level)

#학력수준 변수의 빈도수 시각화
x1 <- table(data$level)
barplot(x1)

#등간척도 기술 통계량 --> 등간격(서열x)
survey <- data$survey
survey
summary(survey)#만족도(5)인 경우 의미 있음 -> 2.6평균이상
x1 <- table(survey)
x1
hist(survey)
pie(x1)

#비율척도 기술 통계량
length(data$cost)
summary(data$cost) #median의미있음

plot(data$cost)
data <- subset(data, data$cost >= 2 & data$cost <= 10)
data
x <- data$cost
x
mean(x)


#대표값 구하기 - cost 변수 대상 대표값 구하기
mean(x)
median(x)
sort(x)
sort(x,decreasing = T)

quantile(x, 1/4)
quantile(x, 2/4)
quantile(x,3/4)
quantile(x,4/4)

#산포도
var(x)
sd(x)
sqrt(var(x))
sd(x)^2

#표본분산과 표본 표준편차
#빈도분석
#cost 변수의 빈도분석과 시각화
table(data$cost)
hist(data$cost)
plot(data$cost)
data$cost2[data$cost >= 1 & data$cost <=3] <- 1
data$cost2[data$cost >= 4 & data$cost <=6] <- 2
data$cost2[data$cost >= 7] <- 3

table(data$cost2)
barplot(table(data$cost2))
pie(table(data$cost2))


#비대칭도 구하기
#install.packages('moments')#왜도, 첨도 위한 패키지지
library(moments)
cost <- data$cost #정제된 data
cost

#왜도 - 평균을 중심으로 기울어진 정도
skewness(cost)
#첨도 - 표준정규분포와 비교하여 얼마나 뾰족한가 측정 지표
kurtosis(cost)
#확률밀도/표준정규분포 곢선
hist(cost, freq= F)
#확률밀도 분포 곡선
lines(density(cost), col = 'red')
#표준정규분포 곡선
x <- seq(0,8,0.1)
curve(dnorm(x, mean(cost), sd(cost)), col = 'blue', add = T)

#연습 이항분포 dbinom(성공횟수,시행횟수,성공확률) = P(x)
dbinom(0,3,0.9)
dbinom(1,3,0.9)
dbinom(2,3,0.9)
dbinom(3,3,0.9)

n = 3
result <- numeric()
for(i in 0:n){
  cat(i, '번 성공 확률 = ', dbinom(i,n,0.9),'\n')
  result <- append(result,dbinom(i,n,0.9))
}
result

a <- seq(0,1000,1)
b <- dbinom(a,1000,0.78)
plot(a,b)
#통계연습
#1. 5지선다인 20문항의 시험에서 랜덤하게 답안을 고를 경우 다음의 확률은 어떻게 되겠는가?
1) 다 틀릴 확률
4^20/5^20
dbinom(0,20,0.2)

2) 8개 이상 맞출 확률 

n = 20
result <- numeric()
for(i in 8:n){
  cat(i, '번 성공 확률 = ', dbinom(i,n,0.2),'\n')
  result <- append(result,dbinom(i,n,0.2))
}
result

#2.
CEO에게 해당 블라인드 테스트 진행을 제안하자, CEO는 100명 중 최소 40명이 
맥주를 맞힐 확률이 95% 이상이라면 $170M을 기꺼이 투자하겠다고 밝혔다. 
이 기획을 서포트 하기 위해 R을 이용해 확률을 구해보세요.
(단, 테스트 진행 표본수(n)과 성공횟수(k)를 변동시키며 값을 확인 할 수 있도록 UDF로 구현해보세요)
테스는 2개의 맥주중 자사 것을 고르는 것임
beer <- function(n,k){
  y <- dbinom(k, size = n, prob = 0.5)
  z <- sum(dbinom(k:n, size = n, prob = 0.5))
  cat(n,"명 중 ",k, "명이 선택활 확률은", round(y*100 ,2), "% \n")
  cat(n,"명 중 ",k, "명 이상이 선택활 확률은", round(z*100 ,2), "% \n")
}

beer(10000,5000)

beer1 <- function(n,k){
  y <- dbinom(k, size = n, prob = 0.5)
  z <- 1- pbinom(k-1, n, prob = 0.5)
  cat(n,"명 중 ",k, "명이 선택활 확률은", round(y*100 ,2), "% \n")
  cat(n,"명 중 ",k, "명 이상이 선택활 확률은", round(z*100 ,2), "% \n")
}

beer1(100,40)

#3. 정규분포 그리기
x <- seq(0,100,1)
x     
plot(x,dnorm(x, mean = 80,sd=5),type = 'l',main = '신입사원 시험성적 분포',ylab = 'Probability',xlab= 'score')
plot(x,pnorm(70, mean=80, sd=5),type = 'l',main = '신입사원 시험성적 분포',ylab = 'Probability',xlab= 'score')     

y <- (1/sqrt(2*pi))*exp(-x^2/2)
plot(x,y,type= 'l',col='green')     
     
     

x <-rnorm(200, 80,5)
x <- sort(x) 
d <-dnorm(x,80,5)
plot(x,d)

xy<-data.frame(x=x, y=d)
View(xy)