#8월 11일 통계

##기술 통계 분석
#1. 기술통계량
data <- read.csv('./data/descriptive.csv', header = T)
head(data,3)
dim(data)
length(data) # 8
length(data$survey) # 300
str(data)
#데이터 특성 파악 summary()
summary(data)

#2. 명목척도 기술 통계량
+ 명목척도 데이터는 factor형을 의미한다. 따라서 빈도분석이 의미있다.
str(data)
length(data$gender)
summary(data$gender) #최소, 최대, 중위수, 평균 - 의미없음
table(data$gender) #빈도가 의미있음
#2-1. 이상치 제거
library(dplyr)
gendata <- subset(data,data$gender == 1 | data$gender == 2)
table(gendata)
x <- table(gendata$gender)
x # 1: 172; 2: 124; 이상치 -> 3개
barplot(x)
#2-2. 구성비율 계산
+prop.table() : 명목변수들 간의 구성 비율 계산
prop.table(x)
y <- round(prop.table(x),2)
y
cat('남자는 ' , y[1], '%를 차지한다.')

#3. 서열척도 기술 통계량
str(data)
length(data$level)
summary(data$level)
table(data$level)
#3-1. 학력수준 변수의 시각화
x1 <- table(data$level)
barplot(x1)

#4. 등간척도 기술 통계량
sur <- data$survey
head(sur)
summary(sur)
x2 <- table(sur)
x2
hist(sur)
pie(x2)

#5. 비율척도 기술 통계량
summary(data$cost)
plot(data$cost)
data <- subset(data,data$cost >= 2 & data$cost <= 10)
data
x <- data$cost
x
mean(x)

#5-1. 대표값 구하기 - cost 변수 대상 대표값 구하기
mean(x)
median(x)
sort(x)
sort(x,decreasing = T)

quantile(x,1/4)

#5-2. 산포도
var(x)
sd(x)
sd(x)^2

#5-3. cost 변수의 빈도분석과 시각화
table(data$cost)
hist(data$cost)
plot(data$cost)
data$cost2[data$cost >= 1 & data$cost <=3] <- 1
data$cost2[data$cost >= 4 & data$cost <=6] <- 2
data$cost2[data$cost >= 7] <- 3
table(data$cost2)
barplot(table(data$cost2))
pie(table(data$cost2))

#5-4. 비대칭도 구하기
library(moments)
cost <-data$cost

#5-4-1. 웨도
+평균을 중심으로 기울어진 정도
skewness(cost)
#5-4-2. 첨도
+표준정규분포와 비교하여 얼마나 뾰족한가 측정 지표
kurtosis(cost)
#확률밀도/표준정규분포 곡선
hist(cost,freq = F)
lines(density(cost),col='green')
x <- seq(0,8,0.1)
curve(dnorm(x,mean(cost),sd(cost)),col = 'red', add =T)

#연습
+이항분포 dbinom(성공횟수,시행횟수,성공확룔) = P(x)
dbinom(0,3,0.9)
dbinom(1,3,0.9)
dbinom(2,3,0.9)
dbinom(3,3,0.9)

n = 3
result <- numeric()
for(i in 0:n){
  cat(i, '번 성공확룔 = ', dbinom(i,n,0.9),'\n')
  result <- append(result,dbinom(i,n,0.9))
}

a <- seq(0,1000,1)
b <- dbinom(a,1000,0.78)
plot(a,b)

#연습2
+5지선다인 20문항의 시험에서 랜덤하게 답안을 고를 경우 다음의 확률은 어떻게 되겠는가?
1) 다 틀릴확률
dbinom(0,20,0.2)

2) 8개 이상 맞출 확률
n = 20
result <- numeric()
for(i in 8:n){
  cat(i, '번 성공확룔 = ', dbinom(i,n,0.2),'\n')
  result <- append(result, dbinom(i,n,0.2))
}
result

#연습3
+CEO에게 해당 블라인드 테스트 진행을 제안하자, CEO는 100명 중 최소 40명이 
맥주를 맞힐 확률이 95% 이상이라면 $170M을 기꺼이 투자하겠다고 밝혔다. 
이 기획을 서포트 하기 위해 R을 이용해 확률을 구해보세요.
(단, 테스트 진행 표본수(n)과 성공횟수(k)를 변동시키며 값을 확인 할 수 있도록 UDF로 구현해보세요)
테스는 2개의 맥주중 자사 것을 고르는 것임

beer <- function(n,k){
  x <- dbinom(k, n, 0.5)
  y <- sum(dbinom(k:n, n, 0.5))
  cat(n,"명 중 ",k, "명이 선택활 확률은", round(x*100 ,2), "% \n")
  cat(n,"명 중 ",k, "명 이상이 선택활 확률은", round(y*100 ,2), "% \n")
}

beer(100,40)

beer1 <- function(n,k){
  y <- dbinom(k, size = n, prob = 0.5)
  z <- 1- pbinom(k-1, n, prob = 0.5)
  cat(n,"명 중 ",k, "명이 선택활 확률은", round(y*100 ,2), "% \n")
  cat(n,"명 중 ",k, "명 이상이 선택활 확률은", round(z*100 ,2), "% \n")
}

beer1(100,40)

#연습4
+정규분포 그리기
x <- seq(0,100,1)
x
plot(x,dnorm(x,80,5),type = 'l',main ='신입사원 시험성적 분포',ylab = 'Probability',xlab= 'score')
plot(x,pnorm(x, mean=80, sd=5),type = 'l',main = '신입사원 시험성적 분포',ylab = 'Probability',xlab= 'score')

x <-rnorm(200, 80,5)
x <- sort(x) 
d <-dnorm(x,80,5)
plot(x,d)

#중심극한 정리
N <- seq(0,9,1)
N
set.seed(99)
x <- sample(N,5,replace = T)
x
mean_x <- mean(x)
list <- c()
for(i in 1:1000){
  x <- sample(N,5,replace = T)
  mean_x <- mean(x)
  list <- append(list,mean_x)
}
list
hist(list)



#5개의 값이 각 표본에 저장되도록 매트릭스 생성
a <- 0:9
rand <- list(sample(a,5,replace = T))
set.seed(55);for(i in 1:1000){
  rand[i] <- list(sample(a,5,replace = T))
}
head(rand)
hist(rand)
rand <- matrix(unlist(rand),nrow = 100,byrow = T)
head(rnad)
#df 변경
rand1 <- data.frame(rand)
head(rand1)
rand1$sum <- apply(rand1,1,sum)
library(dplyr)
rand2 <- rand1 %>% 
  mutate(mean = sum/5)
head(rand2)
hist(rand2$mean,labels = T,freq = F,ylim = c(0,0.35))
lines(density(rand2$mean))


#8월 12일

#1.1 교차분석
+범주형 자료를 대상으로 2개 이상의 변수들에 대한 관련성을 알아보기 위해
+결합분포를 나타내는 교차분할표를 작성하고 이를 통해서 변수 상호간의 관련성 여부를
+분석하는 방법

##1. 데이터 프레임 생성
data1 <- read.csv('./data/cleanDescriptive.csv',header = T)
head(data1)
##2. 변수 리코딩
x <- data1$level2 #부모 학력수준
y <- data1$pass2  #자녀 대학 진학여부
##3. 데이터 프레임 생성
crosstabulation <- data.frame(parentsLv = x, Child_Univ = y)
dim(crosstabulation)
head(crosstabulation)
##4. 교차분석
library(gmodels)
library(ggplot2)
gmodels::CrossTable(crosstabulation$parentsLv,crosstabulation$Child_Univ)
table(crosstabulation)
plot(crosstabulation$parentsLv,crosstabulation$Child_Univ)

################################################################################

#2.1 카이제곱 검정
+카이제곱 검정은 범주별로 관측빈도와 기대빈도의 차이를 통해서 확률 모형이
+데이터를 얼마나 잘 설명하는지를 검정하는 통계방법
  1)일원 카이제곱 검정
    - 교차분할표 이용하지 않고 1개의 변인(집단 또는 범주)을 대상으로 검정 수행
    - 관찰도수와 기대도수가 일치하는지 검정 --> 적합도 검정
    - 주사위 게임, 선호도 분석
  2)이원 카이제곱 검정
    - 교차분할표 이용하고 2개 이상의 변인 대상
    - 독립성 검정 : 한 집단 내에서 2변인의 관계가 독립인지 검정
      -> 귀무가설 : 둘은 관계가 없다.
    - 동질성 검정 : 2 집단 내에서 각 범주간의 비율이 서로 동일한지 검정
      -> 귀무가설 : 모든 표본들의 비율은 동일하다.
##1. 일원 카이제곱 검정 
주사위 게임
+귀무가설 : 기대치와 관찰치는 동일하다.
+대립가설 : 기대치와 관찰치는 동일하지 않다.
+60회 주사위를 던져서 나온 관측도수와 기대도수
  - 관측도수 : 4,6,17,16,8,9
  - 기대도수 : 10,10,10,10,10,10
chisq.test(c(4,6,17,16,8,9))

##2. 이원 카이제곱 검정
독립성 검정
+대립 가설 : 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 있다.
+귀무 가설 : 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 없다.
d <- read.csv('./data/cleanDescriptive.csv',header = T)
str(d)
x <- d$level2
y <- d$pass2

CrossTable(x,y,chisq = T)
#Chi^2 =  2.766951     d.f. =  2     p =  0.2507057

##3. 단일집단 검정
##3-1. 비율검정
+단일집단의 비율이 어떤 특정한 값과 같은지를 검정하는 방법
+데이터 준비 -> 전처리 -> 기술통계 -> binom.test() -> 검통량 분석
<실습>
  H1 : 기존 2020년도 고객 불만률과 2019년 CS 교육 후 불만률에 차이가 없다.
  H0 : 기존 2020년도 고객 불만률과 2019년 CS 교육 후 불만률에 차이가 있다.

+1단계 :데이터 준비
data <- read.csv('./data/one_sample.csv',header = T)
head(data)

+2,3단계 : 데이터 전처리 및 기술통게
x <- data$survey
summary(x)
length(x)
table(x)
x
0   1 
14 136 

+4단계 binom.test() 수행
binom.test(c(136,14),p=0.8,alternative = 'two.sided',conf.level = 0.95)
  -> 양측검정;신뢰구간 95%;만족기준 80%
#해석 : p-value = 0.0006735으로 유의수준 0.05보다 작으므로
#       기존 2020년도 고객 불만률과 2019년 CS 교육 후 불만률에 차이가 있다.
  binom.test(c(136,14),p=0.8, alternative = 'greater', conf.level = 0.95)
#####해석
2020년 cs 교육 후 만족도 조사를 실시한 결과 150명 중 136명이 만족하였고 기존의 
80% 만족도보다 높다는 비율검정을 실시한 결과 p-value는 0.0003179로 유의수준 5%보다 매우 
낮으므로 cs 교육 후 만족도가 높지 않다는 귀무가설을 기각하고 대립가설을 채택한다.
따라서, 고객만족도는 기존보다 높다

##3-2. 평균검정
단일집단의 평균이 어떤 특정한 집단의 평균과 차이가 있는지를 검정하는 방법
정규성 검정(shapiro.test()) 실시하여, 정규분포이면 t.test()
                                      정규분포 아니면 wilcox.test()
shapiro.test() 귀무가설은 '데이터가 정규성을 따른다'

<실습>
상황 : 국내에서 생산된 노트북 평균 사용시간이 5.2시간으로 파악된 상황에서
A회사에서 생산된 노트북 평균 사용시간과 차이가 있는지를 검정하기 위해 A회사 노트북
150대를 랜덤하게 선정하여 검정을 실시한다.
H1 : 국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균사용시간에 차이가 있다.
H0 : 국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균사용시간에 차이가 없다.

+1단계 : 데이터 불러오기
a <- read.csv('./data/one_sample.csv',header = T)
head(a)
x <- a$time
+2단계 : 데이터 전처리
summary(x)
mean(x)
mean(x,na.rm = T)
x1 <- na.omit(x)
+3단계 : 정규분포 검정
shapiro.test(x1)
+4단계 : 정규분포 시각화
hist(x1)
qqnorm(x1)
qqline(x1,lty=1,col='green')
+5단계 : 평균차이 검정
t.test(x1, mu =5.2)
result <- t.test(x1,mu=5.2,alternative = 'two.side',conf.level = 0.95)
result
result1 <-t.test(x1,mu=5.2,alternative = 'greater',conf.level = 0.95)
result1

##4. 두집단 검정
##4-1. 두 집단 비율검정
데이터 준비 -> 전처리 -> 두집단 subset 생성 -> prop.test() -> 검통량 분석
<실습>
상황 : IT 교육센터에서 PT를 이용한 프리젠테이션 교육방법과 실시간 코딩 교육방법을
각각 적용하였다. 2가지 교육방법 중 더 효과적인 교육방법을 조사하기 위해서 교육생 
각 150명을 대상으로 설문조사
H0 : 두가지 교육방법에 따라 규육생의 만족률 차이가 없다.
H1 ; 두가지 교육방법에 따라 교육생의 만족률 차이가 있다.

+1단계 : 데이터 준비
data <- read.csv('./data/two_sample.csv',header = T)
head(data)
+2단계 : 전처리 및 subset() 생성
method <- data$method
survey <- data$survey #만족 :1 , 불만족 : 0
table(method)
table(survey)
table(method,survey)
+3단계 : 두 집단 비율 차이 검증
prop.test(c(110,135),c(150,150))
prop.test(c(110,135), c(150,150),alternative = 'greater',conf.level = 0.95)
prop.test(c(135,110), c(150,150),alternative = 'greater',conf.level = 0.95)

###########################################################################3#

#연습문제 1
교육수준과 흡연율이 관계가 있는지 검정을 실시해보자(data : smoke.csv)
+교육 1 : 대조 2: 고졸 3: 중졸
+흡연 1 : 과다 2: 보통 3: 비흡연
es <- read.csv('./data/smoke.csv',header = T)
head(es)
edu <- es$education
smo <- es$smoking
CrossTable(edu,smo,chisq = T)
##해석
1. 가설설정
H0 : 교육수준과 흡연율은 관계가 없다.
H1 : 교육수준과 흡연율은 관계가 있다.
2.기각역 설정
유의수준 : 0.05
3.계산 결과
검정통계량 : 18.91092
자유도 : 4
p-val : 0.0008182573
4.기각 여부
p-val이 유의수준보다 작으므로 귀무가설 기각
즉, 교육수준과 흡연율 사이에는 관계가 존재한다.

##############################################################################
#연습문제 2
2. 나이(age3)와 직위(position)간의 관련성을 독립성 검정(data : cleanData.csv)
+age변수 설명 1: 청년층 2: 중년층 3: 장년층
+position 변수 설명 1: very high 2: high 3: mid 4: low 5: very low

ap <- read.csv('./data/cleanData.csv',header = T)
head(ap)
str(ap)

age <- ap$age3
pos <- ap$position

CrossTable(age,pos,chisq = T)
##해석
1. 가설설정
H0 : 나이와 직위 사이에는 독립성이 있다.
H1 : 나이와 직위 사이에는 독립성이 없다.
2.기각역 설정
유의수준 : 0.05
3.계산 결과
검정통계량 : 287.8957
자유도 : 8
p-val : 1.548058e-57
4.기각 여부
p-val이 유의수준보다 작으므로 귀무가설 기각
즉, 교육수준과 흡연율 사이에는 독립성이 존재하지 않는다.
###############################################################################
#연습문제3
3. 직업유형에 따른 응답 정도에 차이가 있는가를 검정(data : response.csv)
+job변수 설명 1: 학생 2: 직장인 3: 주부
+response변수 설명 1: 무응답 2: 낮음 3: 높음

response <- read.csv('./data/response.csv',header = T)
head(response)
table(response$job)
table(response$response)
j <- response$job
r <- response$response
chisq.test(j,r)
##해석
1. 가설설정
H0 : 직업유형에 따른 응답 정도에 차이가 없다.
H1 : 직업유형에 따른 응답 정도에 차이가 있다.
2.기각역 설정
유의수준 : 0.05
3.계산 결과
검정통계량 : 58.208
자유도 : 4
p-val : 6.901e-12
4.기각 여부
p-val이 유의수준보다 작으므로 귀무가설 기각
즉, 직업유형에 따른 응답 정도에 차이가 있다.

################################################################8월13일
+모형이 통계적으로 유의미한가 -> 유의수준 5%이하에서 검정통계량 p-value가 0.05보다 작으면
+회귀계수들이 유의미한가 -> 해당 계수의 t통계량과 p-value로 파악
+모형이 얼마나 설명력을 갖는가 -> 결정계수 r^2값 확인
+잔차분석 5가지 가정을 만족하는가
  1) 선형성(독립변수의 변화에 따라 종속변수도 일정 크기로 변화)
  2) 독립성(잔차와 설명번수의 값이 독립적)
  3) 등분산성(설명변수의 모든 값에 대해 오차들의 분산이 일정)
  4) 비상관성(관측치들의 잔차들끼리 상관성이 없어야 한다.)
  5) 정상성(잔차항이 정규분포를 이뤄야 한다.)

+오차는 과정 속에 있는 추정값과 차이
+잔차는 선형그래프이 최종결정 후의 오차를 잔차라고 부른다.

상관분석 : 데이터 안의 변수 간의 관계를 알기 위해 수행
1.Pearson 상관계수
  -비율척도 또는 등간척도 변수들의 상관성 확인
  -해석 : 산점도가 직선에 가깝게 분포하면 상관계수의 절대값이 1에 가까워짐
          반면에 산점도가 넓게 퍼져있다면, 즉 직선의 관계가 희미할수록 상관계수는 0에 가까워짐

install.packages('acepack')
install.packages('Hmisc')
library(Hmisc)
library(dplyr)

#상관분석 수행 과정
drat <- mtcars$drat
disp <- mtcars$disp

str(drat)
str(disp)

mtcars %>%  head
mtcars %>%  str

Hmisc::rcorr(as.matrix(mtcars), type = 'pearson')
###rcorr은 "H0 : 상관계수는 0이다"에 대한 p-value를 보여줌

#공분산
cov(mtcars)

#corrgram
install.packages('corrgram')
library(corrgram)
corrgram(as.matrix(mtcars),upper.panel = panel.conf)

#Chickweight data
library(MASS)

#1단계
chick <- ChickWeight
head(chick)
#2단계 subsetting
ch1 <- chick %>%  filter(Diet == 1 & Chick == 1)
ch1
#3단계 simple linear regression
ch.lm <- lm(formula = weight~Time,data = ch1)
summary(ch.lm)

#4단계 해석
F통계량의 p-value 2.0~~~e-0.8으로 유의수준 0.05기준 매우낮아 모형은 통계적으로 유으ㅟ하다.
설명변수 Time의 t통계량에 대한 p-value 2.97e-09으로 회귀계수 역시 통계적으로 유의하다
결정계수 r^2 0.9588로 1에 매우 가까우며, 모형의 설명력이 매우강함

#절편을 제거하는 회구식
ch.nointer <- lm(weight ~ -1 + Time,data = ch1)

#prodcut파일 불러오기
pro <- read.csv('./data/product.csv')
str(pro)

y <- pro$제품_만족도

x <- pro$제품_적절성
df <- data.frame(x,y)
str(df)
#회귀모델 생성
result.lm <- lm(y~x,data = df)
#회귀분석의 절편과 기울기
result.lm # 회귀계수
#모델의 적합값과 잔차 보기
names(result.lm)
fitted.values(result.lm)
fitted.values(result.lm)[1:2]
head(df,1) #X =4 ,Y = 3
Y = 0.7789 + 0.7393 * 4
Y

#모델의 잔차 뽑아내기
fitted.values(result.lm)[1:2]
result.lm[1:2]
residuals(result.lm)[1:2]

#실습
1)선형회귀 분석 모델 시각화
plot(y~x,data = df)
2)회귀분석
result.lm <- lm(y~x,data = df)
3)회귀선
abline(result.lm, col='red')
4)분석 결과 보기
summary(result.lm)
str(summary(result.lm))
plot(summary(result.lm)$residuals)

#2. 다중회귀분석
+여러개의 독립변수가 종속변수에 미치는 영향 분석
H1 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 영향을 미친다.

prod <- read.csv('./data/product.csv',header = T)

y <- prod$제품_만족도
x1 <- prod$제품_적절성
x2 <- prod$제품_친밀도
df <- data.frame(x1,x2,y)
View(df)

re.lm <- lm(y~x1+x2,data = df)

#계수확인
re.lm #y절편 0.66731 
summary(re.lm)

#분산팽창 요인
install.packages('car') #vif() 패키지 설치
library(car)

#다중공선성 확인
+통계학의 회귀분석에서 독립변수들 간에 강한 상관관계가 나타나는 문제이다.
vif(re.lm) #통상적으로 4보다 크면 문제가 있다고 판단
sqrt(vif(re.lm)) >2 #문제 없음 / 모델을 사용해도 되는 지에 대한 고민 루트를 씌었기때문에 2보다 크면 문제 있음 

#[실습]
data(iris)

1) 회귀 모델 생성
form <- iris$Sepal.Length~Sepal.Width+Petal.Length
model <- lm(form,data=iris)
model
summary(model)

2) 잔차 분석 -> 더비왓슨(자기 상관성)
install.packages('lmtest')
library(lmtest)
dwtest(model) #더빈왓슨값(통상 1~3사이) ;귀무가설 : 자기상관은 없다.

3)잔차도 확인 검정
par(mfrow=c(2,2))

3-1)선형성(빨간 실선이 -에 가까운 수평선) &독립성(특정한 모여있는 패턴이 발견되지 않음)
plot(model, which = 1)
3-2)정규성(직선에 가깝게 잘 모여있음)
plot(model, which = 2)
3-3)등분산성 &독립성(적절하게 퍼져 있음, 특정패턴없음)
plot(model, which =3)
3-4)극단치
plot(model, which = 4)

4)잔차 정규성 검정
attributes(model)  #coefficients(계수), residual(잔차), fitted.values(적합값)
res <- model$residuals
shapiro.test(model$residuals) #W = 0.99394, p-value = 0.7856 >= 0.05
                              #귀무가설 : 정규성과 차이가 없다.
5)정규성 시각화
head(res,20)
length(res)
hist(model$residuals,freq = F,ylim=c(0,1.4));lines(density(model$residuals))
qqnorm(model$residuals)
plot(model,2)

#다중공선성 검사 예시
x1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
y <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)

df <- data.frame(x1,x2,x3,x4,y)
a <- lm(y~.,data=df)
str(df)
summary(a)

a1 <- lm(y~x1+x2+x4,data=df)
summary(a1)

a2 <- lm(y~x1+x3, data = df)
summary(a2)

aa <- step(lm(y~x1+x2+x3+x4, data = df), 
           scope = list(lower=~1, upper = ~x1+x2+x3+x4)) #자동으로 변수를 소거
summary(aa)
aa

install.packages('mlbench')
library(mlbench)
data("BostonHousing2")
head(BostonHousing2)
housing <- lm(medv~.,data=BostonHousing2)
summary(housing)#summary 결과 indus, age 는  p 값이 매우 높아 유효하지 않은 변수로 판단되나,
                #변수를 그대로 유지한채, 소거법 적용
ho1 <- step(housing,direction = c('both'))
ho1
summary(ho1)#소거법 결과 : 3번째 스텝에서 indus와 age를 제외한 결과 확인 가능.
            #housing2에 미리 indus 와 age를 제외하고 회구한 결과,
            #첫번째 스텝에서 종료되며, housing1의 결과와 일치한다.
ho2 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+b+lstat+tax,data = BostonHousing2)
ho3 <- step(ho2,direction = 'both')

###############################################################################
#연습문제 1
1. HDTV 판매율을 높이기 위해 프로모션 진행한 결과 기존 구매비율 15% 보다 
향상되었는가? (data : hdtv.csv)
hdtv <- read.csv('./data/hdtv.csv',header = T)
head(hdtv)
summary(hdtv)
hdtv$buy2[hdtv$buy == 1] <- 'no'
hdtv$buy2[hdtv$buy == 2] <- 'yes'
hdtv$buy2
table(hdtv$buy2)

#이항분포 비율검정 :
50명의 고객을 대상으로 프로모션 후 15% 구매비율이 향상되었는지를 검정
install.packages('prettyR')
library(prettyR)
freq(hdtv$buy2)
binom.test(c(10,40),p=0.15)
#해석
p-val > 0.05 이므로 '프로모션 후 15%'구매비율의 향상은 없었다.
귀무가설을 채택한다
###############################################################################
#연습문제2
우리나라 전체 중학교 2학년 여학생 평균키가 148.5cm이다. A 중학교 2학년 전체 500명 중
10%인 50명을 선정하여 평균신장을 계산하고 평균 신장에 차이가 있는지 규명(data : student_height.csv)

sth <- read.csv('./data/student_height.csv', header = T)
head(sth)
summary(sth) #결측치 확인
height <-sth$height
mean(height)

1)정규분포 검정
shapiro.test(height)
hist(height)
qqnorm(height)
qqline(height,lty=1,col='red')
#W = 0.88711, p-value = 0.0001853 < 0.05이므로 정규분포를 따르지 않는다.
#H0 : 정규분포를 따른다.
wilcox.test(height, mu = 148.5,alternative = 'two.sided',conf.level = 0.95)
#해석
#H0 : 평균키의 차이는 없다.
p-value > 0.05이므로 표본의 평균이 한국 중학교 2학년 여학생 평균 신장 크기와
차이가 없다는 귀무가설을 채택한다.

###############################################################################
#연습문제3
3. 대학 진학한 남학생과 여학생 대상으로 학교에 대한 만족도에 차이가 있는지 검정(data : sample.csv)
성별 1: 남 ,2;여 survey 0 : 불만족, 1 : 만족

sur <- read.csv('./data/sample.csv',header = T)
head(sur)
summary(sur)
x <- sur$gender
y <- sur$survey
table(x)
table(y)
table(x,y)

#비율차이검정
prop.test(c(138,107),c(174,126),
          alternative = 'two.sided',conf.level = 0.95)
#해석
p-value > 0.05이므로 귀무가설을 채택한다. 즉, 만족도에는 차이가 없다.

##############################################################################
#연습문제4
4. 두가지 다른 교육방법에 대한 시험성적에 차이가 있는지 검정(method.csv)
met <- read.csv('./data/method.csv',header = T)
head(met)
summary(met)
met1 <- subset(met,!is.na(score),c(method,score))
summary(met1)

a <- subset(met1,method ==1)
b <- subset(met1,method == 2)
a1 <- a$score
b1 <- b$score

length(a1)
length(b1)
mean(a1)
mean(b1)
var.test(a1,b1) #동분산성 검정
t.test(a1,b1,alternative = 'two.sided',conf.level = 0.95)

#해석
p-value=1.303e-06 < 0.05이므로 귀무가설을 기각한다. 즉, 교육방법에 대한 시험성적에
차이가 있다.







