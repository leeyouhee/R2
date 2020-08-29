#중심극한 정리
N <- seq(0,9,1)
N
X <- sample(N,5, replace = T)
mean_x <- mean(X)
mean_x


list <- c()
for(i in 1:10000){
  X <- sample(N,5, replace = T)
  mean_x <- mean(X)
  list <- append(list,mean_x)
}
list

hist(list)

##강사님
a <- 0:9
rand_data <- list(sample(a,5,replace = T))# 표본의 크기가 5인 list 생성
rand_data
set.seed(500) ; for(i in 1:100){ # 100개 생성, set.seed 샘플링의 값이 바뀌지 않게, 난수 고정!
  rand_data[i] <- list(sample(a,5,replace = T))
}

head(rand_data)
rand_data
#5개의 값이 각 표본에 저장되도록 매트릭스 생성
rand_data1 <- matrix(unlist(rand_data), nrow = 100, byrow = T)
head(rand_data1)
#df 변경
rand_data2 <- data.frame(rand_data1)
head(rand_data2)
#각 랜덤표본의 합 및 평균 산출
rand_data2$sum <- apply(rand_data2,1,sum)
head(rand_data2)
library(dplyr)
rand_data2 <- rand_data2 %>%  mutate(mean = sum/5)
head(rand_data2)
hist(rand_data2$mean, labels = T, freq = F, ylim = c(0,0.35))
lines(density(rand_data2$mean))

#가설검정
##1.1 교차분석
범주형 자료를 대상으로 두개 이상의 변수들에 대한 관련성을 알아보기 위해
결합분포를 나타내는 교차분할표를 작성하고 이를 통해서 변수 상호 간의 관련성 여부를 분석하는
방법이다.

###1데이터 프레임 생성
data1 <- read.csv('./data/cleanDescriptive.csv',header = T)
###변수 확인
head(data1)
###2 변수 리코딩
x <- data1$level2
y <- data1$pass2
###부모학력수준(x) -> 자녀대학진학여부(y)
x;y 
###3 데이터프레임 생성
result<- data.frame(Lv = x, Pass = y)
###차원보기
dim(result)
head(result)

##1.2 교차분석
#install.packages('gmodels')
library(gmodels)
library(ggplot2)

###1 교차분할표 작성 및 테이블 만들기
gmodels::CrossTable(result$Lv, result$Pass)
table(result)

## 2.1 카이제곱 검정
카이제곱 검정은 범주(category)별로 관측빈도와 기대빈도의 차이를 통해서 확률 모형이
데이터를 얼마나 잘 설명하는지를 검정하는 통계방법
 +일원 카이제곱 검정
  - 교차분할표 이용 x, 한개의 변인(집단 또는 범주)을 대상으로 검정 수행
  - 관찰도수와 기대도수가 일치하는지 검정 -> 적합도 검정
  - 예 : 주사위 게임, 선호도 분석
 +이원 카이제곱 검정
  - 교차분할표 이용, 두개 이상의 변인 대상
  - 독립성 검정 : 한 집단 내에서 두 변인의 관계가 독립인지 검정
    -> 귀무가설 : 둘은 관계가 없다.
  - 동질성 검정 : 두 집단 이상에서 각 범주 간의 비율이 서로 동일한지를 검정
    -> 귀무가설 : 모든 표본들의 비율은 동일하다.


### 2.1.1 주사위 게임
귀무가설 : 기대치와 관찰치는 차이가 없다. : p >= a
대립가설 : 기대치와 관찰치는 차이가 있다. : p < a

#60회 주사위를 던져서 나온 관측도수와 기대도수
관측도수 : 4,6,17,16,8,9
기대도수 : 10,10,10,10,10,10
chisq.test(c(4,6,17,16,8,9)) #알아서 기대도수를 구해줌

########## 3) 이원 카이제곱 검정

############# 3-1) 독립성 검정(두개 이상의 변인/두 변인 사이 관계 있다? 없다?)

# 연구 가설 : 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 있다.
# 귀무 가설 : 부모의 학력 수준과 자녀의 대학진학 여부는 관련성이 없다.

# 독립 변수 (x) = 설명변수, 종속 변수(y) = 반응 변수 생성

data1 <-read.csv('C:\\rsource\\cleanDescriptive.csv',header = T)
str(data1)

x <- data1$level2    #부모의 학력수준
y <- data1$pass2     #자녀의 대학 진학 여부

CrossTable(x,y,chisq = T)  #p = 0.2507057


##1. 단일집단 검정
###1.1 단일집단 비율검정
  단일집단의 비율이 어떤 특정한 값과 같은지를 검정하는 방법
  데이터 준비 -> 전처리 -> 기술통계 -> binom.test() -> 검정통계량 분석

<실습>
  H1 : 기존 2020년도 고객 불만률과 2019년 CS 교육 후 불만률에 차이가 없다.
  H0 : 기존 2020년도 고객 불만률과 2019년 CS 교육 후 불만률에 차이가 있다.
  
##단계1. 실습데이터 가져오기
data <- read.csv('./data/one_sample.csv', header =T)
head(data)
x <- data$survey
##단계2. 빈도수와 비율 계산
summary(x) #결측치 확인
length(x)
table(x) #불만족 14, 만족 136
#만족율 기준 비율 검정
#단계1. 양측검정 기존 80% 만족율 기준을 유의수준 0.05로 검사
binom.test(c(136,14),p=0.8, alternative = 'two.sided', conf.level = 0.95)

#H1 : 2020년도 CS교육 후 만족도가 더 크다
#H0 : 2020년도 CS교육 후 만족도가 더 크지 않다.
#단계2. 방향성이 있는 단측가설 검정
binom.test(c(136,14),p=0.8, alternative = 'greater', conf.level = 0.95)

#####해석
2020년 cs 교육 후 만족도 조사를 실시한 결과 150명 중 136명이 만족하였고 기존의 
80% 만족도보다 높다는 비율검정을 실시한 결과 p-value는 0.0003179로 유의수준 5%보다 매우 
낮으므로 cs 교육 후 만족도가 높지 않다는 귀무가설을 기각하고 대립가설을 채택한다.
따라서, 고객만족도는 기존보다 높다


###1.2 단일집단 평균검정(단일표본 T 검정)
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

a <- read.csv('./data/one_sample.csv', header =  T)
head(a)
x <- a$time
#단계 2. 데이터 분포/결측치 제거
summary(x)
mean(x)
#단계 3 : 데이터 정제 2가지 방법
mean(x, na.rm = T)
x1 <- na.omit(x)
mean(x1)
#단계 4 : 정규분포 검정
shapiro.test(x1) #정규분포 검정함수(p-val = 0.7242)

#단계 5: 정규분포 시각화
hist(x1)
qqnorm(x1)
qqline(x1, lty=1, col ='blue')
#단계 6 : 평균차이 검정
t.test(x1, mu=5.2)
result <- t.test(x1,mu=5.2,alternative = 'two.side',conf.level = 0.95)
result

result1 <-t.test(x1,mu=5.2,alternative = 'greater',conf.level = 0.95)
result1

###2.1 두 집단 비율검정
데이터 준비 -> 전처리 -. 두집단 subset 생성 -> prop.test() -> 검통량 분석
<실습>
  상황 : IT 교육센터에서 PT를 이용한 프리젠테이션 교육방법과 실시간 코딩 교육방법을
각각 적용하였다. 2가지 교육방법 중 더 효과적인 교육방법을 조사하기 위해서 교육생 각
150명을 대상으로 설문조사
H0 : 두가지 교육방법에 따라 규육생의 만족률 차이가 없다.
H1 ; 두가지 교육방법에 따라 교육생의 만족률 차이가 있다.

data <- read.csv('./data/two_sample.csv', header = T)
data
head(data)
#e단계 2. 두 집단 subset 작성 및 데이터 전처리
x <- data$method
x
y <- data$survey #만족 1, 불만족 0
y
#단계 3. 집단별 빈도분석
table(x)
table(y)
#단계4 두 변수에 대한 교차분석
table(x,y)
gmodels::CrossTable(x,y)
#두 집단 비율 차이 검증
prop.test(c(110,135), c(150,150))#방법1과 방법2 비율차이 검증증
prop.test(c(110,135), c(150,150),alternative = 'greater',conf.level = 0.95)

#단계 2. 방향성이 있는 단측 검정 : 방법1 > 방법2 : P-val = 0.9998 >0.05
prop.test(c(110,135), c(150,150),alternative = 'greater',conf.level = 0.95)


########################연습문제#################################
1. 교육수준과 흡연율이 관계가 있는지 검정을 실시해보자(data : smoke.csv)
+교육 1 : 대조 2: 고졸 3: 중졸
+흡연 1 : 과다 2: 보통 3: 비흡연
smoke <-read.csv('./data/smoke.csv', header =T)
head(smoke)
View(smoke)
x <- smoke$education
y <- smoke$smoking
CrossTable(x,y,chisq = T)

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


##################################################################
2. 나이(age3)와 직위(position)간의 관련성을 독립성 검정(data : cleanData.csv)
+age변수 설명 1: 청년층 2: 중년층 3: 장년층
+position 변수 설명 1: very high 2: high 3: mid 4: low 5: very low

clean <- read.csv('./data/cleanData.csv',header = T)
head(clean)
table(clean)
age <- clean$age3
pos <- clean$position

CrossTable(age,pos,chisq = T)
table(age)
table(pos)

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

#################################################################
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
View(j)
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

#####z카이제곱검정 설명 url
https://bioinformaticsandme.tistory.com/139