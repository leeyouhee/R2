모형이 통계적으로 유의미한가 -유의수준 5%하에서 F통계량의 p-value가 0.05보다 작으면
회귀계수들이 유의미한가?해당계수의 t통계량과 p-value
모형이 얼마나 설명력을 갖는가?결정계수 r^2값 확인(SSR/SST)
잔차분석 5가지 가정을 만족하는가

오차는 과정속에 있는 추정값과의 차이
잔차는 선형그래프의 최종결정 후의 오차를 잔차라고 부른다.

5가지 가정
선형성(독립변수의 변화에 따라 종속변수도 일정 크기로 변화)
독립성(잔차와 설명변수의 값이 독립적)
등분산성(설명변수의 모든 값에 대해 오차들의 분산이 일정)
비상관성(관측치들의 잔차들끼리 상관성이 없어야한다)
정상성(잔차항이 정규분포를 이뤄여야 한다.)

상관분석 : 데이터 안의 변수 간의 관계를 알압기 위함
1.Pearson() 상관계수
-비율척도 또는 동간처도 변수들의 상관성 확인
해석 : 산점도가 직선에 가깝게 분포하면 상관계수의 절대값이 1에 가까워짐-->강한 상관관계
산점도가 넓게 퍼짐 --> 직선의 관계가 희미할수록 상관계수는 0에 가까워짐

install.packages('acepack')
install.packages('Hmisc')
library(Hmisc)
library(dplyr)

drat <- mtcars$drat
disp <- mtcars$disp
#상관성분석2
mtcars %>%head
mtcars %>%  str

Hmisc::rcorr(as.matrix(mtcars), type = 'pearson') #데이터 matrix로 전달

###rcorr은 "H0 : 상관계수는 0이다"에 대한 p-value를 보여줌

#cf)공분산
cov(mtcars)

##corrgram
install.packages('corrgram')
library(corrgram)
corrgram(as.matrix(mtcars),upper.panel = panel.conf)

## PerformanceAnalytics
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
chart.Correlation(as.matrix(mtcars))

#1st ChickWeight data (MASS package)

library(MASS)

#1. MASS
chicken <- ChickWeight
head(치킨)

#2 subsetting -->diet==1 &chick = 1

chick <- chicken %>%  filter(Diet == 1 & Chick == 1)
chick

#3. simple linear regression
Chick.lm <-lm(formula= weight ~ Time, data = chick)

#4 checking result
summary(Chick.lm)

#5. interpretation
# #5-1) F통계량의 p-value 2.0~~~e-0.8으로 유의수준 0.05기준 매우낮아 모형은 통계적으로 유으ㅟ하다.
# 5.2) 설명변수 Time의 t통계량에 대한 p-value 2.97e-09으로 회귀계수 역시 통계적으로 유의하다
# 5.3 결정계수 r^2 0.9588로 1에 매우 가까우며, 모형의 설명력이 매우강함

#6. 절편을 제거하는 회귀식

chick.nointer <- lm(formula = weigth ~ -1 + Time, data = chick)


##product.csv

product <- read.csv("product.csv")
str(product)

y  <- product$제품_만족도
x  <- product$제품_적절성
df <- data.frame(x,y)

#회귀모델 생성
result.lm <- lm(y~x, data=df)

#회귀분석의 절편과 기울기
result.lm #회귀계수
#모델의 적합값과 잔차 보기
names(result.lm)
fitted.values(result.lm)
fitted.values(result.lm)[1:2]
head(df,1) #X =4 ,Y = 3
Y = 0.7789 + 0.7393 * 4
Y #3.7361

#잔차 : residual = 관측치 = 예측치
3-3.7361 # -0.7361

residuals(result.lm)[1:2] #모델의 잔차만 뽑아냄

#[실습]선형회귀 분석 모델 시각화(오류 확인)
#x,y 산점도 그리기
plot(formula = y ~ x, data=df)
#회귀분석
result.lm <- lm(formula = y~x,data=df)
#회귀선
abline(result.lm, col='red')

#실습 선형회귀 분석 결과 보기
summary(result.lm)

str(summary(result.lm))

plot(summary(result.lm)$residuals)

#2. 다중 회귀 분석
여러개의 독립변수 ->종속변수에 미치는 영향 분석
H1 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 정의 영향을 미친다.
제품의 적절성(x1), 제품의 친밀도(x2) -> 제품의 만족도(y)

product <- read.csv('product.csv', header = TRUE)

#1 적절성 +친밀도 = 만족도
y <- product$제품_만족도
x1 <- product$제품_적절성
x2 <- product$제품_친밀도
df <- data.frame(x1,x2,y)

result.lm <- lm(formula = y ~x1 +x2, data = df)

#계수 확인
result.lm
summary(result.lm)

#0.66731 y절편 

#분산팽창 요인
install.packages('car') #vif() 함수 패키지 설치
library(car)#로딩

#2 분산팽창요인(VIF) -->다중공선성 문제 확인
vif(result.lm) #통상적으로 4보다 크면 문제가 있다고 판
sqrt(vif(result.lm)) > 2 #FALSE FALSE 문제 없음 / 모델을 사용해도 되는 지에 대한 고민 루트를 씌었기때문에 2보다 크면 문제 있음 

#3. 다중공선성 문제 해결과 모델 성능평가
#[실습] 다중공선성 문제 확인
data(iris)

#1, 회귀모델 생성
# 변수 모델링
formula <- iris$Sepal.Length ~Sepal.Width + Petal.Length
#회귀모델 생성
model <- lm(formula = formula, data = iris)
model
summary(model)

#2 잔차 분석

#1 잔차 독립성 --> 더빈왓슨(자기 상관성)
install.packages('lmtest')
library(lmtest)
dwtest(model) #더빈왓슨값(통상 1~3사이) #귀무가설 : 자기상관은 없다.

#2. 잔차도 확인 검정

par(mfrow = c(2,2))
#선형성(빨간 실선이 -에 가까운 수평선) &독립성(특정한 모여있는 패턴이 발견되지 않음)
plot(model, which = 1)
#정규성(직선에 가깝게 잘 모여있음)
plot(model, which = 2)
#등분산성 &독립성(적절하게 퍼져 있음, 특정패턴없음)
plot(model, which =3)z
#극단치
plot(model, which = 4)

#3. 잔차 정규성 검정
attributes(model) #coefficients(계수), residual(잔차), fitted.values(적합값)
res <- model$residuals
shapiro.test(model$residuals) #W = 0.99394, p-value = 0.7856 >= 0.05

#귀무가설 : 정규성과 차이가 없다.

#정규성 시각화
head(res,20)
length(res)
hist(model$residuals, freq = F, ylim = c(0,1.4));lines(density(model$residuals))
qqnorm(model$residuals)
plot(model,2)

#4. 다중공선성 검사


x1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
y <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
df <- data.frame(x1,x2,x3,x4,y)
a <- lm(y~x1+x2+x3+x4, data = df)

summary(a)

a1 <- lm(y~x1+x2+x4, data = df)
summary(a1)

a2 <- lm(y~x1+x3, data = df)
summary(a2)

aa <- step(lm(y~x1+x2+x3+x4, data = df), 
           scope = list(lower=~1, upper = ~x1+x2+x3+x4)) #자동으로 변수를 소거

?step
summary(aa)
aa

install.packages('mlbench')
library(mlbench)
data('BostonHousing2')
summary(BostonHousing2)
종속 변수 medv (집값)
str(data1)
data1 <- BostonHousing2
data1.lm <-lm(formula= medv ~ lon, data = data1)
summary(data1.lm)

y <- BostonHousing2$medv
x1 <- BostonHousing2$town
x2 <- BostonHousing2$tract
x3 <- BostonHousing2$lon
x4 <- BostonHousing2$lat
x5 <- BostonHousing2$cmedv
x6 <- BostonHousing2$crim
x7 <- BostonHousing2$zn
x8 <- BostonHousing2$indus
x9 <- BostonHousing2$chas
x10 <- BostonHousing2$nox
x11 <- BostonHousing2$rm
x12 <- BostonHousing2$age
x13 <- BostonHousing2$dis
x14 <- BostonHousing2$rad
x15 <- BostonHousing2$tax
x16 <- BostonHousing2$ptratio
x17 <- BostonHousing2$b
x18 <- BostonHousing2$lstat
bh_df <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,y)
aa <- step(a, 
           scope = list(lower = ~1, upper = ~x1+x2+x3+x4+
                          x5+x6+x7+x8+x9+x10+x11+x12+x13+
                          x14+x15+x16+x17+x18),
           direction = 'both')
summary(aa)
a <- lm(y~x2+x4+x5+x14, data = bh_df)
summary(a)
    ##############################################################################
1. HDTV 판매율을 높이기 위해 프로모션 진행한 결과 기존 구매비율 15% 보다 
향상되었는가? (data : hdtv.csv)
hd_df <- read.csv("hdtv.csv", header = TRUE)
View(hd_df)
table(hd_df$buy)
prop.test(c(15,20), c(100,100), alternative = "greater", conf.level = 0.95)

2. 우리나라 전체 중학교 2학년 여학생 평균키가 148.5cm이다. A 중학교 2학년 전체 500명 중
10%인 50명을 선정하여 평균신장을 계산하고 평균 신장에 차이가 있는지 규명(data : student_height.csv)
sh_df <- read.csv("student_height.csv", header = TRUE)
View(sh_df)
h1 <- sh_df$height
t.test(h1, mu = 148.5)
result <- t.test(h1, mu = 148.5, alter = "two.side", conf.level = 0.95)
result


3. 대학 진학한 남학생과 여학생 대상으로 학교에 대한 만족도에 차이가 있는지 검정(data : sample.csv)
성별 1: 남 ,2;여 survey 0 : 불만족, 1 : 만족
sam_df <- read.csv("sample.csv", header = TRUE)
str(sam_df)
table(is.na(sam_df$survey))
sam_df_m <- sam_df %>% filter(gender == 1)
sam_df_f <- sam_df %>% filter(gender == 2)
table(sam_df_m$survey)
table(sam_df_f$survey)
prop.test(c(138,107), c(174,126), alternative = "two.sided", conf.level = 0.95)


4. 두가지 다른 교육방법에 대한 시험성적에 차이가 있는지 검정(method.csv)
mtd_df <- read.csv("method.csv", header = TRUE)
str(mtd_df)
table(is.na(mtd_df$score))
mtd_df_1 <- mtd_df %>% 
  filter(!is.na(mtd_df$score)&method ==1)
mtd_df_2 <- mtd_df %>% 
  filter(!is.na(mtd_df$score)&method ==2)

chisq.test(mtd_df$method, mtd_df$score)
t.test(mtd_df_1$score, mtd_df_2$score, alter = "two.side", conf.level = 0.95)


##############강사님 풀이
#1.
hdtv <- read.csv('./data/hdtv.csv', header =  T)
head(hdtv)
summary(hdtv)
#step1. 코딩변경
hdtv$buy2[hdtv$buy ==1] <- '구매안함'
hdtv$buy2[hdtv$buy ==2] <- '구매함'
hdtv$buy2
table(hdtv$buy2)

#이항분포 비율검정 :
#50명의 고객을 대상으로 프로모션후
#15% 구매비율이 향상 되었는지를 검정
install.packages('prettyR')
library(prettyR)
freq(hdtv$buy2)
binom.test(c(10,40),p=0.15)
# number of successes = 10, number of trials = 50, p-value = 0.321
# alternative hypothesis: true probability of success is not equal to 0.15
# 95 percent confidence interval:
...
#해석
p-val > 0.05 이므로 '프로모션 후 15%'구매비율의 향상은 없었다.
귀무가설을 채택한다.

#2.
stheight <- read.csv('./data/student_height.csv', header =  T)
head(stheight)
#결측치 확인
summary(stheight)
#표본ㅇ늬 평균 신장
height <- stheight$height
mean(height)
#정규분포 검정
shapiro.test(height)
hist(height)
qqnorm(height)
qqline(height,lty=1,col='red')
#W = 0.88711, p-value = 0.0001853 < 0.5 이므로 정규분포를 따르지 않아 wilcox test를 실시
help('wilcox.test')
wilcox.test(height, mu = 148.5,
            alternative = 'two.sided', conf.level = 0.95)
#해석
p-value > 0.05이므로 표본의 평균이 한국 중학교 2학년 여학생 평균 신장 크기와
차이가 없다는 귀무가설을 채택한다.

#3.
satis <- read.csv('./data/sample.csv', header = T)
head(satis)
#NA확인
summary(satis)
#두 집단 데이터 전처리
x <- satis$gender
y <- satis$survey
#집단별 빈도 분석
table(x)
table(y)
table(x,y)
#비율 차이 검정
prop.test(c(138,107),c(174,126),
          alternative = 'two.sided',conf.level = 0.95)

#4.
twomethd <- read.csv('./data/method.csv', header = T)
head(twomethd)
#subset
tmethod <- subset(twomethd,!is.na(score), c(method,score))
summary(tmethod)
#데이터 분리
a <- subset(tmethod, method ==1)
b <- subset(tmethod, method ==2)
a1 <- a$score
b1 <- b$score
length(a1)
length(b1)
mean(a1)
mean(b1)
#동질성 검정
var.test(a1,b1) #주집단의 분포형태는 동질함
#두 집단 평균 차이 검정
t.test(a1,b1,alternative = 'two.sided',conf.level = 0.95)

#보스턴하우징
library(mlbench)
data('BostonHousing')
head(BostonHousing)
housing <- lm(medv ~.,data = BostonHousing)
summary(housing)
#summary 결과 indus, age 는  p 값이 매우 높아 유효하지 않은 변수로 판단되나,
#변수를 그대로 유지한채, 소거법 적용
housing1 <- step(housing,direction =c('both'))
housing1
summary(housing1)
#소거법 결과 : 3번째 스텝에서 indus와 age를 제외한 결과 확인 가능.
#housing2에 미리 indus 와 age를 제외하고 회구한 결과,
#첫번째 스텝에서 종료되며, housing1의 결과와 일치한다.
housing2 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+b+lstat,data = BostonHousing)
housing3 <- step(housing2, direction = c('both'))
###Multicollinearity
install.packages('car')
library(car)
vif(housing2)#rad와 tax의 vif 값이 5보다 크다., 상관계수가 너무 높으면 회귀식을 방해한다.
#상관관계의 확인
#chas의 데이터 구조가 범주형이므로, 이를 제외한 나머지 변수들간의 상관관계 확인
#결과 : rad-tax 상관관계가 0.91로 매우 높아, tax 변수를 제외하기로 결정
y1 <- BostonHousing[c(1:3)]
y2 <- BostonHousing[c(5:14)]
y3 <- data.frame(y1,y2)
cor(y3)
y3
housing4 <- BostonHousing[c(-3,-7,-10)]# 3,7,10번째 컬럼 제외외
head(housing4)
head(housing3) 
###재회구
housing5 <- lm(medv ~.,data =  housing4)
housing5
summary(housing5)

#잔차도 확인
par(mfrow=c(2,2))
#선행성(빨간 실선이 0에 가까운 수평선) & 독립성(특정한 모여있는 패턴이 발견되지 않음)
plot(housing5,which = 1)
#정규성(직선에 가깝게 잘 모여있음)
plot(housing5,which = 2)
#등분산성 & 독립성(적절하게 퍼져 있음, 특정 패턴 없음)
plot(housing5,which =3)
#극단치
plot(housing5, which = 4)

##자기상관성 확인
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치
dwtest(housing5)
summary(housing5)
length(housing5$residuals)
housing5$residuals[c(369,372,373)]
housing5$residuals[c(368,370,371,374)]
boston <- BostonHousing
boston[c(368:374),]
boston2 <- boston[-c(369,372,373),]

#remodeling
hosing44 <- boston2[c(-3,-7,-10)]
hosing55 <- lm(medv ~.,data = hosing44)
summary(hosing55)
dwtest(hosing55)
par(mfrow=c(2,2))
plot(hosing55,which=1)
plot(hosing55,which=2)
plot(hosing55,which=3)
plot(hosing55,which=4)
##











