
## 1. 종속변수의 범주가 2개로 하기 위해 iris 데이터의 일부분만 이용
## --> 종속변수는 setosa, versicolor 2개로 이항분류 적용, 독립변수는 sepal.length만 이용
## -->

# 1) 데이터 로딩 및 서브셋

data(iris)
a <- subset(iris, Species == 'setosa' | Species =='versicolor')
table(a$Species)

# 2) factor 변경

a$Species <- as.character(a$Species)
str(a)
a$Species <- as.factor(a$Species)
table(a$Species)

# 3) glm 함수로 모델 생성(family = binomial)

b <- glm(formula = Species ~ Sepal.Length, family = 'binomial', data = a)

# 4) 적합된 모델 요약 정보 보기
summary(b)
str(summary(b))

# 5) 적합결과 보기
options(scipen = 100)
fitted(b)[c(1:5,96:100)]

# 6) predict 위해 편의상 모형 구축에 사용된 데이터, 1,50,51,100 사용

newdata <- a[c(1,50,51,100),]

# 7) predic()로 예측 실시

c <- predict(b, newdata = newdata, type='response')

# 8) 0.5보다 작으면 1(setosa), 아니면 2(versicolor)로 리코딩

result <- ifelse(c <= 0.5, 'setosa', 'versicolor')

# 9) 예측결과와 테스트데이터 caret 패키지의 confusionMatrix()로 비교

d <- newdata[,5]
table(d,result)

# install.packages('caret')
# install.packages('e1071')
# library(caret)
# library(e1071)
confusionMatrix(d,as.factor(result)) #error code 중 level이 같아야한다면, 두 변수가 factor로 되어있는지 확인!


# 10) 7:3 분할하여 모델 적합 및 예측 실시
#idx <- sample(1:nrow(y),nrow(y)*0.7) factor별로 불균등하게 샘플링됨. 즉, 데이터 외곡이 발생
length(a$Species)
idx <- createDataPartition(iris$Species, p = 0.7, list = F) #caret::createDattaPartition()
idx
tra <- a[idx,]
tes <- a[-idx,]
tes
length(tra$Species);length(tes$Species)

# 11) 모델 적합하기

bb <- glm(Species ~ Sepal.Length, family = 'binomial', data = tra)
summary(bb)

# 12) 테스트 데이터 이용 예측

cc <- predict(bb, newdata = tes, type = 'response')
cc

# 13) 0.5보다 작으면 1(setosa), 아니면 2(versicolor)로 리코딩

result1 <- ifelse(cc <= 0.5, 'setosa', 'versicolor')
result1

# 14) 정확도 비교

dd <- tes[,5]
dd <- as.character(dd)
dd <- as.factor(dd)

table(dd,result1)

pairs(iris)

str(dd)
str(as.factor(result1))

caret::confusionMatrix(dd, as.factor(result1))

# dd <- as.character(dd)
# dd <- as.factor(dd)

########## 예제 mtcars ##########

# 1) 데이터 준비

mt <- mtcars
nrow(mt)

# 2) 8:2 데이터 나누기

idx <- sample(1:nrow(mt), 0.8*nrow(mt))
mt_train <- mt[idx,]
mt_test <- mt[-idx,]
head(mt_train)


# 3) 종속변수 vs, 독립변수 mpg + am로 모형 적합

glm.mt <- glm(formula = vs ~ mpg + am, family = 'binomial',
              data = mt_train)
summary(glm.mt)
glm.mt$fitted.values

# 4) 예측

glm.pr <- predict(glm.mt, newdata = mt_test, type = 'response')

# 5) 변수 선택

step.mt <- step(glm.mt, direction = 'backward')

# 6) 결과 비교

glm.pr
glm.mt.result <- ifelse(glm.pr <= 0.5, 0, 1)

table(mt_test$vs, glm.mt.result)

data.frame(orig = mt_test$vs, pred = glm.mt.result)