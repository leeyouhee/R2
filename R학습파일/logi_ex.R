### 예제

## 1. 종속변수의 범주가 2개로 하기 위해 iris 데이터의 일부분만 이용
## --> 종속변수는 setosa, versicolor 2개로 이항분류 적용, 독립변수는 sepal.length만 이용

# 1) 데이터 로딩 및 서브셋
setwd('C:/Users/09/Desktop/R/R_study/data')
data('iris')
summary(iris)
View(iris)
set.seed(11)
df <- iris[,-c(2,3,4)]
library(dplyr)
View(df)
y <- df %>% 
  filter(df$Species == 'setosa' | df$Species == 'versicolor')
y
View(y)
str(y)
# 2) factor 변경
y$Species <- as.character(y$Species)
y$Species[y$Species == 'setosa'] <-1
y$Species[y$Species == 'versicolor'] <-0
y$Species <- as.numeric(y$Species)
head(y)
y
table(y)
# 3) glm 함수로 모델 생성(family = binomial)
idx <- sample(1:nrow(y),nrow(y)*0.7)
idx
length(idx)
train <- y[idx,]
test <- y[-idx,]
# 4) 적합된 모델 요약 정보 보기
y_md <- glm(Species ~.,data=train, family = 'binomial') #glm(종속변수~독립변수, data 종류 , 종속변수가 따른 분포)
str(glm)
summary(y_md)
train
# 5) 적합결과 보기
fitted.values(y_md)
str(y_md)
y_md[3]
# 6) predict 위해 편의상 모형 구축에 사용된 데이터, 1,50,51,100 사용
test <- y[c(1,50,51,100),]
test
# 7) predic()로 예측 실시
pred <- predict(y_md,newdata = test, type = 'response' )
pred
summary(pred)
# 8) 0.5보다 작으면 1(setosa), 아니면 2(versicolor)로 리코딩
result_pred <- ifelse(pred >= 0.5,1,2)
length(result_pred)
# 9) 예측결과와 테스트데이터 caret 패키지의 confusionMatrix()로 비교
str(y$Species)
table(result_pred,test$Species)
install.packages('caret')
library(caret)
confusionMatrix()
# 10) 7:3 분할하여 모델 적합 및 예측 재실시
idx <- sample(1:nrow(y),nrow(y)*0.7)
idx
length(idx)
train <- y[idx,]
test <- y[-idx,]

# 11) 모델 적합하기
y_md1 <- glm(Species ~.,data=train, family = 'binomial')
summary(y_md1)

# 12) 테스트 데이터 이용 예측
pred1 <- predict(y_md1,newdata = test,type = 'response')
pred1
summary(pred1)
nrow(test)
# 13) 0.5보다 작으면 1(setosa), 아니면 2(versicolor)로 리코딩
result_pred1 <- ifelse(pred1>=0.5,1,0)
length(result_pred1)
# 14) 정확도 비교
install.packages('e1071')
library(e1071)
table(result_pred1,test$Species)
result_pred1 <- as.factor(result_pred1)
test$Species <- as.factor(test$Species)
confusionMatrix(result_pred1,test$Species)
str(result_pred1)
result_pred1
str(test$Species)