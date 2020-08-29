install.packages('rpart')
install.packages('rpart.plot')
require(rpart.plot)
require(rpart)
library(caret)
head(iris,51)

# 데이터 준비
idx <- createDataPartition(iris$Species, p =0.7,list = F)
train <- iris[idx,]
test <- iris[-idx,]

# 데이터확인
table(train$Species)

#######1. rpart #######

##모델 적합
m.rpart <- rpart(Species ~.,data = train)
m.rpart
root 105 70 setosa (0.33333333 0.33333333 0.33333333)  #맨위에 105개에서 시작하여 1/3씩 나뉨
2) Petal.Length< 2.45 35  0 setosa (1.00000000 0.00000000 0.00000000) * #터미널노드 끝났다를 의미
  3) Petal.Length>=2.45 70 35 versicolor (0.00000000 0.50000000 0.50000000)  
6) Petal.Width< 1.65 38  3 versicolor (0.00000000 0.92105263 0.07894737) *
  7) Petal.Width>=1.65 32  0 virginica (0.00000000 0.00000000 1.00000000) *
summary(m.rpart)

##plot
plot(m.rpart,margin = .2, compress = T)
text(m.rpart,cex=1.0)

##plot2
rpart.plot(m.rpart, type = 4, extra = 2,digits = 3)
prp(m.rpart, type = 4, extra = 2, digits = 3)


##예측
m.rpart.pr <- predict(m.rpart, newdata =  test, type = 'class')
m.rpart.pr
summary(m.rpart.pr)

## 정확도 평가
table(test$Species,m.rpart.pr)
confusionMatrix(m.rpart.pr,test$Species)

##CP 확인
summary(m.rpart)
#결과###
CP nsplit  rel error     xerror       xstd
1 0.5000000      0 1.00000000 1.25714286 0.05392288
2 0.4571429      1 0.50000000 0.94285714 0.07073129
3 0.0100000      2 0.04285714 0.05714286 0.02802193
########
m.rpart$cptable
m.rpart$cptable[2]

##prunning(가지치기)(사후) 사후 가지치기를 하는 이유는 overfitting의 문제를 제거하기 위해
m.rpart.prune <- rpart::prune(m.rpart,cp = m.rpart$cptable[2]) #cp를 기준으로 짜르겠다, 가지치기 하겠다

#visulizatiopn
par(mfrow=c(1,2))
rpart.plot(m.rpart.prune,type=4,extra = 2,digits = 3)
rpart.plot(m.rpart,type = 4,extra = 2, digits = 3)

#party패키지
install.packages('party')
require(party)
##모델적합
m.ctree <- ctree(Species ~., data = train)
m.ctree

##plot
plot(m.ctree)
##예측
m.ctree.pr <- predict(m.ctree,newdata=test)
summary(m.ctree.pr)
##정확도확인
table(test$Species,m.ctree.pr)
confusionMatrix(test$Species,m.ctree.pr)
##과적합 방지 #사전가지치기
con <- ctree_control(maxdepth =2)#maxdepth = n , n층에서 자르기
m.ctree.prn <- ctree(Species~.,data = train,controls = con)
plot(m.ctree.prn)

##tree패키지
install.packages('tree')
library(tree)
m.tree <- tree(Species~.,data=train)
m.tree
##plot
plot(m.tree)
text(m.tree)
##예측
m.tree.pr <- predict(m.tree, newdata = test, type = 'class')
m.tree.pr
##정확도확인
table(test$Species, m.tree.pr)
confusionMatrix(test$Species,m.tree.pr)

##Random Forest
install.packages('adabag')
library(adabag)
library(caret)
library(e1071)

##1. bagging
### bootstrap + aggregating
### 원데이터로부터 크기가 같은 표본을 여러번
### 단순임의 복원추출하여 각 표본에 대한 분류
### 생성 후 앙상블

#data load
data(iris)
#bagging
?bagging
iris.bagging <- adabag::bagging(Species~.,
                                data = iris,mfinal = 1000) #나무의 개수는 1000개
iris.bagging
#feature importance
iris.bagging$importance

# visualization
par(mfrow=c(1,2))
plot(iris.bagging$trees[[1]])
text(iris.bagging$trees[[1]])
plot(iris.bagging$trees[[11]])
text(iris.bagging$trees[[11]])

#prediction
pred1 <- predict(iris.bagging,newdata = iris[,-5])
#model accuracy
table(pred1$class)
caret::confusionMatrix(as.factor(pred1$class),as.factor(iris[,5]))
#looking into model
summary(iris.bagging)
str(iris.bagging)
iris.bagging$votes
iris.bagging$samples[,1]

#checking sample data
sort(iris.bagging$samples[,1])

bg_sample1 <- table(sort(iris.bagging$samples[,1]))
attributes(bg_sample1)$dimnames[[1]]
bg_sample1 <- data.frame((idx=as.numeric(attributes(bg_sample1)$dimnames[[1]]),
                          freq = bg_sample1))
str(bg_sample1)
bg_sample1

#배깅에 랜덤을 추가, 붓스트램 샙플에 대해 모든 컬럼을 
#대상으로 분기를 검토하지 않고,
#사용할 feature를 임의로 추출하여 추출된  feature 중에
##최적의 분할을 만ㄷ르어낸다

install.packages('randomForest')
library(randomForest)
#splitting data
idx <- createDataPartition(iris$Species,p=0.7,list=F)
iris.train <- iris[idx,]
iris.test <- iris[-idx,]

#bootsting1
set.seed(1839)
iris.rf <- randomForest(Species ~., data = iris.train,
                        ntree =100, mtry = sqrt(4), importance =T)

iris.rf

# prediction
pred_rf <- predict(iris.rf, newdata = iris.test[, -5])
pred_rf

# model accuracy
table(pred_rf)
caret::confusionMatrix(as.factor(pred_rf), as.factor(iris.test[,5]))

#importance
importance(iris.rf)

# importance(분산값 이용)
varImpPlot(iris.rf, main = "varImpPlot of iris")

#예제
ggplot2의 diamonds 데이터셋을 활용하여,
decision tree와 앙상블 모형을 적용,
모델링을 수행하고 정확도를 비교해보세요 
(단, price 컬럼을 다음과 같이 명목형으로 변경하여 수행하세요)
(3분위(75%)이상이면 S등급, 2분위(50%)이상이면 A, 1분위수(25%)이상이면 B등급, 25%이하는 C등급)

library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)

ggplot2::diamonds %>%  head(2)

#import the dataset
str(diamonds)
dia <- diamonds
head(dia)
str(dia)
summary(dia)
#Convert the variables to numerical , level이 있는 factor형이기에 따로 숫자를 줄 필요 없다
dia$cut <- as.integer(dia$cut)
dia$color <- as.integer(dia$color)
dia$clarity <- as.integer(dia$clarity)

head(dia)
table(dia$price) #종속변수

# Creat features and target
x <- dia %>% 
  select(carat,depth,table,x,y,z,clarity,cut,color)
y <- dia$price

dia$level <- ifelse(dia$price>=quantile(y,0.75),"s",
                    ifelse(dia$price>=quantile(y,0.50),'a',
                           ifelse(dia$price>=quantile(y,0.25),'b','c')))
table(dia$level)
dia$level <- as.factor(dia$level)

#Split data into training and test sets
idx <- createDataPartition(dia$level,p=0.75,list = F)
dia2 <- dia[,-7] #price 컬럼 제거
head(dia,1)
head(dia2,1)

dia.train <- dia2[idx,]
dia.test <- dia2[-idx,]
length(names(dia.train))

#Train the model
set.seed(1949500);dia_rf <- randomForest(level~.,data = dia.train,ntree =100,
                                         mtry=floor(sqrt(length(names(dia.train)))))
set.seed(1949501); dia_rf5 <- randomForest(level~.,data = dia.train,ntree =500,
                                           mtry=floor(sqrt(length(names(dia.train))))) #mtry는 루트로 했을때 가장 효율적이다.

# Make prediction
predictions <- predict(dia_rf,dia.test)
predictions5 <- predict(dia_rf5,dia.test)

# Modelling assessment
confusionMatrix(as.factor(predictions),as.factor(dia.test$level))
confusionMatrix(as.factor(predictions5),as.factor(dia.test$level))

plot(dia_rf)


####Decision Tree
library(rpart)
dia.rpart <- rpart(level ~.,data = dia.train)
#prediction
dia.rpart.pr <- predict(dia.rpart, newdata = dia.test, type = 'class')
dia.rpart.pr

confusionMatrix(as.factor(dia.rpart.pr),dia.test$level)
table(dia.rpart$y == as.integer(dia.train$level))
table(dia.rpart.pr == dia.test$level)
confusionMatrix(as.factor(dia.rpart.pr),dia.test$level)

dia.rpart$y
















