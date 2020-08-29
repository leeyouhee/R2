###ensemble

##2. boosting
###배깅의 과정과 유사하나 재표본 과정에서 각 자료에
###동일한 확률을 부여하는 것이 아니라, 분류가 잘못된
###데이터에 더 큰 가중을 주어 표본을 추출
###붓스트랩 표본을 추출하여 classifier 생성 후
###분류결과를 이용하여 확률을 조정 --> 다음 표본 추출
###즉, 오차가 큰 나무가 있다면 다음에 오차가 더작은 나무를 만든다.
#boosting
iris.boosting <- adabag::boosting(Species~.,
                                  data = iris,
                                  boos = T,
                                  mfinal = 11)
#feature importance
iris.boosting$importance
#visualization
par(mfrow = c(1,2))
plot(iris.boosting$trees[[1]])
text(iris.boosting$trees[[1]])
plot(iris.boosting$trees[[11]])
text(iris.boosting$trees[[11]])

#prediction
pred_boosting <- predict(iris.boosting,newdata = iris[,-5])
pred_boosting
#model accuracy

table(pred_boosting$class)
caret::confusionMatrix(as.factor(pred_boosting$class),as.factor(iris[,5]))

#splitting data
?caret::createDataPartition
idx <- createDataPartition(iris$Species,p=0.7,list = F)
iris.train <- iris[idx,]
iris.test <- iris[-idx,]

#boosting1
iris.boosting1 <-adabag::boosting(Species~.,
                                  data = iris.train,
                                  boos = T,
                                  mfinal = 11)
iris.boosting1

#prediciton
pred_b1 <- predict(iris.boosting1,newdata = iris.test[,-5])

#model accuracy , train data에 대한 예측값과 실제 측정값의 정확도 -> 100%
table(iris.boosting1$class == iris.train$Species)
#test accuracy, test data에 대한 예측값과 실제 측정값의 정확도 -> 93.3%
table(pred_b1$class)
caret::confusionMatrix(as.factor(pred_b1$class),as.factor(iris.test[,5]))

++model accuracy와 test accuracy를 하는 이유는 과대적합을 보기 위함, 과소적합으로 가기위해서는 트리 개수를 줄인다.
#과소적합으로 가는 과정
#tuning
iris.b2 <- adabag::boosting(Species~.,
                            data= iris.train,
                            boos = T,
                            mfinal = 6) #나무 6개
pred_b2 <- predict(iris.b2,newdata = iris.test[,-5])
table(iris.b2$class == iris.train$Species)

table(iris.b2$class)
caret::confusionMatrix(as.factor(pred_b2$class),as.factor(iris.test[,5])) #정확도 93.33%

iris.b3 <- adabag::boosting(Species~.,
                            data= iris.train,
                            boos = T,
                            mfinal = 5) #나무 5개
pred_b3 <- predict(iris.b3,newdata = iris.test[,-5])
table(iris.b3$class == iris.train$Species)

table(iris.b3$class)
caret::confusionMatrix(as.factor(pred_b3$class),as.factor(iris.test[,5])) #정확도 97.78%

#boosting1001
해보쟈

#군집은 Y값을 모를때
# 군집과 거리
# 계층적 군집, 분할군집, 자기 조직화 지도 등이 있다.
# 거리에는 유클리디안, 맨하튼, 민코우스키, 마할라노비스 등이 있다.

#연습
install.packages('cluster')
library(cluster)
library(caret)

x <- matrix(1:9,nrow =3, by = T)
#계층적 군집
dist <- dist(x,method = 'euclidean') #컬럼간 거리 xxxx, 행간거리 oooo
dist
dist(x,method = 'euclidean')
s <- sum((x[1,]-x[2,])^2 )#유클리디안 거리 확인인
sqrt(s)

dist(x,method='minkowski',p=1)
dist(x,method='manhattan')

hc <- hclust(dist)
hc
plot(hc) #플롯 결과 2개의 그룹으로 나눌 수 있겠다고 파악 가능
#1.계층적 군집
## linkage methods
### 1) 최단연결법(single) : 두군집 사이의 관측값들 간의 거리 중 최소값
### 2) 최장연결법(complete) : 두 군집 사이의 관측값들 간의 거리 중 최대값
### 3) 중심연결법(centroid) : 두 군집의 중심 간의 거리
### 4) 평균연결법(average) : 모든 값들 간의 거리 평균 --> 계산량 많음
### 5) 와드연결법(ward) : 군집간의 거리를 이용하는 다른 연결과는 달리
                        # 군집내의 오차체곱합을 이용
                        # 두 군집이 병합하면 이전의 오차제곱합보다 커지는데 이 증가량이
                        # 작아지는 방향으로 군집을 형성
###https://ratsgo.github.io/machine%20learning/2017/04/18/HC/

#1. data load
data('USArrests')
#2. 거리측정
d <- dist(USArrests,method='euclidean')
d
#3. 계층적 군집 실시
#3-1)ward 연결
cw <- hclust(d, method = 'ward.D')
cw
#3-2)single 연결
cs <- hclust(d, method = 'single')
cs
#3-3)average 연결
ca <-hclust(d,method = 'average')
ca
#4. 덴드로그램 확인
#4-1)ward
plot(cw)
rect.hclust(cw, k =2, border = 'red') #k = n of goup
rect.hclust(cw, h=40,border=5:6,which = c(1:8))
#4-2)single
plot(cs)

#4-3)average
plot(ca)
rect.hclust(ca, k=6, border = 'green')
rect.hclust(ca,h=40,border = 5:6,which = c(1:8))

# 5. 높이나 그룹의 수로 군집 확인
# k = # of group,h = heights

grp1 <- cutree(ca,k=6)
grp1
str(grp1)
data.frame(attributes(grp1)$names[1:10],grp1[1:10])

cluster.df <- data.frame(states = attributes(grp1)$names, group = grp1)
View(cluster.df)

grp2 <- cutree(ca,h=40)
grp2

#2.비계층적 군집
# 원하는 군집 수만큼 초기값을 지정 -> 데이터를 가까운
# 초기값에 할당하여 군집을 형성한 뒤, 군집들의 평균을
# 재계산하여 초기값을 갱신

# process
# 1)k개의 군집을 임의 선택
# 2)각 데이터를 가까운 군집 중심에 할당
# 3)군집 내의 자료들의 평균을 계산하여 군집 중심을 갱신
# 4)군집 중심이 안정될 때까지 2)와 3)을 반복

#장접#
1) 알고리즘이 단순하며 빠름
2) 게층적 군집보다 맣은 데이터 빠르게 처리

#단점#
1)잡음이나 이상값에 영향을 많이 받음
2)비선형 데이터의 경우 성능이 떨어짐

#최적군집 수 검토
install.packages('NbClust')
install.packages('rattle')
library(NbClust)
library(rattle) #wine data
library(dplyr)

##data load
data(wine)
wines <- wine
wines %>% head
summary(wines)

#scaling(스케일 조정) : 상대적 거리를 알기위해!
+정규화(normalizing) : 평균과 표준편차로 조정 (x-평균/표준편차)
+표준화(standardizing) : min,max로 조정(x-최소값/최대-최소)
#normalizing : scale() --> 정규화 using standard deviation

wines.norm <- scale(wines[-1]) #각컬럼별로 스케일링(정규화)
wines.norm %>% head
summary(wines.norm)

#checking
wines1 <- wine
std_check <- sapply(wines1$Alchol,
                    function(x){(x-mean(wines1$Alcohol))/sqrt(var(wines1$Alcohol))})
std_check
std_check == wines.norm[,1]
plot(std_check)
#표준화
std_check1 <- sapply(wines1$Alcohol,
                     function(x){(x-min(wines1$Alcohol))/(max(wines1$Alcohol)-min(wines1$Alcohol))})
std_check1 == wines.std[,1]

plot(std_check1)
points(std_check1, col = 'blue')

#스케일 조정
std.preproc <- caret::preProcess(wines1,method=c('range'))
wines.std <- predict(std.preproc,wines1)
summary(wines.std)

#확인
wines.std$Alcohol == std_check1

#t스케일 조정2
norm.preproc <- caret::preProcess(wines1,method=c('center','scale'))
wines.norm2 <- predict(norm.preproc,wines1)
wines.norm[,1] == wines.norm2$Alcohol


#compute the number of clusters , 적절한 군집수를 추천하는 패키지
nc <- NbClust(wines.norm,max.nc = 15,method = 'kmeans')
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]),xlab = 'number of clusters',
        ylab = 'num of criteria', ylim=c(0,20))
dim(wines.norm)

#kmeans() modelling

cluster.kms <- kmeans(wines.norm,3) #최적의 군집수는 3개! ;; NbClust에서 추천한  군집수 3개
cluster.kms$size

군집이 잘 나뉘지 않을 데이터면 안좋음

#plotting
plot(wines.norm, col = cluster.kms$cluster)
points(cluster.kms$centers,col = 1:3, pch = 8, cex =1.5)

#metrics
confusionMatrix(as.factor(cluster.kms$cluster),as.factor(wines$Type))
cluster.kms$cluster
wines$Type
cluster.kms$cluster
wines

########################연습문제##########################################
1. interview.csv 파일을 활용하여 계층적 군집을 다음의 조건에 맞게 수행해보세요

- 데이터 첫번째 컬럼 삭제
- 와드연결법 사용하여 수행
- 3개의 군집으로 덴드로그램 그리기
- 군집결과와 실제 데이터의 합격여부와 비교검토

interview <- read.csv('./data/interview.csv',header = T)
interview
head(interview)

#no, 합격여부 컬럼 삭제
interview_df <- interview[-c(1,9)] #1번은 넘버링, 합격여부 삭제
head(interview_df,1)

#거리 생성
idist <- dist(interview_df,method = 'euclidean')

#계층적 군집 모델링
clustering.inter <- hclust(idist,method='ward.D')

#군집 시각화
plot(clustering.inter)
plot(clustering.inter,hang = -1) #음수값은 나오지 않아야 하므로 제거 속성 사용, 수평화
rect.hclust(clustering.inter,k=3,border = 'red')

#subsetting
grp_data <- cutree(clustering.inter,k=3)
grp_data
#결과 [1] 1 1 2 1 2 1 3 3 2 3 2 3 1 2 3 (군집번호)
group1 <- interview[which(grp_data == 1),]
group2 <- interview[which(grp_data == 2),]
group3 <- interview[which(grp_data == 3),]

#각 그룹의 요약 통계량 --> 합격여부를 보고, 각각의 차이를 파악할 수 있는 인사이트 도출
summary(group1);summary(group2);summary(group3)


















#########################################################################
2. ggplot2 diamonds 데이터셋 활용하여 다음의 조건에 맞게 비계층적 군집 수행

- price, carat, depth, table 4개의 컬럼 추출하여 dia1 생성 
- 위 작업 후 1000개의 데이터 랜덤샘플링하여 dia2 생성
- dia2 데이터 정규화 진행
- nbclust 이용 적정 k값 산정
- 비계층적 군집실시
- carat과 price에 대한 산포도 작성(plot() 의 컬러 인수에 군집결과$cluster를 할당하여 군집별 색상 구분)

#데이터 로드
library(ggplot2)
data(diamonds)

#데이터 샘플링 및 변수 선택
set.seed(1029);t <- sample(1:nrow(diamonds),1000)
dia9 <- diamonds[t,]
dia10 <- dia9[c('price','carat','depth','table')]

#정규화
dia.norm <- scale(dia10)
dia.norm

#nbclust k 값 산정
dia.k <- NbClust(dia.norm,max.nc = 15,method = 'kmeans')
table(dia.k$Best.nc[1,])

#비게층적 군집
dia.kms <- kmeans(dia.norm,2)

#plotting
plot(dia.norm[,2],dia.norm[,1],col = dia.kms$cluster)












