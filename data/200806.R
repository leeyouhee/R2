install.packages('mlbench')
library(mlbench)
data(Ozone)

Ozone
ozo <- Ozone

#1.plot : 기본은 산점도
plot(ozo$V8, ozo$V9)

#그래프 꾸미기
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch=20, cex=0.5)
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch='+', cex=0.7)

par(mfrow = c(1,2), #1x2 picture on the plot
    pty = 's')
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch=20, cex=0.5)
plot(ozo$V8, ozo$V9, xlab = 'Sandburg Temp',
     ylab = 'El Monte Temp', pch='+', cex=0.7)

#cars 데이터
cars #속도와 제동거리
head(cars)
plot(cars, type='1')
par(mfrow = c(1,2),
    pty = 's')
plot(cars,type = 'b')
plot(cars, type = 'o')

#지터 jitter() : 데이터에 약간의 노이즈를 추가함

tail(ozo[,c('V6','V7')],10)

#동일한 값을 가지는 데이터가 많을 경우 좌표평면 상에 오버랩 문제
# 값에 노이즈를 살짝 추가하여 겹치지 않도록 표혀
par(mfrow = c(1,2),
    pty = 's')
plot(ozo$V6,ozo$V7,xlab = 'wind speed',
     ylab = 'Humidity', pch = 15, cex = .5)
plot(jitter(ozo$V6), jitter(ozo$V7),
     xlab = 'wind speed',ylab = 'Humidity',
     pch = 15, cex = .5)

#points() : 이미 생성된 plot()에 점을 추가로 그려줌
# data(iris)
# with(iris,{
#   plot(NULL, xlim = c(0,5), ylim = c(0,10),
#        xlab = 'width', ylab = 'length',
#        main = 'iris', type = 'n')
#   points(Sepal.width,Sepal.Length, cex =1 , pch = '*', col = 'blue')
#   points(Petal.width,Petal.Length, cex =1 , pch = '+', col = 'red')
# })

data(iris)
with(iris, {
  plot(NULL, xlim = c(0,5), ylim = c(0,10),
       xlab = 'width', ylab = 'length',
       main = 'iris',type = 'n')
  points(Sepal.Width, Sepal.Length, cex = 1 ,pch = "*", col = 'blue')
  points(Sepal.Width, Petal.Length, cex = 1 ,pch = "+", col = 'red')
})
#lines() : 꺾은선
x <- seq(0,2*pi,0.1)
y<- sin(x)
plot(x,y,cex =0.5, col = 'red')
lines(x,y)

#abline() : 직선
#y= ax+b 형태의 직선이나 y = h, x=v 형태의 직선 등을 plot()에 추가

plot(cars,xlim = c(0,25));abline(a = -5, b =3.5,col = 'red')

#데이터를  plot하고 세개의 보조선을 표현하세요
#1)기울기 : 3.2, 절편 : -10 인 보조선
#2)speed 변수의 평균값을 나타내는 보조선
#3)dist 변수의 평균값을 나타내는 보조선

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
#############################################상명
#############################################

#네이버 개발자 센터
#http://developers.naver.com/main

#1. ID/PW 저장
clientID <- 'PnRvqOklqoJ3PUo_jth9'
clientPW <- 'TRd9zhQk9z'

#2. 기본 URL
urlStr <- 'https://openapi.naver.com/v1/search/blog.xml?'

#3. 검색어 서정 및 UTF-8 URL encoding
searchQuery <- 'query= 속초 숙박'

#3-1. UTF-8 인코딩
searchStr <- iconv(searchQuery, to ='UTF-8')

#3-2. URL 인코딩
searchStr <- URLencode(searchStr)
searchStr

#4. 나머지 요청변수 설정(유사도 정렬)
otherStr <- '&display=100&start=1&sort=sim'

#5. URL 완성
reqURL <- paste(urlStr,searchStr,otherStr,sep = '')
reqURL

#############################################################################

#6. httr 패키지(GET() 이용 인증정보 담아 url 호출)

#install.packages('httr')
library(httr)

#7. GET 함수 호출
apiResult <- httr::GET(reqURL,
                       add_headers('X-Naver-Client-Id' = clientID,
                                   'X-Naver-Client-Secret' = clientPW))
apiResult

str(apiResult) #content가 text!!

#9. raw data --> char 변경
blogData <- rawToChar(apiResult$content)
blogData

#10. encoding 변경
Encoding(blogData) <- 'UTF-8'

#11. 결과 확인
blogData

###################데이터 정제######################
copyData <- blogData

library(KoNLP)
# library(rJava)
#library(stringr)


#1. XML 태그 제거

copyData <- gsub('<.*?>',' ',copyData)
copyData

copyData_re <- gsub('[[:punct:]]',' ',copyData)
copyData_re <- str_replace_all(copyData_re,'[A-z0-9]','')
copyData_re <- gsub('물놀이',' ',copyData_re)
copyData_re <- gsub('데',' ',copyData_re)
copyData_re <- gsub('더위',' ',copyData_re)
copyData_re <- gsub('대',' ',copyData_re)
copyData_re <- gsub('해',' ',copyData_re)
copyData_re <- gsub('친구',' ',copyData_re)
copyData_re <- gsub('자연',' ',copyData_re)
copyData_re <- gsub('번',' ',copyData_re)
copyData_re <- gsub('군',' ',copyData_re)
copyData_re <- gsub('동',' ',copyData_re)
copyData_re <- gsub('전',' ',copyData_re)
copyData_re <- gsub('을',' ',copyData_re)
copyData_re <- gsub('한',' ',copyData_re)
copyData_re <- gsub('여름',' ',copyData_re)
copyData_re <- gsub('블로그',' ',copyData_re)
copyData_re <- gsub('곳',' ',copyData_re)
copyData_re <- gsub('물',' ',copyData_re)
copyData_re <- gsub('수',' ',copyData_re)
copyData_re <- gsub('이',' ',copyData_re)
copyData_re <- gsub('시원',' ',copyData_re)#
copyData_re <- gsub('으로',' ',copyData_re)
copyData_re <- gsub('은',' ',copyData_re)
copyData_re <- gsub('길',' ',copyData_re)
copyData_re <- gsub('라',' ',copyData_re)
copyData_re <- gsub('의',' ',copyData_re)
copyData_re <- gsub('시',' ',copyData_re)
copyData_re <- gsub('것',' ',copyData_re)
copyData_re <- gsub('양',' ',copyData_re)
copyData_re <- gsub('를',' ',copyData_re)
copyData_re <- gsub('추천',' ',copyData_re)
copyData_re <- gsub('공식',' ',copyData_re)
copyData_re <- gsub('나',' ',copyData_re)

copyData_s <- sapply(copyData_re, KoNLP::extractNoun, USE.NAMES = FALSE)
head(sort(table(copyData_s),decreasing = T),40)
str(cs)
library(RColorBrewer)
library(wordcloud)

palette <- brewer.pal(8,'Set3') #RcolorBrewer에서 제공해주는 함수
wordcloud(names(cs),freq = cs,scale = c(5,0.8),
          rot.per = 0.25, min.freq=1,
          random.order = F,random.color = T,colors = palette)

library(wordcloud2)
# wordcloud2(data=wdcount,fontFamily = '나눔바른고딕')
wordcloud2(wdcount,color = "random-light", backgroundColor = "grey",fontFamily = '나눔바른고딕')




















