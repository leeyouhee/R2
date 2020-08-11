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















