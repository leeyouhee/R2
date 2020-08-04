#패키지 설치
#'KoNLP' ; 형태소 분석하는 패키지

install.packages('multilinguer')
library(multilinguer)
install.packages(c('stringer', 'hash', 'tau',
                   'Sejong', 'RSQLite','devtools'), type = 'binary')
install.packages('remotes')
remotes::install_github('haven-jeon/KoNLP', upgrade='never', 
                        INSTALL_opts = c('--no-multiarch'))
library(KoNLP)
.libPaths()
library(rJava)

#R과 패키지는 같이 Version이 올라감. Version이 다를시 오류날 수 있습니다.

#출력함수 : print(), cat()

print()  | cat()
변수의 값마 출력 가능 | 텍스트를 삽입해서 출력 가능
하나의 변수만 출력 | 여러 변수 출력 가능

#print, cat()
i <- 40
sprintf('나는 %d 살이다',i)



print_test <- c(1,5,7)
print_test1 <- c(2,4,6)
print(print_test)
print(print_test,print_test1)

cat(print_test,print_test1)
cat('첫번째 홀수 =', print_test, '두번째 짝수 =' , print_test1)

#1-1. if()
 형식 : if(조건식){참인 경우 처리문}
        else {거짓인 경우 처리문}
#1) if()함수
x <- 10
y <- 5
z <- x*y
z

if(x*y >40){#산술 > 관계 > 논리
  cat('x*y의 결과는 40 이상입니다.\n')
  cat('x*y=', z, '\n')
  print(z)
}
else{
  cat('x*y의 결과는 40 미만입니다. x*y =', z, '\n')
}

#학점 구하기
score <-scan()
#score <- 85

result <-'low'
#참인경우에만 실행, else가 꼭 있어야 하는 것이 아님
if(score >= 80){
  result <- '완벽'
  print(result)
}
result
cat('your score is', score, ', which is', result)

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
  rewult = 'F학점'
}
cat('당신의 학점은', result)
print(result)

#ifelse(조건식, 참인 경우 처리문, 거짓인 경우 처리문)
score <-95
ifelse(score >=80,'좋아','별로')

#1-3 which()
subsetting
벡터 객체를 대상으로 특정데이터를 검색하는데 사용
조건식을 만족하는 경우 해당 벡터 원소의 인덱스를 반환
데이터 프레임에서 사용

#데이터 프레임 생성
no <- c(1:6)
name <- c('홍길동','이유희','정두영','김민수','민국기','박상명')
score <- c(85,78,89,90,74,89)
exam <- data.frame(학번 = no,이름=name,점수=score)
exam
#인덱스 4의 전체 데이터를 보고싶다.
exam[4,]
#Boolean indexing
exam$이름 =='정두영' 
exam[exam$이름 =='정두영',] #false를 무시하고 T인것에 대한 모든 컬럼 반환

#이름이 정두영인 사람의 인덱스 및 하번과 점수는?
exam[which(exam$이름 == '정두영'),]
#조건을 만족하는 이름이 정두영인 사람의 인덱스를 알고싶다
which(exam$이름 =='정두영')
#subsetting
subset(exam, 이름 == '정두영')

#2. 반복문
##2-1. for()
 형식 : for(변수 in 범위){실행문}

#1)반복문 for(변수 in 값){표현식}
i <- c(1:10)
i
d <- c()
for(n in i){#10회 반복
  print(n*10)
  print(n)
  d[n] <- n*2 #d[1]= 2,....d[10]=20
}
for(n in 1:length(i)){
  print(n*10)
  print(n)
  d[n] <- n*2
}

#실습 : 1~30까지 짝수값만 출력하는 반복문
i <- c(1:30)

for(n in i){
  if(n%%2 ==0){
    print(n)
  }
}

#실습 1~20까지에서 짝수이면 넘기고, 홀수이면 *3해서 출력
j <- 1:20
for(n in j){
  if(n%%2==0){
    next
  }else{
    print(n*3)
  }
}

#실습 : 벡터를 인덱스로 사용 ; 벡터데이터의 사용
        #score <- c(85,95,98), name <- c('이유희','정두영','김민수')일때, 
        #다음 형태로 출력해 보세요
        #which(exam$이름 =='정두영')
  이유희 -> 85
  정두영 -> 95
  김민수 -> 98


score <- c(85,95,98)
name <- c('이유희','정두영','김민수')
score

i <- 1
for(s in score){
  cat(name[i], ' -> ',s,'\n')
}


for(i in 1:length(score)){
  cat(name[i], ' -> ',score[i],'\n')
}

#2-2. while()
기본적으로 for()와 동일
다만, while은 for와 달리 직접 반복 회수를 결정하지 않고 블록내에서
증감식을 이용해 종료시점이 지정

++형식 : while(조건문){실행문}

j <- 1
while(j<5){
  print('남은 시험이 있습니다.')
  j <- j+1
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




#3. User Defined Function
사용자가 인풋을 입력하여 처리하는 매개변수를 이용하는 함수를 정의해본다
ex) 두수의 덧ㅅ셈
f_plus <- function(x,y){
  adding = x+y
  return(adding) #함수 밖으로 return
}

f_plus
f_plus(3,5)




#실습 : 아래의 형태로 구구단을 출력하는 함수를 정의하세요(2~9단)
**2단**
2*1 =2
  ...
**9단**
9*9 = 81
  
# i <- 2
# while(i<10){
#   cat('**')
# }

  
##푼것  
f_mul <- function(x,y){
  mul = x*y
  return(mul)
}


for(i in 2:9){
  cat("**",i,"단 **\n")
  for(n in 2:9){
    cat(i,"*",n,"=",f_mul(i,n),"\n")
  }
}

##강사님
gugudan <- function(i){
  for(x in 2:i){
    cat('**',x,'단','**','\n')
    for( y in 1:9){
      cat(x,'*',y,'=',x*y,'\n')
    }
  }
}

gugudan(1)

#패키지 로딩
library(rJava) ; library(KoNLP)
#파일 불러오기, readLines : 줄단위를 벡터로 가져온다.
seoul_data <- readLines(con='./data/seoul.txt',
                        encoding = 'UTF-8')
head(seoul_data,10)
seoul_data[1]
#2. 명사 추출
sdata <- sapply(seoul_data, KoNLP::extractNoun,
                USE.NAMES = F) #키값을 제거하고, 인덱스를 보여줌
sdata
sdata2 <- unlist(sdata) #단어로 분리

head(sdata2,8)
sdata2
#불용어 제거
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

#데이터 저장
write(sdata3, 'seoul_new.txt')
#데이터 다시 불러오기
sdata4 <- read.table('seoul_new.txt')
sdata4

#빈도 확인
wdcount <- head(sort(table(sdata4),decreasing =  T),30)

#wordcloud 표현
install.packages(c('RColorBrewer','wordcloud'))

library(RColorBrewer)
library(wordcloud)

palette <- brewer.pal(9,'Set1') #RcolorBrewer에서 제공해주는 함수
wordcloud(names(wdcount),freq = wdcount,scale = c(3,0.5),
          rot.per = 0.25, min.freq=1,
          random.order = F,random.color = T,colors = palette)

.libPaths()
KoNLP::buildDictionary()
buildDictionary(ext_dic=c('woorimalsam','sejong'),
                replace_usr_dic = F)

#인터넷에서 파일 바로 불러오기
#실습 GDP 상위 20개 barplot 그리기
#key-point
 +1. GDP_ranking 상위 5개 행은 값이 없다. 즉, 제거해야한다.
 +2. Chr 자료형인 GDP를 numeric으로 바꿔야 한다. 즉, y축의 범위는 연속형이여야 하기 때문이다.
 +3. 위의 과정을 수행한 뒤, barplot을 그려야 한다.

GDP_ranking =read.csv(file = 'http://databank.worldbank.org/data/download/GDP_PPP.csv',
                      encoding = 'UTF-8')

head(GDP_ranking, 20)

summary(GDP_ranking)

install.packages("stringr") #문자열을 쉽게 처리할 수 있는 패키지 ;;  여기서는 필요 없음
library(stringr)
ranking_GDP <- GDP_ranking[-c(1:5), c(1, 2, 4, 5)] # GDP_ranking의 1:5 행을 제거 및 1,2,4,5 열을을 추출하여 ranking_GDP로 넣는다.
# head(ranking_GDP,3)
# View(ranking_GDP)

ranking_GDP$X.3 <- gsub(',', "", ranking_GDP$X.3) #Chr 자료형인 X.3열(GDP수치)에서 ','를 공백으로 바꾸는 것
ranking_GDP$X.3 <- as.numeric(ranking_GDP$X.3) #Chr 자료형인 X.3열을 numeric 자료형으로 변환
ranking_GDP$X.3 <- ranking_GDP$X.3/1000 #Y축과 범위를 맞취기 위해 1000으로 나눔
#ranking_GDP$X.3
#ranking_GDP[4]
#ranking_GDP
#str(ranking_GDP)
barplot(ranking_GDP$X.3[1:20], names.arg = ranking_GDP$X.2[1:20], 
        col=rainbow(21), xlab ='Nations', ylab='unit($Billion)',
        ylim =  c(0,25000),
        main = '2018년 국가별 GDP 순위(상위 20개국)')








