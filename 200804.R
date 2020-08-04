#명사 추출
#여행지 조사(사전에 builddictionary 후 다시 추출)
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
install.packages(c('RColorBrewer','wordcloud'))

library(RColorBrewer)
library(wordcloud)

#파일 불러오기
jeju_data <- readLines(con = './data/jeju.txt')
head(jeju_data,3)

#명사 추출
jdata <- sapply(jeju_data,KoNLP::extractNoun,
                USE.NAMES = F)
head(jdata,3)

#단어 분리
jdata2 <- unlist(jdata)
head(jdata2,3)
jdata2

#불용어 제거
jdata3 <- gsub('제주','',jdata2)
jdata3 <-gsub('제주도','',jdata3)
jdata3 <-gsub('[0-9]+','',jdata3)
jdata3 <-gsub('오전','',jdata3)
jdata3 <-gsub('오후','',jdata3)
jdata3 <-gsub('/','',jdata3)
jdata3 <-gsub('\\.','',jdata3)
jdata3 <-gsub('-','',jdata3)
jdata3 <-gsub('?','',jdata3)
jdata3

jdata3 <- gsub('추천','',jdata3)
jdata3 <- gsub('흑돼지','',jdata3)
jdata3 <- gsub('가게','',jdata3)
jdata3 <- gsub('쪽','',jdata3)
jdata3 <- gsub('^ㅇ','',jdata3)
jdata3 <- gsub('것','',jdata3)
jdata3 <- Filter(function(x){nchar(x)>=2},jdata3)
jdata3 <-gsub('숙소','',jdata3)
jdata3 <-gsub('시간','',jdata3)
jdata3 <-gsub('여행','',jdata3)
jdata3 <-gsub('해안','',jdata3)
jdata3 <-gsub('코스','',jdata3)
jdata3 <-gsub('까지','',jdata3)
jdata3 <-gsub('드라이브','',jdata3)
jdata3 <-gsub('출발','',jdata3)
jdata3 <-gsub('예약','',jdata3)
jdata3 <-gsub('경유','',jdata3)
jdata3 <-gsub('관광지','',jdata3)
jdata3 <-gsub('일정','',jdata3)
jdata3 <-gsub('하게','',jdata3)
jdata3 <-gsub('도착','',jdata3)

###############################################################################

install.packages("stringr")
library(stringr)
place_re <- str_replace_all(cdata,'[A-z0-9]','')
place_re <- gsub('제주','',x=place_re)

head(sort(table(place_re),decreasing = T),40) #단어를 확인하여 불필요한 단어 확인

#불용어 목록 만들고, 불러오기
no <- readLines(con='./data/불용어.txt',
                 encoding = 'UTF-8')

for( i in 1:length(no)){
   place_re <- gsub(pattern = no[i],"",x = place_re)
}

#문자길이 2개 이상 단어 추출
place_re <- Filter(function(x){
  nchar(x) >= 2 #nchar()는 문자길이 추출
},place_re)

#################################################################################



#데이터 저장
write(jdata3, 'jeju_new.txt')
#데이터 다시 불러오기
jdata4 <- read.table('jeju_new.txt')
jdata4

#빈도확인
wfreq <- head(sort(table(jdata4),decreasing = T),30)

palette <- brewer.pal(9,'Set1') #RcolorBrewer에서 제공해주는 함수
wordcloud(names(wfreq),freq = wfreq,scale = c(4,0.5),
          rot.per = 0.25, min.freq=1,
          random.order = F,random.color = T,colors = palette)




.libPaths()


#데이터 프레임으로 변경
Jeju <- readLines(con='./data/go.txt',
                  encoding = 'UTF-8')
Jeju_to <- data.frame(여행지 = Jeju, 형태 = 'ncn')

#user_dic에 추가
  +1. user_dic의 의미가 중요!!!!
  +2. 내가 고민했던 것은 : 딕셔너리로 만든 결과와 jeju.txt에 있는 형태소를 어떻게 비교할지!
  +3. 하지만 아래와 같이 제주명소로 딕셔너리를 만들었고, 이것은 KoNLP 패키지 안에 자동적으로 참조되어 비교해 줌

KoNLP::buildDictionary()
buildDictionary(user_dic = Jeju_to,
                replace_usr_dic = F) 

#파일 불러오기
jeju_data <- readLines(con = './data/jeju.txt')
head(jeju_data,3)

#명사 추출

Jdata <- sapply(jeju_data,KoNLP::extractNoun,
                USE.NAMES = F)
Jdata

#단어 분리
jdata2 <- unlist(Jdata)
head(jdata2,3)
jdata2
#불용어 제거
jdata3 <- gsub('제주','',jdata2)
jdata3 <-gsub('제주도','',jdata3)
jdata3 <-gsub('[0-9]+','',jdata3)
jdata3 <-gsub('오전','',jdata3)
jdata3 <-gsub('오후','',jdata3)
jdata3 <-gsub('/','',jdata3)
jdata3 <-gsub('\\.','',jdata3)
jdata3 <-gsub('-','',jdata3)
jdata3 <-gsub('?','',jdata3)
jdata3

jdata3 <- gsub('추천','',jdata3)
jdata3 <- gsub('흑돼지','',jdata3)
jdata3 <- gsub('가게','',jdata3)
jdata3 <- gsub('쪽','',jdata3)
jdata3 <- gsub('^ㅇ','',jdata3)
jdata3 <- gsub('것','',jdata3)
jdata3 <- Filter(function(x){nchar(x)>=2},jdata3)
jdata3 <-gsub('숙소','',jdata3)
jdata3 <-gsub('시간','',jdata3)
jdata3 <-gsub('여행','',jdata3)
jdata3 <-gsub('해안','',jdata3)
jdata3 <-gsub('코스','',jdata3)
jdata3 <-gsub('까지','',jdata3)
jdata3 <-gsub('드라이브','',jdata3)
jdata3 <-gsub('출발','',jdata3)
jdata3 <-gsub('예약','',jdata3)
jdata3 <-gsub('경유','',jdata3)
jdata3 <-gsub('관광지','',jdata3)
jdata3 <-gsub('일정','',jdata3)
jdata3 <-gsub('하게','',jdata3)
jdata3 <-gsub('도착','',jdata3)

#데이터 저장
write(jdata3, 'jeju_new.txt')
#데이터 다시 불러오기
jdata4 <- read.table('jeju_new.txt')
jdata4

#빈도확인
wfreq <- head(sort(table(jdata4),decreasing = T),30)
?brewer.pal()
palette <- brewer.pal(6,'Dark2') #RcolorBrewer에서 제공해주는 함수
wordcloud(names(wfreq),freq = wfreq,scale = c(3,0.5),
          rot.per = 0.4, min.freq=1,
          random.order = F,random.color = T,colors = palette)

###################연습문제######################
1. 다음 조건에 맞게 client 데이터프레임을 생성하고 데이터를 처리해보세요

+ name : '유관순','홍길동','이순신','신사임당'

+ gender : 'F','M','M','F'

+ price : 50,65,45,75

+ 조건1 : 3개의 벡터 객체를 이용하여 client 데이터 프레임 생성

+ 조건2 : price 변수의 값이 65만원 이상이면, 문자열 'Best', 65만원 미만이면 'Normal'을 변수 result 컬럼에 추가

+ result 변수를 대상으로 빈도수 구하기

#조건 1
name <- c('유관순','홍길동','이순신','신사임당당')
gender <- c('F','M','M','F')
price <- c(50,65,45,75)
client <- data.frame(이름 = name, 성별 = gender, 가치 = price)
client
#조건 2

for(i in 1:length(price)){
  ifelse(price[i]>= 65, client$result[i] <- 'Best', client$result[i] <- 'Normal')
}
client
str(client)

table(client$result)

#########################################################################

2. 다음 벡터 EMP는 '입사년도이름급여' 순으로 사원의 정보가 기록된 데이터이다.
벡터 EMP를 이용하여 다음과 같은 결과가 나타나도록 함수를 정의해보세요
(함수에 변수 EMP를 전달했을 때 출력결과와 같도록 만드시면 됩니다)

EMP <- c('2014홍길동220','2002이순신300','2010유관순260',"2019왕건500","2019동방신기1000")

<출력결과>
전체 급여 평균 : 456
평균이상 급여 수령자
왕건 => 500
동방신기 => 1000

install.packages('stringr')
library(stringr)


cal_sal <-function(EMP){
  #로직
  #year <- str_extract(EMP,'[0-9]{1,}$')
  EMP_year <- str_extract(EMP,'[0-9]{4}')
  str_extract_all(EMP,'[가-힣]{1,}')
  EMP_name <- unlist(str_extract_all(EMP,'[가-힣]{1,}'))
  sal <- str_extract(EMP,'[가-힣]{1,}[0-9]{1,}')
  EMP_sal <- unlist(str_extract_all(sal,'[0-9]{1,}'))
  
  EMP_sal <- as.numeric(EMP_sal)
  employee <- data.frame(name = EMP_name, year = EMP_year, Salary = EMP_sal
                         ,stringsAsFactors = F) #StringAsFactor 주의!!
  EMP_mean <- mean(employee$Salary)
  
  #출력문
  cat('<출력결과>\n')
  cat('전체 급여 평균 : ', EMP_mean, '\n')
  cat('평균 이상 급여 수령자 \n')
  for(i in 1:length(employee$name)){
    if(employee$Salary[i]>=EMP_mean){
      cat(employee$name[i], ' => ', employee$Salary[i],'\n')
    }
  }
}
cal_sal(EMP)

###############################################################################
3. 함수 y = f(x)에서 x의 값이 a에서 b까지 변할 때 △x = b - a를 증분이라 하며,
△y = f(b) - f(a)를 y의 증분으로 표현한다.
평균변화율 : △y/△x = (f(b)- f(a))/(b - a)

조건) 함수 f(x) = x^3 + 4에서 x의 값이 1에서 3까지 변할 때 평균변화율을
구하는 함수를 작성해보세요. (평균변화율 = (f(3)-f(1))/(3-1) = 13)

Chan <- function(x1,x2){
  a1 <- x1^3+4
  a2 <- x2^3+4
  b <-  x2-x1
  c <- a2-a1
  return(c/b)
}

Chan(1,3)

#########################################################################
4. 실습 : 몬테카를로 시뮬레이션 (runif)

몬테카를로 시뮬레이션은 현실적으로 불가능한 문제의 해답을 얻기 위해서 난수의

확률분포를 이용하여 모의시험으로 근사적 해를 구하는 기법

n번 시행했을 때 동전던지기의 확률을 구하라!

mons <- function(n){
  result = 0
  for( i in 1:n){
    co <- runif(1)
    if (co >= 0.5){
      result <- result +1
    }
  }
  prob <- result/n
  return(prob)
}
mons(99999)




























