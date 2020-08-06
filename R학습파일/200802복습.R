#3.1 2차원 어레이 생성
+다차원 구조를 가진 데이터를 만들 때 사용
+즉, 벡터는 1차원 어레이, 매트릭스는 2차원 어레이로 생각
+결론적으로 3차원 이상의 데이터 구조도 표현 가능하다 x(벡터),y(매트릭스),z(3차원 어레이)

arr1 <- array(1:12, dim = c(3,4)) # 3행 4열 어레이 생성
arr1

arr2 <- array(1:12, dim = c(3,4), dimnames =
              list(c('행1','행2','행3'),c('열1','열2','열3','열4')))
arr2

mat1 <- matrix(1:12, nrow=3)
mat1
mat2 <- matrix(1:12, nrow=3, dimnames =
                 list(c('행1','행2','행3'),c('열1','열2','열3','열4')))
mat2

str(arr1)

#3.2 3차원 어레이 생성
arr3 <- array(1:12, dim = c(2,2,3))
arr3

arr <- array(1:12)
arr

#4. 리스트(list)
#리스트란 키 벨류 구조를 의미한다.
a <- list(name ='이유희', age =27, job = '취준생')
a
#리스트와 벡터의 차이는 다양한 타입의 데이터를 가지느냐이다.

b <- list(1:3, 'a', c(T,F,T),c(2,3,4,5))
b
str(b)

c <- unlist(a)
c
class(c)

d <- c('lee', 27, T)
d
names(d) <- c('이름', '나이', '진실여부부')
d

#데이터프레임
df <- data.frame(name = '이유희', age = 27, job = '품질관리자자')
df
str(df)

#컬럼명 변경
names(df) <- c('이름', '나이', '직업')
df

#Factor 형 제거
df1 <- data.frame(name = '이유희', age = 27, job = '품질관리자',
                  stringsAsFactors = F)
df1
str(df1)

names(df1)[1] <- '이름'
df1

#연습문제
1. R이 5번 반복되는 vector 생성
vec1 <- rep('R',5)
vec1

2. 1부터 10까지 3씩 증가되는 요소를 가지는 vector 생성
vec2 <- seq(1,10,3)
vec2

3. 1부터 10까지 3씩 증가는 요소를 갖고 각 요소가 3번씩 반복되는 vector 생성
vec3 <- rep(seq(1,10,3),3)
vec3

4. vec2와 vec3의 요소를 모두 가지는 vector 생성
vec4 <- c(vec2, vec3)
vec4

5. 25부터 15까지 5씩 작아지는 vector 생성
vec5 <- seq(25,15,-5)
vec5

6. vec4에서 홀수번째 요소들만 가지는 vector 생성
vec6 <- vec4[seq(1,length(vec4),2)]
vec6

7.user 데이터 프레임 생성
user <- data.frame(name = '최민수','유관순','이순신','김유신','홍길동',
                   age = 55,45,45,53,15,
                   gender = 1,2,1,1,1,
                   job = '연예인','학생','군인','직장인','무직',
                   sat = 3,4,2,5,5,
                   grade = 'C','C','A','D','A',
                   total = 44.4, 28.5,43.5,NA,27.1)
user

df_user <- data.frame(name = c('최민수','유관순','이순신','김유신','홍길동'),
                      age = c(55,45,45,53,15),
                      gender = c(1,2,1,1,1),
                      job = c('연예인','학생','군인','직장인','무직'),
                      sat = c(3,4,2,5,5),
                      grade = c('C','C','A','D','A'),
                      total = c(44.4, 28.5,43.5,NA,27.1))
df_user
str(df_user)

8.gender 기준으로 barplot 생성(Factor로 변경)
install.packages('ggplot2')
library(ggplot2)

table(df_user$gender) #table이란 data의 빈도를 보여준다.
barplot(table(df_user$gender))

plot(as.factor(df_user$gender)) # plot이란 산점도를 의미한다.

9. 아래의 데이터로 프레임을 생성 후, 
apply()함수를 적용하여 행/열 방향으로 조건에 맞게 통계량을 구하세요

+kor : 90, 85, 90
+eng : 70, 85, 75
+mat : 85, 92, 88

+조건1) 행/열 방향으로 각각 max() 적용
+조건2) 행/열 방향으로 mean() 적용, 소수점 2자리까지 표현(셋째 자리에서 반올림)

#프레임 생성
df_score <- data.frame(kor = c(90,85,90),
                       eng = c(70,85,75),
                       mat = c(85,92,88))
rownames(df_score) <- c('이유희','정뒁','김민수수')
df_score

#조건1)
apply(df_score,1,max) #행별 최대값
apply(df_score,2,max) #열별 최대값

#조건2)
apply(df_score,1,mean)
round(apply(df_score,1,mean),2)

apply(df_score,2,mean)
round(apply(df_score,2,mean),2)

#인덱싱 2열, 3열
mat <- matrix(1:9,nrow =3 ,dimnames =
                list(c('행1','행2','행3'),c('열1','열2','열3')))
mat

#방안1
mat[1:3,2:3]
#방안2
mat[,-1]
#방안3
mat[1:3,-1]

#방안4
mat[c('행1','행2','행3'),c('열2','열3')]

#2,3행과 1,2열의 데이터 인덱싱
mat[-3,-1]

#어레이
arr <- array(1:12, dim=c(2,2,3))
arr

#첫번째 매트릭스의 모든 행에 대하여 2열 데이터를 인덱싱
arr[,2,1]

#세번째 매트릭스의 1행 1열 데이터 인덱싱
arr[1,1,3]

#모든 매트릭스의 2행2열
arr[2,2,]

#리스트 인덱싱
li <- list(name=c('이유희','유희'),
           age = c(27,28),
           job = c('품질관리자','QC'))
li

li$name
li$name[2]

li[2] #2번쨰 키
li[[2]] # list의 요소가 아닌 벡터형식으로 표현
li[2]-1 #오류
li[[2]] -1 #age란 요소가 아닌, 수치 벡터
li[2][2]
li[[2]][2]

#Data Frame
df <- data.frame(name=c('이유희','유희'),
                 age = c(27,28),
                 job = c('품질관리자','QC'))
df
str(df)

#데이터프레임의 자료값에 접근하는 방법으로는
1. 매트릭스와 같이 [,]를 이용하거나
2. 리스트와 같이 $ 기호를 이용할 수 있다.
df[1,]
df[1,2]
df$name

#데이터의 이름 컬럼 추출
df[,1]
df[,'name']
df$name

#2. 데이터 다루기 기본함수
우리가 다루는 데이터의 대부분은 데이터프레임 형식으로 되어있다.
대다수의 데이터가 수치, 문자, 팩터의 자료형을 혼합하여 가지고 있다.
따라서 데이터프레임을 예로 들어 데이터를 다루기 위한 구체적이 기능을 알아 볼 것이다.

#행/열 추가할때 : rbind, cbind

df
1) rbind
new_name <- data.frame(name = c('둥','딩','홍','봉'),
                       age = c(27,31,32,256),
                       job = c('aa','bb','cc','dd'))
new_name

df <- rbind(df, new_name)
df

2) cbind
new_nat <- data.frame(nationality=
                        c('kor','kor','kor','kor','kor','kor'))
df <- cbind(df,new_nat)
df

혹은

df$city <- c('대전','서울','부산','대전','서울','부산')
df

3) df에 영문 성 컬럼을 추가
df$firstname <- c('lee','park','kim','lee','park','kim')
df

4) city를 영문으로 변경
df$city <- c('A','B','C','A','B','C')
df

5) 이유희의 나이를 14로 변경
df$age[1] <- 14
str(df)

6) 이유희의 국적을 대한민국으로 변경
df$nationality[1] <- '대한민국'
df ###Factor일시 오류가 남 따라서 밑에와 같이 해야함
# 
# df$nationality <- as.character(df$nationality)
# df$nationality[1] <- '대한민국'
# df$nationality <- as.factor((df$nationality))

#파일 불러오기
Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_251')
install.packages(c('rJava','xlsx'))

library((rJava))
library(xlsx)

#1-1 텍스트 파일
student : 컬럼명 없음
read.table(file='경로', sep='컬럼구분인자', header = 'T/F')

student <- read.table(file='./data/student.txt',sep='')
student

setwd()#경로지정
student1 <- read.table(file = './data/student1.txt',sep = '',header = T)
student1

student2 <- read.table(file='./data/student2.txt',sep = ';', header = T)
student2
head(student2,2)

student3 <- read.table(file = './data/student3.txt', sep= ' ', header = T
                       ,na.strings = '-')
student3
student3[3,4]

student4 <- read.table(file='./data/student4.txt', sep=',',header =  T,
                       na.strings = c('-','$','+'))
student4

#엑셀 파일
library(xlsx)
studentex <- read.xlsx('./data/studentexcel.xlsx',sheetIndex = 1, encoding = 'UTF-8')
studentex

#실습 : 인구밀도 대비 살인률
head(USArrests,3)
library(tibble)
usar_df <- USArrests
usar_df <- tibble::rownames_to_column(usar_df) #tibble :: 함수 ; 행이름을 컬럼으로 사용
head(usar_df)
usar_df$murder_pop <- usar_df$Murder / usar_df$UrbanPop
usar_df
usar_df$murder_pop <- round(usar_df$murder_pop,3)
usar_df
usa_order <- usar_df[c(order(-usar_df$murder_pop)),]
head(usa_order,3)

barplot(usa_order$murder_pop[1:7],names.arg = usa_order$rowname[1:7],
        main = '인구밀도 대비 살인률',col=rainbow(7),xlab='state',ylab='murder/pop')

#정규표현식
[실습] 문자열 추출하기
install.packages('stringr')
library(stringr)
#형식) str_extract('문자열','정규표현식')
str_extract("홍길동35이순신45유관순25",'[0-9]{2}') #연속된 숫자 2개 추출
str_extract_all("홍길동35이순신45유관순25",'[0-9]{2}')  #연속된 숫자 2개 모두 추출
unlist(str_extract("홍길동35이순신45유관순25",'[0-9]{2}'))
str_extract("홍길동35이순신45유관순25",'[0-9]{2}')

str <- "hongkildong106lee1002you25강감찬2004"
str
str_extract_all(str,'[a-z]{3}')#영어 소문자 연속 3개 추출출
str_extract_all(str,'[a-z]{3,}') # 영어 소문자 연속 3개 이상 추출출
str_extract_all(str,'[a-z]{3,5}')

str <- "YEShongkildong106lee1002you25강감찬2004"
str_extract_all(str,'[A-z]{3}')#영문 대소문자 연속 3개이상 추출

str_extract_all(str,'강감찬') #강감찬 추출출
str_extract_all(str, 'hong') # hong 추출
str_extract_all(str, '25') #25 추출출
str_extract_all(str,'[가-힣]{3}')#한글 연속 3개추출
str_extract_all(str, '[a-z]{3}') #영어 연속 3개 추출(소문자)

string1 <- 'YEShongkildong105lee1002you25강감찬2005'
str_extract_all(string1,'[^a-z]') #영어 소문자 제외하고 추출
str_extract_all(string1,'[^a-z]{4}')
str_extract_all(string1,'[^가-힣]{5}')

#주민번호 데이터를 추출하여 주민번호 컬럼명을 가지는 데이터 프레임 작성
jumin <- '123456-3234567654321-3589621' 
c <- unlist(str_extract_all(jumin,'[0-9]{6}-[0-9]{7}'))
c
jumin_df <- data.frame(주민번호 = c)
jumin_df
#지정된 길이의 단어 추출하기
name <- '홍길동1234,이순신5678,강구찬1012'
#각각 이름과 숫자를 분리하여 데이터 프레임(컬럼명; 이름, 학번)으로 만들기
c_name <- unlist(str_extract_all(name,'[가-힣]{1,}'))
c_name
c_num <- unlist(str_extract_all(name,'[0-9]{1,}'))
c_df <- data.frame(이름 = c_name,
                    학번 = c_num)
c_df

























