###3.1 2차원 어레이 생성
#다차원 구조를 가진 데이터를 만들 때 사용
#즉, 벡터는 1차원 어레이, 매트릭스는 2차원 어레이로 생각
# 결론적으로 3차원 이상의 데이터 구조도 표현가능하다. ex) 높이

arr1 <- array(1:12, dim = c(3,4))
arr1

arr2 <- array(1:12, dim = c(3,4), dimnames = list(c('r1','r2','r3'),c('c1','c2','c3','c4')))
arr2

mat1 <- matrix(1:12, nrow =3)
mat1

mat2 <- matrix(1:12, nrow =3, dimnames = list(c('r1','r2','r3'),c('c1','c2','c3','c4')))

str(arr1)


#3.2 3차원 어레이 생성
arr3 <- array(1:12, dim = c(2,2,3))
arr3

arr <- array(1:12)
arr


#4 . 리스트(list)
#키-벨류 구조

a <- list(name='뒁', age = 27, job = '투자자')
a
b <- list(1:3, 'a', c(TRUE,FALSE,TRUE), c(2.3,5.9))
b
str(b)

c <- unlist(a)
c
class(c)

d <- c('뒁', 30, 'kin')
names(d) <- c('name', 'b', 'c')
d

##데이터프레임
df <-data.frame(name = '뒁', age = 27, job = '신풍제약 대주주')
df
str(df)

##컬럼명 변경
names(df1) <- c('이름','나이', 'dd')

df1 <- data.frame(name = '뒁', age = 27, job = '신풍제약 대주주',
                  stringsAsFactors = FALSE)
df1

names(df1)[1]<-"ㅜ뉘뉘"
df1


##연습문제
vec1 <- rep('R',5)
vec1
vec2 <- seq(1,10,3)
vec2
vec3 <- rep(seq(1,10,3),3)
vec3
vec4 <- c(vec2 , vec3)
vec4
vec5 <- seq(25,15,-5)
vec5

vec6 <- vec4[seq(1,length(vec4), 2)]
vec6
#df <-data.frame(name = '뒁', age = 27, job = '신풍제약 대주주')
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

install.packages("ggplot2")
library(ggplot2)

table(user$gender)
barplot(table(df_user$gender))

plot(as.factor(df_user$gender))

#qplot(data = df_user, x = gender)

#hist(df_user$gender, freq = FALSE)
# df_user$gender <- as.factor(df_user$gender)
# is.factor(df_user$gender)
# barplot(df_user,df_user$gender)
# ?barplot

3. 아래의 데이터로 데이터프레임 생성 후, 
apply()함수를 적용하여 행/열 방향으로 조건에 맞게 통계량을 구해보세요

+ kor : 90,85,90

+ eng : 70,85,75

+ mat : 86,92,88

+ 조건1) 행/열 방향으로 각각 max() 적용, 최대값 구하기

+ 조건2) 행/열 방향 mean() 적용 평균을 구하여 소수점 2자리까지 표현하세요(셋째자리에서 반올림)

#프레임 생성
df_score <- data.frame(kor = c(90,85,90),
                       eng = c(70,85,75),
                       mat = c(86,92,88)
)
df_score

#조건1)
apply(df_score, 1,max)

apply(df_score, 2,max)

#조건2)
apply(df_score, 1,mean)
round(apply(df_score, 1,mean),2)
apply(df_score, 2,mean)
round(apply(df_score, 2,mean),2)

###인덱싱 2열 3열
mat <- matrix(1:9, nrow =3 , dimnames = 
                list(c('a1','a2','a3'),c('b1','b2','b3')))
#방안1
mat[1:3,2:3]

#방안2
mat[1:3,-1]
#방안3
mat[,-1]

mat[1,]

#방안4
mat[c('a1','a2','a3'),c('b2','b3')]

#2,3행과 1,2열의 데이터를 인덱싱
mat[2:3,1:2]
mat[2:3,-3]
mat[-1,-3]


##어레이
arr <- array(1:12, dim = c(2,2,3))
arr

#첫번째 매트릭스의 모든 행에 대하여 2열 데이터를 인덱싱

arr[,2,1]
#세번째 매트릭스의 1행 1열 데이터 인덱싱
arr[1,1,3]

#모든 매트릭스의 2행2열
arr[2,2,]


#리스트
li <- list(name = c('뒁','뒁기덩'),
           age = c(27,26),
           job = c('대주주', '투자자'))
li

# $이용
li$name
li$name[2]

#[[]] 이용
li[2] #2번째 키
li[2] + 1 #오류
li[[2]] #age가 아닌 하나의 벡터로 표현
li[[2]]+1
li[[2]][2]

#Data Frame
df <- data.frame(name = c('뒁','뒁기덩'),
                 age = c(27,26),
                 job = c('대주주', '투자자'))
df
str(df)

데이터프레임의 자료값에 접근하는 방법으로는
1. 매트릭스와 같이 [,]를 이용하거나
2. 리스트와 같이 $ 기호를 이용할 수 있다.

df[1,]
df[1,2]
df[1,c('name','age','job')]

#데이터의 이름 컬ㄹ럼 추출
##방법1
df[,1]
df[,'name']
##방법2
df$name

#2. 데이터 다루기 기본함수
우리가 다루는 데이터의 대부분은 데이터프레임 형식으로 되어있다.
대다수의 데이터가 수치, 문자, 팩터의 자료형을
혼합하여 가지고 있기 때문이다. 따라서 데이터프레임을 예로 들어
데이터를 다루기 위한 구체적인 기능을 알아보자

#행/열 추가할때 : rbind, cbind

df
new_men <- data.frame(name = c('둥','딩','홍','봉'),
                      age = c(27,31,32,256),
                      job = c('aa','bb','cc','dd'))
df <- rbind(df, new_men)
df

#df에 국적 컬럼 추가, 그후 거주도시 추가
#방안1
new_nat <- data.frame(nationality =
                        c('kor','kor','kor','kor','kor','kor'))
df <- cbind(df, new_nat)
df
#방안2
df$city <-c('대전','성ㄹ','대전','성ㄹ','대전','성ㄹ')
df

3)df에 영문 성 컬럼을 추가
df$surname <- c('lee','kim','lee','kim','lee','kim')
df
4)city를 영문으로 변경

5)길동의 나이를 34세로 변경
df$age[2] <- 34
df
6)길동의 국적을 chosun으로 변경
df$nationality <- as.character(df$nationality)
df$nationality[2] <- 'chosun'
df
df$nationality <- as.factor(df$nationality)
df
str(df)

#파일 불러오기
##1-1 자바 설치 및 환경설정
Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_251')
install.packages(c('rJava','xlsx'))               

library(rJava)
library(xlsx)

#1-1. 텍스트 파일
student : 컬럼명 없음
read.table(file = '경로명', sep = '컬럼구분자', header = 'TRUE|FALSE')

student <- read.table(file ='C:/Users/09/Desktop/R/20200731_R2/data/student.txt', sep ="",)
student

setwd() #경로지정

student1 <- read.table(file='C:/Users/09/Desktop/R/20200731_R2/data/student1.txt', sep = '', header = T)
student1
student2 <- read.table(file='./data/student2.txt', sep = ';', header = T)
student2
head(student2,2)

student3 <- read.table(file = './data/student3.txt',sep=' ',header = T,
                       na.strings = '-')#문자열을 NA처리리
student3
student3[3,4]

student4 <- read.table(file = './data/student4.txt',sep=',',header = T,
                       na.strings = c('-','$','+'))
student4

#1-3. 엑셀파일
library(xlsx)
studentex <- read.xlsx('./data/studentexcel.xlsx',sheetIndex = 1,encoding = 'UTF-8')
studentex

#1-6. 실습
head(USArrests,3)
인구밀도 대비 살인률

# data("USArrests")
# 
# UrbanM <- (USArrests$Murder)/(USArrests$UrbanPop)
# head(UrbanM,7)
# UM<-order(UrbanM,decreasing = T)
# UM
library(tibble)
USArrests_df <- USArrests
USArrests_df <- tibble::rownames_to_column(USArrests_df)
head(USArrests_df)
USArrests_df$murder_pop <- USArrests_df$Murder / USArrests_df$UrbanPop
USArrests_df
usa_rank <- USArrests_df[c(order(-USArrests_df$murder_pop)),]
barplot(usa_rank$murder_pop[1:7],names.arg = usa_rank$rowname[1:7]   , main = '인구밀도대비 살인률',col=rainbow(7),, xlab ='state', ,ylab='murder/pop')

#####
USArrests_tb <- USArrests
USArrests_df <- tibble::rownames_to_column(USArrests_tb)
USArrests_df$murder_pop <- USArrests_df$Murder / USArrests_df$UrbanPop

USA_order <- USArrests_df[c(order(-USArrests_df$murder_pop)),]
barplot(USA_order$murder_pop[1:7],names.arg = USA_order$rowname[1:7], col=rainbow(7), xlab ='state', ylab='murder/pop',
        main ='인구밀도대비 살인율')

#####

?barplot


##정규표현식
[실습] 문자열 추출하기
install.packages('stringr')
library(stringr)

#형식) str_extract('문자열','정규표혀식')

str_extract("홍길동35이순신45유관순25",'[0-9]{2}') #연속된 숫자 2개 추출(첫번째)
str_extract_all("홍길동35이순신45유관순25",'[0-9]{2}') # 연속된 숫자 2개 모두 추출
unlist(str_extract("홍길동35이순신45유관순25",'[0-9]{2}'))
str_extract("홍길동35이순신45유관순25",'[0-9]{2}')

str <- "hongkildong106lee1002you25강감찬2004"
str_extract_all(str,'[a-z]{3}')
str_extract_all(str,'[a-z]{3,}')
str_extract_all(str,'[a-z]{3,5}')

str <- "YEShongkildong106lee1002you25강감찬2004"
str_extract_all(str,'[A-z]{3}')

str_extract_all(str,'강감찬')
str_extract_all(str, 'hong')
str_extract_all(str, '25')
str_extract_all(str,'[가-힣]{3}')
str_extract_all(str, '[a-z]{3}')

string1 <- 'YEShongkildong105lee1002you25강감찬2005'
#영어 소문자 제외하고 추출
str_extract_all(string1,'[^a-z]')
#영어 소문자 제외, 4개 연속 붙은 것 추출
str_extract_all(string1,'[^a-z]{4}')
#한글제외, 5개 연속 붙은것
str_extract_all(string1,'[^가-힣]{5}')
###########################################################################

jumin <- '123456-3234567654321-3589621' 
#주민번호 데이터를 추출하여 주민번호 컬럼명을 가지는 데이터 프레임 작성
c <- unlist(str_extract_all(jumin,'[0-9]{6}-[0-9]{7}'))
c
jumin_df <- data.frame(주민번호 = c)
jumin_df
#지정된 길이의 단어 추출하기
name <- '홍길동1234,이순신5678,강구찬1012'
#각각 이름과 숫자를 분리하여 데이터 프레임(컬럼명 이름, 학번)으로 만들기
c_name <- unlist(str_extract_all(name,'[가-힣]{1,}'))
c_num <- unlist(str_extract_all(name,'[0-9]{1,}'))
c_name
c_df <- data.frame(이름 = c_name, 학번 = c_num)
c_df



























