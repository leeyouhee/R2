.libPaths()
install.packages()
installed.packages()
data()

#내장함수를 복사 
iris
iris_df <- iris

head(iris_df,10)
table(iris_df$Species) #Species의 데이터 분포?
View(iris_df) #view 파일 생성

#벡터 만들기
c()
vec <- c(1,2,3,4,5)
vec

#인덱스 접근 방법
vec[1] #파이썬고 차이는 인덱스가 1부터 시작
vec[c(2,5)]
vec[c(1,4)]

vec2 <- vec*2
vec2 <- c(vec2,9)
vec2[c(2,5)]
vec2
vec1 <- 1:5
vec1

str(vec1)
str(vec2) # 구조
str(iris_df)
class(vec2) #추상적 자료형
mode(vec2) #물리적 자료형
class(iris_df)
mode(iris_df)

vec2 <- seq(1,5,1) # seq(from,to,by)
vec2

vec3 <- seq(1,50,10)
vec3

str(vec3)

vec3 = '1'
vec3
str(vec3)


vec4 <- '홍길동동'
vec4

vec5 <- c('홍','유')
vec5

vec6 <- c(1,'홍홍')
vec6
str(vec6) #벡테에 수치형과 문자형을 넣으면 문자형이 됨

#평균 구하기
a<- 1:100
mean(a)
mean(seq(1,100,1))

#시스템 날짜 확인
Sys.Date() #년, 월, 일
Sys.time() # 년, 월, 일 시간(초)까지
date()

str('2020/08/10') #문자형
str(as.Date('2020/08/10')) #문자열을 데이트 안에 감싸기
aaa <- as.Date('2020/08/09')
aaa -1 #숫자
str(aaa)

#날짜 데이터가 미국식으로 다음과 같이 표현되어있다.
dt <- c('1-5-17','18-7-20')
#한국 식으로 표현되도록 변경
dt1 <- dt
dt1
dt1 <- as.Date(dt1,'%d-%m-%y')
dt1


x <- c(1,3,5,7)
y <- c(3,5)

x
y
#합집합
union(x,y)

#교집합
intersect(x,y)

#차집합
setdiff(x,y)

#1-3 벡터에 칼럼명과 같이 각 요소별 이름
english <- c(55,80,75,100)
english

names(english) <- c('정','뒁','담','뒁1')
english
english['정']

#결측치 처리
english
english[3] <- NA
english

#1-4 벡터의 길이값 확인 및 자료 추출 이용

num <- 1:100
head(num)
length(num)

num1 <- num[c(1:10,91:100,16:44)]
num1
length(num1)

num2 <- num[c(1:10,91:100)]
num2
length(num2)

num
num[11:(length(num)/2)]
num[c(2,4)]
num[-c(91:100)] #-인덱스 ; -뒤에 빼고 추출'
length(num[-c(91:100)])

round(1.6)
#진리값
1 %in% c(1,3,5)
6 %in% c(1,3,5)

#범주형 factor
+데이터가 특정 유형으로 구분됨
+예1 : 명목형(진보,보수,중도) - 크기비교가 아니라 얼마나 분포해 있는가
+예2 : 순서형(수,우,미,양,가) - 순서를 둘 수 있는 경우

summary(iris_df)
str(iris_df)

#정리 : 벡터의 기본 성질
자료형이 아닌 자료 구조
데이터의 순서를 가지는 1차원 구조
수치형/문자열형

#매트릭스
mat <- matrix(1:9,ncol =3)
mat # 열기준으로 자료 배치
mat1 <- matrix(1:9,ncol = 3, byrow = TRUE)
mat1 #행기준으로 자료 배치

mat2 <- matrix(1:9,nrow=3, dimnames = 
                 list(c('a1','a2','a3'),c(1,2,3)), byrow = T)#행이름, 열이름
mat2


#이름 붙이기
1. dimnames(mat) <- list(c(),c())
2. colnames(mat) <- c()
3. rownames(mat) <- c()

#문자열형
rep(c('a','b','c'),3 ) # rep(c(),times=int)
mat3 <- matrix(rep(c('a','b','c'),times =3),ncol=3)
mat3

mat4 <- matrix(rep(c('a','b','c'),each =3),ncol=3)
mat4

mat5 <- matrix(rep(c('a','b','c'),each=1),ncol=6)
mat5
t(mat5)

dim(mat5) #n by b 행렬 알아보기
nrow(mat5)
ncol(mat5)

#apply 행렬에 단위 연산을 지원하는 함수
%>% 
m <- matrix(1:9,ncol=3,byrow = T)
m
apply(m,1,max) #행별 최대값
apply(m,1,mean) #행별 평균값
apply(m,2,min) #열별 최소값

#열이름 삽입
m1 <- m
colnames(m1) <- c('이','유','희희')
m1










