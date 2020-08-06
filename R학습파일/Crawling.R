#네이버 개발자 센터
#http://developers.naver.com/main

#1. ID/PW 저장
clientID <- 'PnRvqOklqoJ3PUo_jth9'
clientPW <- 'TRd9zhQk9z'

#2. 기본 URL
urlStr <- 'https://openapi.naver.com/v1/search/blog.xml?'

#3. 검색어 서정 및 UTF-8 URL encoding
searchQuery <- 'query= 속초 펜션'

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

copyData <- blogData
library(stringr)
library(KoNLP)
library(dplyr)
# library(rJava)
#library(stringr)

############################################################################
s_data<-read.table(file='./data/상가업소정보_강원_202006.csv',
                     fileEncoding='UTF-8',sep="|",header=T,
                     fill=T,quote = "")
str(s_data)
View(s_data)

ss <- s_data %>% select(상권업종대분류명)
table(ss)
sss <- s_data %>% 
  filter(상권업종대분류명 == '음식')
View(sss)


sokcho <- s_data %>% 
  filter(시군구명 == '속초시', 상권업종대분류명 == '숙박')
View(sokcho)

sokcho_P <- sokcho %>% select(상호명)
head(sokcho_P,4)



dic_sokcho <- data.frame(place = sokcho_P, 분류 = 'ncn')
buildDictionary(user_dic = dic_sokcho,
                replace_usr_dic = F)
.libPaths()

# install.packages('readr')
# library(readr)
# 
# t <- read.delim(file='', col.names = T,delim='|')

#############################################################################


#1. XML 태그 제거

copyData <- gsub('<.*?>',' ',copyData)
copyData_re <- gsub('[[:punct:]]',' ',copyData)
copyData_re <- str_replace_all(copyData_re,'[A-z0-9]','')
copyData_re <- gsub('물놀이',' ',copyData_re)
copyData_re <- gsub('더위',' ',copyData_re)
copyData_re <- gsub('친구',' ',copyData_re)
copyData_re <- gsub('자연',' ',copyData_re)
copyData_re <- gsub('여름',' ',copyData_re)
copyData_re <- gsub('블로그',' ',copyData_re)
copyData_re <- gsub('시원',' ',copyData_re)#
copyData_re <- gsub('으로',' ',copyData_re)
copyData_re <- gsub('추천',' ',copyData_re)
copyData_re <- gsub('공식',' ',copyData_re)
copyData <- gsub('<.*?>',' ',copyData)
copyData_re

copyData_re <- gsub('[[:punct:]]',' ',copyData)
copyData_re <- str_replace_all(copyData_re,'[A-z0-9]','')
copyData_re <- gsub('물놀이',' ',copyData_re)
copyData_re <- gsub('더위',' ',copyData_re)
copyData_re <- gsub('친구',' ',copyData_re)
copyData_re <- gsub('자연',' ',copyData_re)
copyData_re <- gsub('여름',' ',copyData_re)
copyData_re <- gsub('블로그',' ',copyData_re)
copyData_re <- gsub('시원',' ',copyData_re)#
copyData_re <- gsub('으로',' ',copyData_re)
copyData_re <- gsub('추천',' ',copyData_re)
copyData_re <- gsub('공식',' ',copyData_re)
copyData_re <- gsub('속초',' ',copyData_re)
copyData_re <- gsub('숙박',' ',copyData_re)

copyData_re <- gsub('여행',' ',copyData_re)
copyData_re <- gsub('호텔',' ',copyData_re)
copyData_re <- gsub('리조트',' ',copyData_re)
copyData_s <- sapply(copyData_re, KoNLP::extractNoun, USE.NAMES = FALSE)
copyData_s <- Filter(function(x){nchar(x)>2},copyData_s)
wdcount <- head(sort(table(copyData_s),decreasing = T),40)
str(cs)
wdcount

library(wordcloud2)
# wordcloud2(data=wdcount,fontFamily = '나눔바른고딕')
wordcloud2(wdcount,color = "random-light", backgroundColor = "black",fontFamily = 'Arial', size = 0.7)

