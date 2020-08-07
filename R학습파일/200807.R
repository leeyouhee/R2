#Scrapping
install.packages('rvest')
library(rvest)
library(dplyr)
library(stringr)

#1. url

url_main <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=187940&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='

#2. html scrapping
#setwd('')
getwd()
download.file(url = paste0(url_main,1,''), #공백을 없도록
              destfile = 'bakdu.html', quiet = T) #파일명을 무엇으로 할것이냐
bakdu_review <- read_html('bakdu.html')

bakdu_review

#3. get data###################################################################
##3.1 star score
star_score <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.star_score') %>% 
  html_text() #html text 긁어오는 함수
star_score1 <- str_replace_all(star_score,'\r|\t|\n','')
star_score1

##3.2 review
review_text <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('p') %>%  #말단의 노드에는 점없이해야함
  html_text() #html text 긁어오는 함수
review_text
review_text1 <- str_replace_all(review_text,'\r|\t|\n','')
review_text1

##3.3 user
user <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('dt') %>%  #말단의 노드에는 점없이해야함
  html_text() #html text 긁어오는 함수
user
user1 <- str_replace_all(user,'\r|\t|\n','')
user1

###3.3.1 user_id
user_id <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.score_reple') %>%
  html_node('em') %>%
  html_text()
user_id <- str_replace_all(user_id,'\r|\t|\n','')
user_id
###3.3.2 date
date <- str_extract_all(user1,'[0-9]{4}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}')
date1 <- unlist(date)
date1

##3.4 like
like <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.btn_area') %>%
  html_nodes('._sympathyButton') %>% 
  html_text()
like
like_count <- str_replace_all(like,'\r|\t|\n','')
like_count

#3.5 unlike
unlike <- bakdu_review %>% 
  html_nodes('.score_result') %>% 
  html_nodes('.btn_area') %>%
  html_nodes('._notSympathyButton') %>% 
  html_text()
unlike
unlike_count <- str_replace_all(unlike,'\r|\t|\n','')
unlike_count

###############################################################################
#4. loop

star_list <- c()
text_list <- c()
user_id_list <- c()
date_list <- c()
like_list <- c()
unlike_list <- c()

page_no <- bakdu_review %>% 
  html_nodes('.score_total') %>% 
  html_nodes('.total') %>% 
  html_node('em') %>% 
  html_text()

page_no <- gsub(',','',page_no)
page_no <- as.numeric(page_no)
page_no <- ifelse(page_no%%10, page_no%/%10+1,page_no%/%10)
page_no
for(page in 1 : page_no){
  if(page == 747){
    next
  }
  url <- paste0(url_main,page,'')
  fname <- sprintf('bakdu_review(%d).html',page)
  download.file(url,destfile = fname,quiet = T)
  review_temporary <- read_html(fname)

  #평점
  star_score <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.star_score') %>%
    html_text()
  star_score1 <- str_replace_all(star_score,'\r|\t|\n','')
  star_list <- append(star_list,star_score1)

  #리뷰
  # text <- review_temporary %>%
  #   html_nodes('.score_result') %>%
  #   html_nodes('.star_reple') %>%
  #   html_node('p') %>%
  #   html_text()
  # text1 <- str_replace_all(text,'\r|\t|\n','')
  # text_list <- append(text_list,text1)
  # text_list
  text <- review_temporary %>% 
    html_nodes(".score_result") %>% 
    html_nodes(".score_reple") %>% 
    html_node('p') %>%  
    html_text()
  text1 <- str_replace_all(text,"\r|\t|\n","")
  text_list <- append(text_list,text1)
  text_list
  
  #사용자
  userid <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.score_reple') %>%
    html_node('em') %>%
    html_text()
  userid1 <- str_replace_all(userid,'\r|\t|\n','')
  user_id_list <- append(user_id_list,userid1)

  #날짜
  user <-review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.score_reple') %>%
    html_node('dt') %>%  #말단의 노드에는 점없이해야함
    html_text() #html text 긁어오는 함수
  user1 <- str_replace_all(user,'\r|\t|\n','')
  date <- str_extract_all(user1,'[0-9]{4}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}')
  date1 <- unlist(date)
  date_list <- append(date_list,date1)

  #공감
  like <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.btn_area') %>%
    html_nodes('._sympathyButton') %>%
    html_node('strong') %>% 
    html_text()
  like_count <- str_replace_all(like,'\r|\t|\n','')
  like_list <- append(like_list,like_count)

  #비공감
  unlike <- review_temporary %>%
    html_nodes('.score_result') %>%
    html_nodes('.btn_area') %>%
    html_nodes('._notSympathyButton') %>%
    html_node('strong') %>% 
    html_text()
  unlike_count <- str_replace_all(unlike,'\r|\t|\n','')
  unlike_list <- append(unlike_list,unlike_count)

  cat("남은 페이지는", page_no-page)

}
length(user_id_list)
length(text_list)


str(star_list) 
str(text_list)
str(user_id_list )
str(date_list )
str(like_list )
str(unlike_list )
unlike_list[1]

####데이터 프레임 생성
review <- data.frame(리뷰어= user_id_list, 일시 = date_list,평점 = star_list, 리뷰 = text_list, 공감 = like_list, 비공감 = unlike_list)
View(review)

####파일 저장
#install.packages('writexl')
library(writexl)
.libPaths()
write_xlsx(review, path = 'C:/Users/09/Desktop/R/R_study/data/review.xlsx')

table(review$리뷰어)

#ID와 닉네임 분리하기
# ID만 분리
id<-c()
for(i in 1:length(review$리뷰어)){
  id[i]<-str_extract(review$리뷰어[i],'\\((.*?)\\)')
  id[i]<-gsub('[)(]',"",id[i])
  if(is.na(id[i])){
    id[i]<-review$리뷰어[i]
  }
}

review$리뷰어<-id
View(review)

write_xlsx(review, path = 'C:/Users/09/Desktop/R/R_study/data/review_id.xlsx')

#동일ID 빈도 보기
install.packages('readxl')
library(readxl)
aa <- read.xlsx(file = './data/review_id.xlsx',sheetIndex = 1,encoding =  'UTF-8')
View(aa)
review_order <- head(sort(table(aa),decreasing = T),30)
review_order

#댓글 부대의 제거 /첫 영화가 나온 날 제거/ 10점 제거/좋아요가 1000이상 제거/같은 아이디가 3회이상 댓글시 제거
v <- review_df %>% filter(n<=2) %>% filter(score != 10) %>%  filter(date >"2019.12.27 00:00") %>% filter(like <1000) %>% select(id, date, score,like)
v




















