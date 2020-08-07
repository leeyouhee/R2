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

barplot(review_order)

#빈도가 높다?? 이렇다고 과연 댓글 알바라고 할 수 있을까??!?!?

#평점의 분포를 알아보

library(stringr)
library()
idx <- which(str_detect(aa$리뷰,'알바'))
aa_alba <- aa[idx,]
str(aa_alba)
aa_alba[aa_alba$평점 == 1,3]
aa_alba[aa_alba$평점 == 10,3]
table(aa[idx,]$평점)
which(aa[idx,]$평점 ==1)

idx_temp <- which(aa[idx,]$평점 == 1)
idx_temp

##############################################강사님###########################
library(rvest)

library(stringr)

​

# 1. main url

​

main_url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=181381&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="

​

# 2. crawling(loop)

​

# 페이지수 확인

​

download.file(main_url, destfile = 'review_page.html', quiet = T)

review_page <- read_html('review_page.html')

page_num <- review_page %>% html_node('.score_total') %>% html_text('em')

page_num1 <- gsub("\r|\n|\t|[가-힣]+","",page_num)

page_num2 <- as.numeric(gsub(" +|,","",page_num1))

page_num3 <- ceiling(page_num2/10)

page_num3

​

score_list <- c()

review_list <- c()

reviewer_list <- c()

date_list <- c()

posi_list <- c()

nega_list <- c()

for(page in 1:page_num3){
  
  url <- paste0(main_url,page,"")
  
  filename <- sprintf('review(%d).html',page)
  
  download.file(url, destfile = filename, quiet = T)
  
  review_temp <- read_html(filename)
  
  filename
  
  # 평점
  
  score <- review_temp %>% html_nodes('.score_result') %>% html_nodes('.star_score') %>% html_text()
  
  score1 <- gsub("\r|\n|\t","",score)
  
  score_list <- append(score_list, score1)
  
  ​
  
  # 리뷰
  
  review <- review_temp %>% html_nodes('.score_result') %>% html_nodes('.score_reple') %>% html_node('p') %>% html_text()
  
  review1 <- gsub("\r|\n|\t","",review)
  
  review_list <- append(review_list, review1)
  
  ​
  
  # 게시자 / 게시일
  
  reviewer <- review_temp %>% html_nodes('.score_result') %>% html_nodes('.score_reple') %>% html_node('dt') %>% html_text()
  
  reviewer1 <- gsub("\r|\n|\t","",reviewer)
  
  # reviewer_list <- append(reviewer_list, reviewer1)
  
  ​
  
  # date11 <- substr(reviewer1,str_locate(reviewer1, "20")[,1],str_length(reviewer1))
  
  date11 <- substr(reviewer1,str_length(reviewer1)-15,str_length(reviewer1))
  
  date_list <- append(date_list,date11)
  
  ​
  
  reviewer_only <- character()
  
  ​
  
  for(i in 1:length(date11)){
    
    reviewer_only[i] <- gsub(date11[i],"",reviewer1[i])
    
  }
  
  ​
  
  reviewer_list <- append(reviewer_list, reviewer_only)
  
  ​
  
  # 공감 / 비공감
  
  posi_nega <- review_temp %>% html_nodes('.score_result') %>% html_nodes('.btn_area') %>% html_text()
  
  posi_nega1 <- gsub("\r|\n|\t","",posi_nega)
  
  posi <- str_extract(posi_nega1,'[0-9]{1,}')
  
  posi_list <- append(posi_list,posi)
  
  nega <- str_extract(posi_nega1,'[0-9]{1,}$')
  
  nega_list <- append(nega_list,nega)
  
  ​
  
  cat('남은 페이지는',page_num3-page,'\n')
  
  ​
  
}

​

### 3. create dataframe

​

review_df <- data.frame(reviewer = reviewer_list,score=score_list,review=review_list,
                        
                        like=posi_list,dislike=nega_list,date=date_list)

​

library(tibble)

review_tb <- as_tibble(review_df)

​

head(review_df,4)

str(review_tb)

​

# 데이터 저장

write.csv(review_df, 'D:\\rwork\\05ex\\06review\\review.csv', row.names = F)

​

### 형변환

​

review_tb$date <- strptime(review_tb$date,'%Y.%m.%d %H:%M')

str(review_tb)

review_tb$date[1]

review_df$date[1]

​

####################################################

​

review_tb <- read.csv(file = "D:\\lec_mat\\01_analysis\\R\\05ex\\06review\\review.csv",
                      
                      header = T, stringsAsFactors = F)

​

​

##### analysis #####

​

# 1. 평점 분포

​

tb <- review_tb

​

tb$score <- as.numeric(tb$score)

table(tb$score)

​

barplot(table(tb$score))

​

# 2. 동일 게시자? --> 의심 가능

​

a <- head(sort(table(tb$reviewer),decreasing = T),20)

a

​

tb[tb$reviewer=='love****',]

​

tb[tb$reviewer=='사바자다라안무(vast****)',]

​

tb[tb$reviewer=='김소정(ksjm****)',]

​

tb[tb$reviewer=='rlaw****',]

​

a_t <- head(sort(table(tb$reviewer),decreasing = T),20)[1]

a_t

​

table(df_test$date)

​

# 3. 평점 1점 중 공감 최다?

​

tb$like <- as.numeric(tb$like)

tb$dislikelike <- as.numeric(tb$dislike)

index1 <- order(tb[tb$score==1,]$like,decreasing = T)

​

tb[tb$score==1,][index1,][1:4,]

​

# 4. '알바'를 언급한 댓글 조사 

​

tb <- read.csv(file = 'D:\\rwork\\05ex\\06review\\review.csv', 
               
               header = TRUE, sep = ",")

str(tb)

library(stringr)

idx <- which(str_detect(tb$review,'알바'))

tb_alba <- tb[idx,]

str(tb_alba)

tb_alba[tb_alba$score == 1,3]

tb_alba[tb_alba$score == 10,3]

table(tb[idx,]$score)

which(tb[idx,]$score == 1)

​

​

idx_temp <- which(tb[idx,]$score == 1)

idx_temp







