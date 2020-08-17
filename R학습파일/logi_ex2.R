########## 예제 mtcars ##########
# mtcars 데이터셋 이용 아래와 같이 작성해보세요 :)
#sqld 315 page
data(mtcars)
head(mtcars)
library(dplyr)
str(mtcars)
View(mtcars)
# 1) 데이터 준비
df <- mtcars %>% 
  dplyr::select(mpg,vs,am)
heda(df)
# 2) 8:2 데이터 나누기


# 3) 종속변수 vs, 독립변수 mpg + am로 모형 적합


# 4) 예측


# 5) 변수 선택
step

# 6) 결과 비교