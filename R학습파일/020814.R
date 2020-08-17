#지도학습(Supervised Learning)
weather <- read.csv('./data/weather.csv', stringsAsFactors = F)
summary(weather)
weather <- na.omit(weather)
summary(weather)
dim(weather)
head(weather)
str(weather)

#2. chr 칼럼, Date, RainTday 칼럼 제거
weather_df <- weather[,c(-1,-6,-8,-14)]
str(weather_df)
head(weather_df,2)

#3. RaniTomorrow 칼럼 -> 로지스틱 회구분석 label1 --> (0,1)
weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)
table(weather_df$RainTomorrow)

#4. 데이터 분할
nrow => 데이터프레임에서 데이터 길이를 알 수 있다.
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
length(idx)
floor(nrow(weather_df)*0.7)
length(idx) == floor(nrow(weather_df)*0.7) #251

nrow(weather_df)
train <- weather_df[idx,]
test <- weather_df[-idx,] #251개 뺀것
length(test)
#5. 모델 생성 : 학습 데이터
weather_model <- glm(RainTomorrow ~., data = train, family = 'binomial')
str(weather_model)
summary(weather_model)

#6. 변수선택
step1 <- step(object = weather_model, trace =  F , direction = 'backward') #변수 소거
step1
summary(step)

#newdata =test : 새로운 데이터 셋, type ='response' : 0~1 확률값으로 예측
pred <- predict(weather_model, newdata = test, type = 'response') #확률값 제공
pred
summary(pred)

#예측결과 분류 및 정확도 확인
result_pred <-ifelse(pred>=0.5,1,0)
result_pred
table(result_pred, test$RainTomorrow)

#newdata =test : 새로운 데이터 셋, type ='response' : 0~1 확률값으로 예측
pred_step <- predict(step1, newdata = test, type = 'response')
pred_step
summary(pred_step)

#예측결과 분류 및 정확도 확인
result_preds <-ifelse(pred_step>=0.5,1,0)
result_preds
table(result_preds, test$RainTomorrow)











