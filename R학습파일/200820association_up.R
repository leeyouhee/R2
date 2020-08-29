####### 연관분석(Association Analysis) #######

# 하나의 거래나 사건에 포함된 항목 간의 관련성을 파악하여
# 둘 이상의 항목들로 구성된 규칙을 도출하는 탐색적 분석방법
# 
## 규칙이나 패턴을 찾는 것이 핵심
## 레이블이 없으므로 비지도학습의 일종
## 평가 메트릭 : 지지도(support), 신뢰도(confidence), 향상도(lift)
## 지지도 : A,B가 동시 포함된 거래수/전체거래수
## 신뢰도 : A,B가 동시 포함된 거래수/A를 포함하는 거래수
## 향상도 : A,B가 동시 포함된 거래수/(A 포함)*(B 포함)
## 향상도 : 신뢰도 / B의 지지도

# 
## 활용분야
# 대형마트, 백화점, 쇼핑몰 등에서 고객의 장바구니에 들어 있는
# 품목 간의 관계를 분석 --> 마케팅 활용
# 
## 특징
# - 거래사실이 기록된 트랜잭션 형식의 데이터셋 이용
# - 사건과 사건의 연관성(뻔한 패턴을 의미하지 않음) --> 기저귀-맥주
# 
## 절차
# 1) 거래내역 데이터를 이용하여 트랜잭션 객체 생성
# 2) item과 transaction 관찰
# 3) metrics 활용 rules 발견
# 4) 결과 시각화
# 5) 결과 해석

## 해석 방법

# 지지율이 낮다는 의미는 해당 조합의 거래 수가 적다는 의미
# 신뢰도가 낮으면 A 상품 구매시 B 상품을 함께 구매하는 거래 수 적음
# 향상도 1이면 상품 간의 관계는 독립적 >1 : 양의 상관 <1 : 음의 상관
## packages ###
install.packages('arules') # Adult, AdultUCI datasets
install.packages('arulesViz')
library(arules)
library(arulesViz)
# update.packages("arules")
################

# 1. file load : transaction 객체 생성

transac <- arules::read.transactions("D:\\lec_mat\\01_analysis\\R\\04ml\\association\\tran.txt",
                                     format = 'basket', sep=",")
transac

# 2. 트랜잭션 데이터 보기 : inspect()
inspect(transac)
# items           
# [1] {라면,맥주,우유}
# [2] {고기,라면,우유}
# [3] {고기,과일,라면}
# [4] {고기,맥주,우유}
# [5] {고기,라면,우유}
# [6] {과일,우유}

# 3. 규칙 조사 : apriori()
insight <- apriori(transac, parameter = list(supp = 0.3, conf=0.1))
inspect(insight)

# 4. 해석
# 1~5행 : 전체 6 트랜잭션 중 과일 포함 2, 맥주 포함 2, 고기 포함 4,
# 라면 포함 4, 우유 포함 5
# 6~7행 : 맥주와 우유 동시에 를 산 트랜잭션은 2회(/6 -> supp=0.33)
# 그 중 맥주를 산 사람이 우유를 산 트랜잭션은 2/2 --> conf = 1.0
# 반면 우유를 산 사람 중 맥주를 산 트랜잭션은 2/5 -> conf = 0.4
# 향상도 > 1 이므로 맥주와 우유는 양의 상관관계가 있다

# 5. 기타 확인사항
summary(insight)

# 6. plot
plot(insight,method="grouped")
plot(insight, method='graph', control = list(type='items'))


################ part 2 ################

# Groceries 데이터셋
data("Groceries")
str(Groceries)
Groceries

# data.frame으로 변환
Groceries.df <- as(Groceries, "data.frame")
head(Groceries.df)

# 지지도 0.001, 신뢰도 0.8 이상을 만족하는 규칙 찾기
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules
# 규칙을 구성하는 왼쪽(LHS) -> 오른쪽(RHS)의 item 빈도수 보기 
plot(rules, method = "grouped")

# Confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules <- sort(rules, decreasing = T, by = "confidence")
inspect(rules)
inspect(head(rules,10))

rules1 <- sort(rules, decreasing = T, by = "lift")
inspect(rules1)
inspect(head(rules1,10))

# 발견된 규칙 시각화 
library(arulesViz)
plot(rules, method = "graph")

# 실습: 특정 상품(item)으로 서브 셋 작성과 시각화 
# 오른쪽 item이 전지분유(whole milk)인 규칙만 서브 셋으로 작성
wmilk <- subset(rules, rhs %in% 'whole milk')
wmilk

inspect(head(wmilk,10))
inspect(tail(wmilk,10))
plot(wmilk, method = "graph")

# 오른쪽 item이 other vegetables인 규칙만 서브 셋으로 작성
oveg <- subset(rules, rhs %in% 'other vegetables')
oveg
inspect(oveg)
plot(oveg, method = "graph")

# 오른쪽 item이 vegetables 단어가 포함된 규칙만 서브 셋으로 작성
oveg1 <- subset(rules, rhs %pin% 'vegetables')
oveg1
inspect(oveg)

# 왼쪽 item이 butter 또는 yogurt인 규칙만 서브 셋으로 작성
butter_yogurt <- subset(rules, lhs %in% c('butter', 'yogurt'))
butter_yogurt
inspect(butter_yogurt)

plot(butter_yogurt, method = "graph")

butter_yogurt1 <- subset(butter_yogurt, confidence >= 0.9 & lift >= 5)
butter_yogurt1
inspect(butter_yogurt1)

plot(butter_yogurt1, method = "graph")


## cf) 세부 거래품목 빈도확인 itemFrequency
sort(itemFrequency(Groceries,type="absolute"),decreasing = T)
arules::itemFrequencyPlot(Groceries,topN=10,type="absolute")
itemFrequencyPlot(Groceries,topN=10,type="relative")

groc <- apriori(Groceries,parameter = list(supp=0.005,
                                           conf=0.5,minlen=2))
inspect(groc)

####################### 연습 #######################

## 예제

# 1.Adult 데이터셋을 로드하세요
data(Adult)
Adult

# 2. support 0.5 confidence 0.9로 연관규칙 생성
adult <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9))
adult
inspect(sort(adult, by = 'confidence', decreasing = T))

# 3. lift 기준 정렬하여 상위 10개 규칙 확인
adult.rules <- head(sort(adult, by='lift', decreasing = T),10)
inspect(adult.rules)

# 4. 위 결과를 연관어 네트워크 시각화
plot(adult.rules, method = 'graph')

# 5. 3의 결과를 LHS와 RHS의 빈도수 시각화 표현
plot(adult.rules, method = 'grouped')

# 기타
adult.rules1 <- apriori(Adult, 
                        parameter = list(supp = 0.1, conf = 0.6),
                        appearance = list(rhs=c('income=small','income=large'),
                                          default='lhs'))
adult.rules1.sorted <- sort(adult.rules1, by='lift')
inspect(head(adult.rules1.sorted))
inspect(tail(adult.rules1.sorted))

## 예제2

# 1. 데이터 생성
itms <- list(c("삼겹살","생수",'소주','과자'),
             c("삼겹살","생수",'소주','사과'),
             c("장어","생수",'소주','양파'),
             c("땅콩","생수",'맥주','오이'),
             c("땅콩","생수",'맥주','김'))

# 2. 트랜잭션 변환
itms.tr <- as(itms, 'transactions')

# 3. 트랜잭션 확인
inspect(itms.tr)

# 4. 지지도 0.1, 신뢰도 0.8 이상인 연관성 규칙 구하기
it.rules <- apriori(itms.tr, parameter=list(supp=0.1, conf=0.8))
it.rules
# 5. 결과 검토
# 1) 도출된 규칙 5개 확인
# 2) 향상도가 1.2이상인 규치만 선택하여 내림차순 정렬 보기
# 3) lhs에 삼겹살이 포함된 연관성 규칙
# 4) lhs에 삼겹살과 과자 동시에 포함하는 규칙(%ain%)
# 5) lhs에 삼겹살 또는 과자 또는 삼겹살&과자 동시 포함 하는 규칙(%oin%)
# 6) itms.rules에 대해 지지도 0.2 이상인 항목의 빈도수 그래프 그리기

# 5. 결과 검토
# 1) 도출된 규칙 5개 확인
inspect(it.rules[1:5])
# 2) 향상도가 1.2이상인 규칙만 선택하여 내림차순 정렬하고 상위 5개 보기
a <- subset(it.rules, lift>=1.2)
aa <- head(sort(a, by='lift', decreasing = T ),5)
inspect(aa)
# 3) lhs에 삼겹살이 포함된 연관성 규칙
inspect(subset(it.rules, lhs %in% c('삼겹살')))
# 4) lhs에 삼겹살과 과자 동시에 포함하는 규칙(%ain%)
inspect(subset(it.rules, lhs %ain% c('삼겹살','과자')))
# 5) lhs에 삼겹살 또는 과자 또는 삼겹살&과자 동시 포함 하는 규칙(%oin%)
inspect(subset(it.rules, lhs %oin% c('삼겹살','과자')))
# 6) itms.tr에 대해 지지도 0.2 이상인 항목의 빈도수 그래프 그리기
itemFrequencyPlot(itms.tr, support=0.2, type='relative')





