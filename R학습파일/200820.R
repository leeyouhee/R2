##연과분석(Association Analysis)##
연관규칙(장바구니 분석)
Package : arules::apriori()
특이 자료구조 생성(transaction)
모든 가능한 조합들에 대한 규칙 생성 [지지도, 신뢰도,향상도]
규칙확인 -> inspect()

#하나의 거래나 사건에 포함된 항목 간의 관련성을 파악하여
#둘 이상의 항목들로 구성된 규칙을 도출하는 탐색적 분석방법

#규칙이나 패턴을 찾는 것이 핵심
#레이블이 없으므로 비지도학습의 일종
#평가 메트릭 : 지지도, 신뢰도, 향상도
##지지도 : P(AnB) / 전체
##신뢰도 : 지지도 / P(A)
##향상도 : 신뢰도/P(B)

#활용분야 :
대형마트, 백화점, 쇼핑몰 등에서 고객의 장바구니에 들어 있는
품목간의 관계를 분석 
#특징
- 거래 사실이 기록된 트랜잭션 형식의 데이터셋 이용
- 사건과 사건의 연관성(뻔한 패턴을 의미하지 않음) --> 기저귀-맥주

#절차
1) 거래내역 데이터를 이용하여 트랜잭션 객체 생성
2) item과 transaction 관찰
3) metrics 활용 rules 발견
4) 결과 시각화
5) 결과 해석

#해석 방법
지지율이 낮다는 의미는 해당 조합의 거래 수가 적다는 의미
신뢰도가 낮으면 A 상품 구매시 B상품을 함께 구매하는 거래 수 적음
향상도 1이면 상품 간의 관게는 독립접 ; >1 : 양의 상관 ;  <1 : 음의 상관

#packages
install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)

#1. file load : transaction 생성
transc <- arules::read.transactions('./data/tran.txt',format='basket',sep=',')
transc

#2. 트랜잭션 데이터 보기 : inspect()
inspect(transc)

#3. 규칙 조사 : apriori
insight <- apriori(transc,parameter = list(supp = 0.3, conf = 0.1))
inspect(insight)

#4. 해석
1~5행 : 전체 6 트랜잭션 중 과일 포함 2, 맥주 포함2,고기포함4. 라면포함 4, 우유포함5
6~7 행 : 맥주와 우유 동시에 산 트랜잭션은 2회(supp = 0.33)
그 중 맥주를 산 사람이 우유를 산 트랜잭션은 2/2 conf = 1.0
반면 우유를 산 사람 중 맥주를 산 트랜잭션은 2/5 conf = 0.4
향상도 >1 이므로 맥주와 우유는 양의 상관관계 

#5. 기타 확인사항
summary(insight)

#6. plot
library(ggplot2)
plot(insight,method = 'grouped')
plot(insight, method = 'graph', control=list(type='items'))

library(dplyr)
# Groceries 데이터셋
data('Groceries')
str(Groceries)
Groceries

# data.frame으로 변환
Gdf <- as(Groceries,'data.frame')
head(Gdf)

#지지도 0.001, 신뢰도 0.8  이상을 만족하는 규칙 찾기
rules <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.8))

#규칙을 구성하는 왼쪽(LHS) -> 아이템을 보여주는 RSSH 의 빈도수
plot(rules,method='grouped')

#Confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules <- sort(rules,decreasing = T,by = 'confidece')
inspect(rules)
inspect(head(rules,10))

rules <- sort(rules, decreasing = T, by = 'lift')
inspect(rules)
inspect(head(rules,2))

#벌견된 규칙 시각화
plot(rules,method = 'graph')

# 실습 : 특정 상품으로 서브 셋 작성과 시각화
# 오른쪽 상품이 전지분유(whole milk)인 규치만 서브 셋으로 작성
wilk <- subset(rules,rhs %in% 'whole milk')
wilk

inspect(head(wilk,10))
inspect(tail(wilk,10))
plot(wilk,method = 'graph')

#오른쪽 item이 vegetables인 규칙만 서브 셋으로 작성
oveg <- subset(rules, rhs %in% 'other vegetables')
oveg
inspect(oveg)
plot(oveg,method = 'graph')

#오른쪽 item이 vegetables단어가 포함된 규칙만 서브 셋으로 작성
oveg1 <- subset(rules, rhs %pin% 'vegetables')
oveg1
inspect(oveg1)
plot(oveg1,method = 'graph')

#왼쪽 item이 butter 또는 yogurt인 규칙만 서브 셋으로 작성
by <- subset(rules, lhs %in% c('butter', 'yogurt'))
by
inspect(by)
plot(by,method = 'graph')

by1 <- subset(by,confidence >= 0.9 & lift >= 5)
plot(by1,method = 'graph')

#세부 거래품목 빈도확인 itemFrequency
sort(itemFrequency(Groceries,type='absolute'),decreasing = T)
arules::itemFrequencyPlot(Groceries,topN=10,type='absolute')
itemFrequencyPlot(Groceries,topN=10,type='relative')

groc <- apriori(Groceries,parameter = list(supp=0.005,
                                           conf=0.5,
                                           minlen=2))
inspect(groc)

## 첫번째

# 1.Adult 데이터셋을 로드하세요

​

# 2. support 0.5 confidence 0.9로 연관규칙 생성

​

# 3. lift 기준 정렬하여 상위 10개 규칙 확인

​

# 4. 위 결과를 연관어 네트워크 시각화

​

# 5. 3의 결과를 LHS와 RHS의 빈도수 시각화 표현

​

​

## 두번째

​

# 1. 데이터 생성

itms <- list(c("삼겹살","생수",'소주','과자'),
             
             c("삼겹살","생수",'소주','사과'),
             
             c("장어","생수",'소주','양파'),
             
             c("땅콩","생수",'맥주','오이'),
             
             c("땅콩","생수",'맥주','김'))

​

# 2. 트랜잭션 변환

itms.tr <- as(itms, 'transactions')

​

# 3. 트랜잭션 확인

​

# 4. 지지도 0.1, 신뢰도 0.8 이상인 연관성 규칙 구하기

​

# 5. 결과 검토

# 1) 도출된 규칙 5개 확인

# 2) 향상도가 1.2이상인 규치만 선택하여 내림차순 정렬 보기

# 3) lhs에 삼겹살이 포함된 연관성 규칙

# 4) lhs에 삼겹살과 과자 동시에 포함하는 규칙(%ain%)

# 5) lhs에 삼겹살 또는 과자 또는 삼겹살&과자 동시 포함 하는 규칙(%oin%)

# 6) it.rules에 대해 지지도 0.2 이상인 항목의 빈도수 그래프 그리기



