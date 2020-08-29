library(dplyr)
library(ggplot2)

aa <- read.csv('./data/change.csv', encoding = 'eu-kr', fileEncoding = "CP949", header = T)
df <- data.frame(aa)
dfs<- df[-c(1,2,7,8,11)]
View(dfs)
pairs(dfs,panel = panel.smooth,main ='suuu')

df.prin <- princomp(dfs, cor = T)
summary(df.prin)
screeplot(df.prin,npcs=9,type = 'lines')
loadings(df.prin)
df.prin$scores

####결과###3
유의한 결과를 얻지 못했다.