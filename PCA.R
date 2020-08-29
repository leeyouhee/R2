library(dplyr)
library(ggplot2)

aa <- read.csv('./data/gg_zzin.csv', encoding = 'eu-kr', fileEncoding = "CP949", header = T)
df <- data.frame(aa)
str(df)
  dfs<- df[-c(1,2,3)]
View(dfs)
pairs(dfs,panel = panel.smooth,main ='suuu')
plot(dfs$final_score,dfs$g_ratio)
df.prin <- princomp(dfs, cor = T)
summary(df.prin)
screeplot(df.prin,npcs=9,type = 'lines')
loadings(df.prin)
df.prin$scores

####결과###3
유의한 결과를 얻지 못했다.

ggplot(dfs, aes(x=dfs$final_score, y=dfs$g_ratio)) +
  geom_point(shape=21, colour="black", size=3) + 
  coord_fixed() +
  scale_y_continuous(breaks=seq(0, 1, 0.001)) + 
  ggtitle("manual setting with fixed interval of x and y axis : scale_x_continuous(breaks=seq())")

