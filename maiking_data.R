library(tidyverse)
library(ggplot2)
library(broom)

df<-read.csv('data.csv')

kyoto<-df %>% filter(prefecture=='京都')
osaka<-df %>% filter(prefecture=='大阪')
tokyo<-df %>% filter(prefecture=='東京')

df <- rbind(kyoto,osaka,tokyo)
df%>%group_by(prefecture)%>%summarise(n=n(),mean(score),sd(score))

make_random <- function(df,num){
  return(rnorm(num,mean(df$score),sd(df$score)))
  mean(df$score)
  var(df$score)
  sd(df$score)
}

print_stat <- function(df){
  return(rnorm(num,mean(df$score),sd(df$score)))
  mean(df$score)
  var(df$score)
  sd(df$score)
}

set.seed(123456789)
df_kyoto<-cbind(make_random(kyoto,2000),'kyoto')
df_osaka<-cbind(make_random(osaka,1800),'osaka')
df_tokyo<-cbind(make_random(tokyo,1800),'tokyo')

df<-rbind(df_kyoto,df_osaka,df_tokyo)
# df<-rbind(df_kyoto,df_tokyo)
df<-as.data.frame(df)
colnames(df) <- c('score','group')
df$score <- as.numeric(df$score)

df <- df%>%filter(group %in% c('osaka','tokyo'))

attach(df)
g <- ggplot(df, aes(x = score, fill = group))
g <- g + geom_density(position = "identity", bins = 100, alpha = 0.5)
# g <- g + scale_fill_npg()
plot(g)

df %>% group_by(group) %>% summarise(n = n(), avg = mean(score))

# df <- df%>%filter(group %in% c('osaka','tokyo'))
t.test(score~group, var.equal=TRUE,data = df) %>% tidy()

library(clipr)

write_clip(content = df, sep = NULL, eos = NULL)
