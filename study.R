###############
#爬虫espn网址，得到07-17年gamelog
#包含季后赛，不包含加时赛
#处理比赛时间的函数
change <- function(x) {
  n<-length(x)
  x<-strsplit(x,split=":")
  x<-unlist(x)
  x<-as.numeric(x)
  y<-c()
  for(i in 1:n){
    m<-x[2*i-1]
    s<-x[2*i]
    y[i]<-60*m+s
  }
  y<-unlist(y)
  return(y)
}

library(rvest)
data2<-NULL
number<-c(400828894:400829117)
#爬虫，得到gamelog
for(i in number){
  string<-"http://www.espn.com/nba/playbyplay?gameId="
  www<-paste(string,i,sep="")
  web<-read_html(www,encoding ="UTF-8")
  a<-web %>%html_nodes("table") %>%html_table(fill=TRUE)
  if(length(a)<3){
    next
    }else{
    data<-NULL
    for(j in 2:5){
      b<-a[j]
      b<-as.data.frame(b)
      d<-as.character(b$time)
      d<-change(d)
      d<-720*(j-1)-d
      b$time<-d
      data<-rbind(data,b)
    }
    data<-data[-2,]
    data$TEAM<-NULL
    data$NA.<-NULL
    names1<-web %>% html_nodes("span.long-name") %>% html_text()
    names2<-web %>% html_nodes("span.short-name") %>% html_text()
    away<-paste(names1[1],names2[1],sep = " ")
    home<-paste(names1[2],names2[2],sep=" ")
    data<-data.frame(data,Away=away,Home=home,gameId=i)
    print(i)
    data2<-rbind(data2,data)
    }
}
##处理分数score
library(stringr)
data2<-read.csv("2015-16gamelog.csv",header = T,stringsAsFactors = F)
score<-data2$SCORE
score<-unlist(strsplit(score," - "))
away_score<-score[c(TRUE,FALSE)]
home_score<-score[c(FALSE,TRUE)]
data2$SCORE<-NULL
data2<-cbind(data2,away_score,home_score)
str(data2)
data2$away_score<-as.numeric(as.character(data2$away_score))
data2$home_score<-as.numeric(as.character(data2$home_score))

###################
log<-read.csv("2015-16gamelog.csv",header = T,stringsAsFactors = F)
log<-data2
log$score_diff<-log$away_score-log$home_score

#去除分差一样的数据
n<-length(log$gameId)
diff<-log$score_diff
delete<-NULL
for(i in 1:(n-1)){
  if(diff[i]==diff[i+1]){
    delete<-c(delete,i+1)
  }
}
log<-log[-delete,]
#去除罚球，时间相同 
n<-length(log$gameId)
time<-log$time
delete<-NULL
for(i in 1:(n-1)){
  if(time[i]==time[i+1]){
    delete<-c(delete,i)
  }
}
log<-log[-delete,]

# get the momentum  斜率0.06
#还包括没有momentum的比赛
m<-unique(log$gameId)
momentum<-NULL
no_momentum<-NULL#得到没有momentum的gameID
for(k in m){
  data1<-subset(log,gameId==k)
  Away<-data1$Away[1]
  Home<-data1$Home[1]
  score<-data1$score_diff
  time<-data1$time
  n<-length(time)
  vector.slope<-NULL
  startpoint<-NULL
  endpoint<-NULL
  from<-NULL
  for(j in seq(from=1,to=n-2,by=2)){
    for(i in 1:(n-j)){
      if(time[i+j]-time[i] <90 | time[i+j]-time[i] >360){
        next
      }else{
        line.slope<-(score[i+j]-score[i])/(time[i+j]-time[i])
        if(abs(line.slope)>0.06){
          vector.slope<-c(vector.slope,line.slope)
          startpoint<-c(startpoint,time[i])
          endpoint<-c(endpoint,time[i+j])
          if(sign(line.slope)==1){
            from<-c(from,Away)
          }else{
            from<-c(from,Home)
          }
        }
      }
    }
  }
  if(identical(startpoint,NULL)){
    frame<-data.frame(game_id=k,startpoint=0,endpoint=0,slope=0,Away=Away,Home=Home,from=0)
    no_momentum<-rbind(no_momentum,frame)
    next
  }else{
    frame<-data.frame(game_id=k,startpoint,endpoint,slope=vector.slope,Away=Away,Home=Home,from=from)
    frame<-frame[order(startpoint,endpoint),]
    momentum<-rbind(momentum,frame)
  }
}
#去除momentum相交叉时，斜率较小的
#1去除时间相互包容，斜率较小的
mom<-momentum
n<-length(mom$game_id)
count<-NULL
start<-mom$startpoint
end<-mom$endpoint
slope<-mom$slope
gameid<-mom$game_id
for(i in 1:(n-1)){
  if(start[i+1] > start[i] & start[i+1] < end[i] & gameid[i]==gameid[i+1]){
    if(slope[i]>slope[i+1]){
      count<-c(count,i+1)
    }else{
      count<-c(count,i)
    }
  }else{
    next
  }
}

mom<-mom[-count,]

#2去除相同时间起点或终点，选最大斜率
n<-length(mom$game_id)
count<-NULL
start<-mom$startpoint
end<-mom$endpoint
slope<-mom$slope
for(i in 1:(n-1)){
  if(start[i+1] == start[i] | end[i+1] == end[i]){
    if(slope[i]>slope[i+1]){
      count<-c(count,i+1)
    }else{
      count<-c(count,i)
    }
  }else{
    next
  }
}

mom<-mom[-count,]
#将有momentum和没有monmentum合并起来
mom<-rbind(mom,no_momentum)
mom<-mom[order(mom$game_id),]
library(data.table)
mom<-data.table(mom)
#得到每个队伍的momentum
team<-c("Atlanta Hawks","Brooklyn Nets","Boston Celtics","Charlotte Hornets",
        "Chicago Bulls","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
        "Detroit Pistons","Golden State Warriors","Houston Rockets",
        "Indiana Pacers","LA Clippers","Los Angeles Lakers","Memphis Grizzlies",
        "Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
        "New Orleans Pelicans","New York Knicks","Oklahoma City Thunder",
        "Orlando Magic","Philadelphia 76ers","Phoenix Suns","Portland Trail Blazers",
        "Sacramento Kings", "San Antonio Spurs","Toronto Raptors","Utah Jazz",
       "Washington Wizards")
short<-c("atl","brk","bos","cha","chi","cle","dal","den","det",
        "gsw","hou","ind","lac","lal","mem","mia","mil","min",
        "nop","nyk","okc","orl","phi","pho","por","sac","sas",
        "tor","uta","was")
result<-NULL
for (i in 1:30) {
  atl<-mom[mom$Away==team[i] | mom$Home==team[i],]
  atl$slope=ifelse(atl$from==team[i],1,0)
  atl$slope[is.na(atl$slope)]<-0
  atl<-atl[,.(count = sum(slope)),by=.(game_id)]$count
  if(length(atl)!=82){
    print(i)
  }
  result<-cbind(result,atl)
}
result<-t(result)
rownames(result)<-short
write.csv(result,"result.csv")
###以斜率为Y
#############################
#例子########################
a<-data[data$playId==400827888,]
n<-length(a$time)
time<-a$time
away<-a$away_score
home<-a$home_score
diff<-a$score_diff
for(i in 1:(n-1)){
  if(time[i]!=time[i+1]){
    d=time[i]+1
    e=time[i+1]-1
    for(j in d:e){
      f<-data.frame(time=j,playId=400827888,away_score=away[i],home_score=home[i],score_diff=diff[i])
      a<-rbind(a,f)
    }
  }
}
a<-read.csv("C:/Users/Administrator/Documents/a.csv",header=TRUE)
#累计分数图
ggplot(data = a, aes(x = time)) +geom_line(aes(y = away_score, colour = "Away")) +geom_line(aes(y = home_score, colour = "Home")) +scale_colour_manual("", breaks = c("Away", "Home"),values = c("Home"="green", "Away"="red")) +
  xlab(" ") +scale_y_continuous("score") + labs(title="累计得分图")
library(easyGgplot2)
ggplot(data = a, aes(x = time)) +geom_line(aes(y = away_score, colour = "客队")) +geom_line(aes(y = home_score, colour = "主队"))+xlab("时间")+ylab("分数")+ggtitle("累计分数图")
plot1<-plot2<-qplot(time,score_diff, data = a, geom = "line",colour="red",main ="差分图",xlab="时间",ylab="分差")
ggplot2.multiplot(plot1,plot2,cols=2)
#分差图
ggplot(data=a,aes(x=time,y=score_diff,colour = "red"))+geom_line()+labs(title="主客队分差图")
#画出两个分差图
plot1<-qplot(time,score_diff, data = a, geom = "line",colour="red",xlab="时间",ylab="分差")
plot2<-ggplot(data=a,aes(x=time,y=score_diff,colour = "red"))+geom_line()+geom_point(size=2, shape=20, colour = "blue")+xlab("时间")+ylab("分差")
ggplot2.multiplot(plot1,plot2,cols=2)


