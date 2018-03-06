
#2016年NBA花名册
library(rvest)
library(xml2)
url<-c("http://www.altiusdirectory.com/Sports/nba-boston-celtics-roster.html",
      "http://www.altiusdirectory.com/Sports/nba-golden-state-warriors-roster.html",
      "http://www.altiusdirectory.com/Sports/nba-new-jersey-nets-roster.html")
total<-data.frame()
for (i in 1:30) {
doc<-read_html(url[i],encoding="UTF-8")
a<- doc %>% html_nodes("table") %>% html_table(fill=TRUE) %>% unique()
a<-as.data.frame(a[2])
names(a) <- a[1,]
a <- a[-1,]

a$POS<-NULL
a$NA.<-NULL
d<-nrow(a)
a<-a[-d,]
##队的标签
b<-doc%>% html_nodes("span.active")%>%html_text()
b<-sub('...............$','',b)
a<-data.frame(a,Team=b)

total<-rbind(total,a)
}
total$NA.<-NULL
write.table (total, file ="bs.csv", sep ="  ", row.names =FALSE, quote =FALSE)



change <- function(x) {
  n<-length(x)
  x<-strsplit(x,split=":")
  x<-unlist(x)
  x<-as.numeric(x)
  for(i in 1:n){
    m<-x[2*i-1]
    s<-x[2*i]
    y[i]<-60*m+s
  }
  y<-unlist(y)
  return(y)
}

library(rvest)
library(xml2)
data2<-NULL
number<-c(400900170:400900235)
for(i in number){
  string<-"http://www.espn.com/nba/playbyplay?gameId="
  www<-paste(string,i,sep="")
  web<-read_html(www,encoding ="UTF-8")
  a<-web %>%html_nodes("table") %>%html_table(fill=TRUE)
  if(a[6]=='NULL' || a[8]!='NULL'){
    next
  }else{
    sec_time<-NULL
    for(j in 2:5){
      b<-a[j]
      b<-as.data.frame(b)
      d<-as.character(b$time)
      d<-change(d)
      d<-720*(j-1)-d
      sec_time<-c(sec_time,d)
    }
    SCORE<-web %>%html_nodes("td.combined-score") %>%html_text()
    SCORE<-strsplit(SCORE,split ="-")
    SCORE<-unlist(SCORE)
    m<-length(SCORE)
    if(m>1000){
      next
    }
    away_score<-SCORE[seq(1,m,by=2)]
    home_score<-SCORE[seq(2,m,by=2)]
    data<-data.frame(game_id=i,sec_time,away_score,home_score)
  }
  print(i)
  data2<-rbind(data2,data)
}
write.csv(data2,"2016-17gamelog4.csv",row.names = F)



2016number<-c((400828377:400829117),(400874380:400874389),(400874410:400874423),
              (400875765:400875768),(400875806:400875810),(400875890:400875896),
              (400876750:400876756),(400876891:400876896),(400878154:400878160),
              (400889799:400889809),(400897108:400897120),(400897250:400897299),
              (400898738:400898749),(400899094:400899189),(400899361:400899363),
              (400899375:400899949),(400903490:400903493),(400905354:400905366))

2015number<-c((400578774:400579522),400606285,(400790902:400790966),
              (400793112:400793126),(400792888:400792894),
              (400793781:400793786),(400793861:400793866),
              (400796266:400796269),(400796355:400796359),
              (400796845:400796850),(400827888:400828376),
              (400830052:400830236),(400833757:400834298))
2014number<-c((400489341:400490103),400517994,400532894,400528655,
              (400553054:400553108),(400556174:400556180),
              (400556254:400556259),(400556274:400556278),
              (400556334:400556338),(400558914:400558959),
              (400559374:400559378),(400578293:400578772),
              (400583897:400584034))
2013number<-c((400278178:400278292),(400278294:400278950),400436572,
              (400459775:400459793),(400459943:400459980),
              (400464174:400464192),(400464337:400464359),
              (400464455:400464469),(400466474:400466600),
              (400467334:400467339),(400491494:400491609),
              (400488874:400489340))
2012number<-c((400277721:400278176),(400400461:400400575))

#计算技术指标

log<-read.csv("2014log.csv",header = TRUE)
log<-as.data.frame(log)
data2<-NULL
number<-c((400489341:400490103),400517994,400532894,400528655,
          (400553054:400553108),(400556174:400556180),
          (400556254:400556259),(400556274:400556278),
          (400556334:400556338),(400558914:400558959),
          (400559374:400559378),(400578293:400578772),
          (400583897:400584034))
library(stringr)
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
play<-as.character(information$PLAY) #比赛记录
away<-vector(mode="numeric",length=0)
home<-vector(mode="numeric",length=0)
newtime<-vector(mode="numeric",length=0)
event<-0
roster<-read.csv("2014-roster.csv",header = TRUE)#花名册
Away1<-as.character(information$Away[2])
Away2<-(subset(roster,TEAM==Away1))$NAME #提取客队的球员名字
Away2<-as.character(Away2)
Home1<-as.character(information$Home[2])
Home2<-(subset(roster,TEAM==Home1))$NAME #提取主队的球员名字
Home2<-as.character(Home2)
for(j in 1:length(play)){
  if (str_detect(play[j],'Sec. timeout')=='TRUE'){  #技术指标
    event<-(event+1)
    newtime[event]<-information$time[j]
    opt<-c(TRUE %in% str_detect(play[j],Away2))
    if(opt=='TRUE'){
      away[event]<-1
      home[event]<-0
    }else{
      away[event]<-0
      home[event]<-1
    }
  }else{
    next
       }
}
 if(event==0){
  next
 }else{
 data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
 data2<-rbind(data2,data1)
 }
 # data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
 # data2<-rbind(data2,data1)
}
}
write.csv(data2,"Sec-timeout.csv")




######################################################3
################################################################
######################################################
#技术指标 blocks
log<-read.csv("2016-17log.csv",header = TRUE)
log<-as.data.frame(log)
library(stringr)
year<-read.csv("number.csv",header = TRUE)
number<-c(400899375:400900235)

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'blocks')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        b<-strsplit(play[j],split ="blocks")
        b<-unlist(b)
        opt1<-c(TRUE %in% str_detect(b[1],Away2))
        opt2<-c(TRUE %in% str_detect(b[1],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
      data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
      data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17blocks.csv")

#技术指标 assists

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'assists')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
      data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
      data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17assists.csv")



#技术指标 offensive rebound

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'offensive rebound')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17offensive-rebound.csv")

#技术指标 defensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'defensive rebound')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17defensive-rebound.csv")




#技术指标 makes shot 

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'makes')=='TRUE' && 
          (str_detect(play[j],'shot')=='TRUE' || 
           str_detect(play[j],'jumper')=='TRUE'|| 
           str_detect(play[j],'layup')=='TRUE'||
           str_detect(play[j],'dunk')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17makes-shot.csv")


#技术指标 makes two point shot\jumper

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'makes')=='TRUE' &&
          (str_detect(play[j],'two point shot')=='TRUE' ||
           str_detect(play[j],'layup')=='TRUE' ||
           str_detect(play[j],'dunk')=='TRUE' ||
           str_detect(play[j],'tip shot')=='TRUE' ||
           str_detect(play[j],'bank shot')=='TRUE' ||
           str_detect(play[j],'foot jumper')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17makes-two-point-shot.csv")


#技术指标 makes three point shot\jumper

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'makes')=='TRUE' &&
          (str_detect(play[j],'three point shot')=='TRUE' ||
           str_detect(play[j],'three point jumper')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    }
  }
  }
write.csv(data2,"2016-17makes-three-point-shot.csv")


#技术指标 misses shot/jumper

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'misses')=='TRUE' && 
          (str_detect(play[j],'shot')=='TRUE' || 
           str_detect(play[j],'jumper')=='TRUE'|| 
           str_detect(play[j],'layup')=='TRUE'||
           str_detect(play[j],'dunk')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17miss-shot.csv")

#技术指标 misses two point shot/jumper

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'misses')=='TRUE' && 
          (str_detect(play[j],'two point shot')=='TRUE' || 
           str_detect(play[j],'foot jumper')=='TRUE'|| 
           str_detect(play[j],'two point jumper')=='TRUE'|| 
           str_detect(play[j],'bank shot')=='TRUE'|| 
           str_detect(play[j],'tip shot')=='TRUE'|| 
           str_detect(play[j],'layup')=='TRUE'||
           str_detect(play[j],'dunk')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17miss-two-point.csv")

#技术指标 misses three shot/jumper


data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'misses')=='TRUE' && 
          (str_detect(play[j],'three point shot')=='TRUE' || 
           str_detect(play[j],'three point jumper')=='TRUE')){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
      data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
      data2<-rbind(data2,data1)
    }
  }
  }
write.csv(data2,"2016-17miss-three-point.csv")



#技术指标 steals

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'steals')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        b<-strsplit(play[j],split ="\\(")
        b<-unlist(b)
        opt1<-c(TRUE %in% str_detect(b[2],Away2))
        opt2<-c(TRUE %in% str_detect(b[2],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    
  }
}
write.csv(data2,"2016-17steals.csv")



#技术指标 makes free throw

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) #比赛记录
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)#花名册
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME #提取客队的球员名字
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME #提取主队的球员名字
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'makes free throw')=='TRUE'){  #技术指标
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17makes-free-throw.csv")


#技术指标 misses free throw
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME 
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'misses free throw')=='TRUE'){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
      data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
      data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17misses-free-throw.csv")

#技术指标 turnover

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'turnover')=='TRUE' ||
          str_detect(play[j],'bad pass')=='TRUE' ||
          str_detect(play[j],'traveling')=='TRUE'||
          str_detect(play[j],'violation')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        b<-strsplit(play[j],split ="\\(")
        b<-unlist(b)
        opt1<-c(TRUE %in% str_detect(b[1],Away2))
        opt2<-c(TRUE %in% str_detect(b[1],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    
  }
}
write.csv(data2,"2016-17turnover.csv")


#技术指标 foul
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'foul')=='TRUE' ||
          str_detect(play[j],'makes technical free throw')=='TRUE'){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        b<-strsplit(play[j],split ="\\(")
        b<-unlist(b)
        opt1<-c(TRUE %in% str_detect(b[1],Away2))
        opt2<-c(TRUE %in% str_detect(b[1],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    
  }
}
write.csv(data2,"2016-17foul.csv")


#技术指标 draws the foul
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'draws the foul')=='TRUE'){ 
        event<-(event+1)
        newtime[event]<-information$time[j]
        b<-strsplit(play[j],split ="\\(")
        b<-unlist(b)
        opt1<-c(TRUE %in% str_detect(b[2],Away2))
        opt2<-c(TRUE %in% str_detect(b[2],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    
  }
}
write.csv(data2,"2016-17draw-the-foul.csv")

#计算技术指标  personal-foul bonus
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("2016-17roster.csv",header = TRUE) 
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$NAME
    Away2<-as.character(Away2)
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$NAME 
    Home2<-as.character(Home2)
    for(j in 1:length(play)){
      if (str_detect(play[j],'personal foul')=='TRUE'){ 
        if (str_detect(play[j+1],'makes free throw')=='TRUE'){
          event<-(event+1)
          newtime[event]<-information$time[j]
          opt1<-c(TRUE %in% str_detect(play[j],Away2))
          opt2<-c(TRUE %in% str_detect(play[j],Home2))
          if(opt1=='TRUE' && opt2=='FALSE'){
            away[event]<-1
            home[event]<-0
          }
          if(opt1=='FALSE' && opt2=='TRUE'){
            away[event]<-0
            home[event]<-1
          }
          if(opt1=='FALSE' && opt2=='FALSE'){
            away[event]<-0
            home[event]<-0
          }
          if(opt1=='TRUE' && opt2=='TRUE'){
            away[event]<-1
            home[event]<-1
          }
        }else{
          next
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17bonus.csv")

#技术指标 team rebound
###################################################
#####################################################
##########################################################
#技术指标offensive team rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("useful.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$long
    Away2<-as.character(Away2[1])
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$long
    Home2<-as.character(Home2[1])
    for(j in 1:length(play)){
      if (str_detect(play[j],'offensive team rebound')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17offensive-team-rebound.csv")


#技术指标deffensive team rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("useful.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$long
    Away2<-as.character(Away2[1])
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$long
    Home2<-as.character(Home2[1])
    for(j in 1:length(play)){
      if (str_detect(play[j],'defensive team rebound')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17defensive-team-rebound.csv")



#技术指标 timeout
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("useful.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$short
    Away2<-as.character(Away2[1])
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$short
    Home2<-as.character(Home2[1])
    for(j in 1:length(play)){
      if (str_detect(play[j],'timeout')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17timeout.csv")

#技术指标 Full timeout

data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("useful.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$short
    Away2<-as.character(Away2[1])
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$short
    Home2<-as.character(Home2[1])
    for(j in 1:length(play)){
      if (str_detect(play[j],'Full timeout')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
    data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
    data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17Full-timeout.csv")

#技术指标 20 Sec. timeout
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  if(identical(information$time,integer(0))){
    next
  }else{
    play<-as.character(information$PLAY) 
    away<-vector(mode="numeric",length=0)
    home<-vector(mode="numeric",length=0)
    newtime<-vector(mode="numeric",length=0)
    event<-0
    roster<-read.csv("useful.csv",header = TRUE)
    Away1<-as.character(information$Away[2])
    Away2<-(subset(roster,TEAM==Away1))$long
    Away2<-as.character(Away2[1])
    Home1<-as.character(information$Home[2])
    Home2<-(subset(roster,TEAM==Home1))$long
    Home2<-as.character(Home2[1])
    for(j in 1:length(play)){
      if (str_detect(play[j],'20 Sec. timeout')=='TRUE'){  
        event<-(event+1)
        newtime[event]<-information$time[j]
        opt1<-c(TRUE %in% str_detect(play[j],Away2))
        opt2<-c(TRUE %in% str_detect(play[j],Home2))
        if(opt1=='TRUE' && opt2=='FALSE'){
          away[event]<-1
          home[event]<-0
        }
        if(opt1=='FALSE' && opt2=='TRUE'){
          away[event]<-0
          home[event]<-1
        }
        if(opt1=='FALSE' && opt2=='FALSE'){
          away[event]<-0
          home[event]<-0
        }
        if(opt1=='TRUE' && opt2=='TRUE'){
          away[event]<-1
          home[event]<-1
        }
      }else{
        next
      }
    }
    if(event==0){
      next
    }else{
      data1<-data.frame(time=newtime,Away=away,Home=home,playId=i)
      data2<-rbind(data2,data1)
    }
  }
}
write.csv(data2,"2016-17-20Sec-timeout.csv")



###################################################################
#################################################################
#合并rebound
 a<-read.csv("2016-17defensive(team)rebound.csv",header=TRUE)
 b<-read.csv("2016-17offensive(team)rebound.csv",header=TRUE)
 d<-merge(a,b,by = c("gameId","time"),all = T)
 write.csv(d,"b.csv",row.names = F)
 
#修改数据
data<-read.csv("b.csv",header=TRUE)
time<-data$time
gameId<-data$gameId
a<-data$Away.x
b<-data$Home.x
d<-data$Away.y
e<-data$Home.y
n<-length(time)
for(i in 1:n){
  if(is.na(a[i])){
    a[i]<-d[i]
    b[i]<-e[i]
  }else{
    next
  }
}
data2<-data.frame(gameId,time,Away=a,Home=b)
write.csv(data2,"2016-17rebound.csv",row.names = F)

####去除主队和客队不一样
a<-read.csv("2005-06offensive-team-rebound.csv",header=TRUE)
data<-a[which(a$Away==a$Home),]
data

####时间刻度,填充时间
a<-read.csv("2016-17steals.csv",header=TRUE)
b<-read.csv("a.csv",header=TRUE)
number<-unique(a$gameId)
data2<-NULL
newtime<-c(1:2880)
for(i in number){
  away<-b$away_score
  home<-b$home_score
  sb<-subset(a,gameId==i)
  time<-sb$time
  n<-length(time)
    for(j in 1:n){
      event<-time[j]
      away[event]<-sb$Away[j]
      home[event]<-sb$Home[j]
    }
    data<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data)
}
write.csv(data2,"2016-17steals.csv",row.names = F)

#修改分数
data<-read.csv("2014-15gamelog.csv",header = TRUE)
number<-unique(data$game_id)
data2<-NULL
for(i in number){
sb<-subset(data,game_id==i)
away<-sb$away_score
home<-sb$home_score
a<-0
b<-0
for(j in 1:2880){
  if(away[j]==0){
    away[j]=a
  }else{
   a=away[j]
  }
}
 for(k in 1:2880){
    if(home[k]==0){
      home[k]=b
    }else{
      b=home[k]
    }
 }
   sb$away_score<-away
   sb$home_score<-home
   data2<-rbind(data2,sb)
}
write.csv(data2,"2014-15.csv",row.names = F)
