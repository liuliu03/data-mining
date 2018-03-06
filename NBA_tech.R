###每一年度各项技术指标发生对应的时间点

#技术指标 blocks
log<-read.csv("2016-17gamelog.csv",header = TRUE)
log<-as.data.frame(log)
roster<-read.csv("team.csv",header = TRUE)
library(stringr)
number<-unique(log$playId)
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'blocks')){  
      event<-event+1
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17blocks.csv",row.names = F)


#技术指标 assists
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'assists')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  } 
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17assists.csv",row.names = F)

#技术指标 personal+team  offensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'offensive')&
        str_detect(play[j],'rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17offensive(team)rebound.csv",row.names = F)

#技术指标 offensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'offensive rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17offensive-rebound.csv",row.names = F)

#技术指标 offensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'offensive team rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17offensive-team-rebound.csv",row.names = F)

#技术指标 personal+team defensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'defensive')&
        str_detect(play[j],'rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  } 
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17defensive(team)rebound.csv",row.names = F)

#技术指标defensive rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'defensive rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17defensive-rebound.csv",row.names = F)

#技术指标 defensive team rebound
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'defensive team rebound')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17defensive-team-rebound.csv",row.names = F)

#技术指标 steals
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'steals')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17steals.csv",row.names = F)


#技术指标 makes free throw
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'makes') & 
        str_detect(play[j],'free throw')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17makes-free-throw.csv",row.names = F)

#技术指标 misses free throw 
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'misses') & 
        str_detect(play[j],'free throw')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17misses-free-throw.csv",row.names = F)

#技术指标 offensive foul/Charge
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'offensive foul') |
        str_detect(play[j],'offensive Charge')){  #Charge
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17offensive-foul.csv",row.names = F)

#技术指标 defensive foul
#shooting foul+shooting block foul+personal foul+loose ball foul
#defense foul+illegal defense/foul+technical foul+flagrant foul
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'foul') | str_detect(play[j],'defense')&
        !(str_detect(play[j],'offensive'))){  #Charge
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }

    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17defensive-foul.csv",row.names = F)

#技术指标 technical foul
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'technical foul') |
        str_detect(play[j],'Technical Foul')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17technical-foul.csv",row.names = F)

#技术指标 flagrant foul
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'flagrant foul')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17flagrant-foul.csv",row.names = F)

#技术指标 Full/full timeout
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'full timeout') |
        str_detect(play[j],'Full timeout')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17full-timeout.csv",row.names = F)

#技术指标 20 Sec. timeout
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'Sec. timeout')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17-20Sec-timeout.csv",row.names = F)

#技术指标 makes point
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  score1<-as.numeric(information$away_score)
  score2<-as.numeric(information$home_score)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 2:length(score1)){
    if (score1[j]-score1[j-1]>1 ||score2[j]-score2[j-1]>1){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
  }
write.csv(data2,"2016-17makes-point.csv",row.names = F)

#技术指标 makes two point 
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  score1<-as.numeric(information$away_score)
  score2<-as.numeric(information$home_score)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 2:length(score1)){
    if (score1[j]-score1[j-1]==2 ||score2[j]-score2[j-1]==2){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
    }else{
      next
    }
  }
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17makes-two-point.csv",row.names = F)

#技术指标 makes three point
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  score1<-as.numeric(information$away_score)
  score2<-as.numeric(information$home_score)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 2:length(score1)){
    if (score1[j]-score1[j-1]==3 || score2[j]-score2[j-1]==3){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
    }else{
      next
    }
  }
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17makes-three-point.csv",row.names = F)

#技术指标 (miss + block) point
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if ((str_detect(play[j],'misses') & 
        !(str_detect(play[j],'free throw')))|
         str_detect(play[j],'blocks')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
    data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)
}
write.csv(data2,"2016-17miss.csv",row.names = F)

#技术指标 turnover
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'turnover') |
        str_detect(play[j],'bad pass') |
        str_detect(play[j],'Bad Pass') |
        str_detect(play[j],'traveling')|
        str_detect(play[j],'shot clock')|
        str_detect(play[j],'backcourt') |
        str_detect(play[j],'second') |
        str_detect(play[j],'technical foul') |
        str_detect(play[j],'lost ball') |
        str_detect(play[j],'Technical Foul')){  
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
        away[event]<-1
        home[event]<-1
      }
    }else{
      next
    }
  }
   data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
    data2<-rbind(data2,data1)

}
write.csv(data2,"2016-17turnover.csv",row.names = F)

#技术指标 bonus
data2<-NULL
for(i in number){
  information<-subset(log,playId==i)
  play<-as.character(information$PLAY)
  away<-vector(mode="numeric",length=0)
  home<-vector(mode="numeric",length=0)
  newtime<-vector(mode="numeric",length=0)
  event<-0
  Away1<-as.character(information$Away[2])
  Away2<-(subset(roster,Team==Away1))$ESPN
  Away2<-as.character(Away2)
  Home1<-as.character(information$Home[2])
  Home2<-(subset(roster,Team==Home1))$ESPN
  Home2<-as.character(Home2)
  option<-as.character(information$team)
  for(j in 1:length(play)){
    if (str_detect(play[j],'personal foul')=='TRUE'){ 
      if (str_detect(play[j+1],'free throw')=='TRUE'){
      event<-(event+1)
      newtime[event]<-information$time[j]
      if(option[j]==Away2 & option[j]!=Home2){
        away[event]<-1
        home[event]<-0
      }
      if(option[j]==Home2 & option[j]!=Away2 ){
        away[event]<-0
        home[event]<-1
      }
      if(option[j]!=Away2 & option[j]!=Home2){
        away[event]<-0
        home[event]<-0
      }
      if(option[j]==Away2 & option[j]==Home2){
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
  data1<-data.frame(gameId=i,time=newtime,Away=away,Home=home)
  data2<-rbind(data2,data1)
  }
}
write.csv(data2,"2016-17bonus.csv",row.names = F)
