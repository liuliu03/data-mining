#得到momentum时间内的技术指标
#1、斜率 
library(data.table)
mom<-read.csv("时间momentum.csv",header=TRUE)
mom$Away<-NULL
mom$Home<-NULL
data<-data.table(mom)
index.data<-read.csv("20Sec-timeout.csv",header=TRUE)
index.data<-data.table(index.data)
n<-length(data$game_id)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]    #开始#开始前30秒 -30
  end<-data$endpoint[i]         #结束
  index.set<-index.data[gameId==data$game_id[i] ]   #子集
  index.subset<-index.set[time>=start & time<=end]   #子集的子集
     #verse the team
    count1<-sum(index.subset$Home)
    index1<-c(index1,count1)

    count2<-sum(index.subset$Away)
    index2<-c(index2,count2)

}
data<-data.table(data,Home_Sec_timeout = index1,Away_Sec_timeout =index2)


index.data<-read.csv("assists.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end] #verse the team
    count1<-sum(index.subset$Home)
    index1<-c(index1,count1)
    
    count2<-sum(index.subset$Away)
    index2<-c(index2,count2)
    
  }
data<-data.table(data,Home_assits = index1,Away_assits =index2)
  


index.data<-read.csv("blocks.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_blocks = index1,Away_blocks =index2)




index.data<-read.csv("bonus.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_bonus = index1,Away_bonus =index2)


index.data<-read.csv("defensive(team)rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
    #verse the team
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
    
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
    
  }
data<-data.table(data,Home_defensive.team.rebound = index1,Away_defensive.team.rebound =index2)


index.data<-read.csv("defensive-foul.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_defensive_foul = index1,Away_defensive_foul =index2)


index.data<-read.csv("defensive-rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_defensive_rebound = index1,Away_defensive_rebound =index2)


index.data<-read.csv("defensive-team-rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_defensive_team_rebound = index1,Away_defensive_team_rebound =index2)

index.data<-read.csv("flagrant-foul.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_flagrant_foul = index1,Away_flagrant_foul =index2)


index.data<-read.csv("full-timeout.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_full_timeout = index1,Away_full_timeout =index2)


index.data<-read.csv("makes-free-throw.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_makes_free_throw = index1,Away_makes_free_throw =index2)


index.data<-read.csv("makes-point.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_makes_point = index1,Away_makes_point =index2)


index.data<-read.csv("makes-three-point.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_makes_three_point = index1,Away_makes_three_point =index2)


index.data<-read.csv("makes-two-point.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_makes_two_point = index1,Away_makes_two_point =index2)


index.data<-read.csv("miss.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_miss = index1,Away_miss =index2)



index.data<-read.csv("misses-free-throw.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_misses_free_throw = index1,Away_misses_free_throw =index2)


index.data<-read.csv("offensive(team)rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_offensive.team.rebound = index1,Away_offensive.team.rebound =index2)


index.data<-read.csv("offensive-foul.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_offensive_foul = index1,Away_offensive_foul =index2)


index.data<-read.csv("offensive-rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_offensive_rebound = index1,Away_offensive_rebound =index2)



index.data<-read.csv("offensive-team-rebound.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_offensive_team_rebound = index1,Away_offensive_team_rebound =index2)



index.data<-read.csv("steals.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_steals = index1,Away_steals =index2)


index.data<-read.csv("technical-foul.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_technical_foul = index1,Away_technical_foul =index2)


index.data<-read.csv("turnover.csv",header=TRUE)
index.data<-data.table(index.data)
index1<-vector()
index2<-vector()
count1<-NULL
count2<-NULL
for(i in 1:n){
  start<-data$startpoint[i]
  end<-data$endpoint[i]
  index.set<-index.data[gameId==data$game_id[i] ]
  index.subset<-index.set[time>=start & time<=end]
  count1<-sum(index.subset$Home)
  index1<-c(index1,count1)
  
  count2<-sum(index.subset$Away)
  index2<-c(index2,count2)
  
}
data<-data.table(data,Home_turnover = index1,Away_turnover =index2)


write.csv(data,"momentum时间-最终版.csv",row.names = F)
