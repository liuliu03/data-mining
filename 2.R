#常规赛和季后赛
#平均技术指标、工资、菜鸟
#从basketball-reference获取
team<-c("atl","brk","njn","bos","cha","cho","chi","cle","dal","den","det",
        "gsw","hou","ind","lac","lal","mem","mia","mil","min",
        "noh","nop","nyk","okc","sea","orl","phi","pho","por","sac","sas",
        "tor","uta","was")
team<-toupper(team)
library(data.table)
library(XML)
library(rvest)
url<-paste0("http://www.basketball-reference.com/teams/",team,"/")
tag<-paste0("http://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F",team,"%2F")
paste_url<-function(x,y){
  return(paste0(x,y,".html"))
}
paste_per<-function(x,y){
  return(paste0(x,y,".html&div=div_per_game"))
}
paster_off<-function(x,y){
  return(paste0(x,y,".html&div=div_playoffs_per_game"))
}
rugular<-data.table()
playoff<-data.table()
for(i in c(2008:2017)){
  for(k in c(1:34)){
    url_roster<-paste_url(url[k],i)
    tryCatch({
      #队员名单
      tables<-readHTMLTable(url_roster,encoding="UTF-8")
      roster<-data.table(tables[[1]])[,c("No.","Player","Pos","Exp")]
      #常规赛
      url_per<-paste_per(tag[k],i)
      per_game<-data.table(readHTMLTable(url_per,encoding="UTF-8")[[1]])
      names(per_game)[names(per_game)=="V2"]="Player"
      data<-merge(roster,per_game,by.x = "Player",by.y="Player")
      data<-data.table(Year=i,Team=team[k],data)
      rugular<-rbind(rugular,data)
      #季后赛
      url_off<-paster_off(tag[k],i)
      per_off<-data.table(readHTMLTable(url_off,encoding="UTF-8")[[1]])
      names(per_off)[names(per_off)=="V2"]="Player"
      data2<-merge(roster,per_off,by.x = "Player",by.y="Player")
      data2<-data.table(Year=i,Team=team[k],data2,Playoff=1)
      playoff<-rbind(playoff,data2)
    },
    error=function(e){
      print(team[k])})
  }
}
rugular[, Rk := NULL]
rugular[G>=10 & GS>=10]


#得到gameID
library(rvest)
library(stringr)
library(data.table)
all_id<-NULL
#常规赛
#day<-c(seq(from=as.Date("2008/10/28"),to=as.Date("2009/4/14"),by="day"),
#      seq(from=as.Date("2009/10/27"),to=as.Date("2010/4/14"),by="day"),
#       seq(from=as.Date("2010/10/26"),to=as.Date("2011/4/13"),by="day"),
#       seq(from=as.Date("2011/10/25"),to=as.Date("2012/4/26"),by="day"),
#       seq(from=as.Date("2012/10/30"),to=as.Date("2013/4/17"),by="day"),
#       seq(from=as.Date("2013/10/29"),to=as.Date("2014/4/16"),by="day"),
#       seq(from=as.Date("2014/10/28"),to=as.Date("2015/4/15"),by="day"),
#       seq(from=as.Date("2015/10/27"),to=as.Date("2016/4/13"),by="day"),
#       seq(from=as.Date("2016/10/25"),to=as.Date("2017/4/12"),by="day"))
#季后赛
day<-c(seq(from=as.Date("2009/4/18"),to=as.Date("2009/5/30"),by="day"),
       seq(from=as.Date("2010/4/17"),to=as.Date("2010/5/29"),by="day"),
       seq(from=as.Date("2011/4/16"),to=as.Date("2011/5/26"),by="day"),
       seq(from=as.Date("2012/4/28"),to=as.Date("2012/6/9"),by="day"),
       seq(from=as.Date("2013/4/20"),to=as.Date("2013/6/3"),by="day"),
       seq(from=as.Date("2014/4/19"),to=as.Date("2014/5/31"),by="day"),
       seq(from=as.Date("2015/4/18"),to=as.Date("2015/5/27"),by="day"),
       seq(from=as.Date("2016/4/16"),to=as.Date("2016/5/30"),by="day"),
       seq(from=as.Date("2017/4/15"),to=as.Date("2017/5/25"),by="day"))
day<-str_replace_all(day,pattern = "-",replacement = "")
all_html<-as.vector(sapply("http://www.espn.com/nba/scoreboard/_/date/",paste0,USE.NAMES=TRUE,day))
for(j in 1:length(day)){
  tryCatch({
  xml<-read_html(all_html[j],encoding="UTF-8")
  id<-str_extract_all(xml,'"id":+"[0-9]{9}"')
  id<-unlist(str_extract_all(id,"[0-9]{9}"))
  judge<-substr(id[1],1,6)
  id<-unique(id[grepl(judge,id)])
  id_table<-data.table(date=day[j],gameID=id)
  all_id<-rbind(all_id,id_table)},
  error=function(e){print(day[j])})
}
Year<-strtrim(all_id$date,4)
all_id<-data.table(Year,all_id)
#all_id<-all_id[order(date)]
write.csv(all_id,"off_gameID.csv",row.names = F)

#排除不适当的id
id<-all_id[gameID %in% c("370628112","370628126",
        "370628120","370628114","370628102",
        "370628106","370628104","370628118",
        "370628103")]

#3得到平时比赛指标
library(rvest)
library(stringr)
library(data.table)
rugular<-read.csv("rugular_gameID.csv",header=TRUE)
n<-length(rugular$Year)
year<-rugular$Year
date<-rugular$date
gameid<-rugular$gameID
table<-NULL
for(i in 1:50){
url<-paste0("http://www.espn.com/nba/boxscore?gameId=",gameid[i])
html<-read_html(url,encoding="UTF-8")
team<-html %>% html_nodes("span.abbrev" )%>% html_text()
Away<-team[1]
Home<-team[2]
#3.1 away的表格
#DT1<-html %>% html_nodes("div.col.column-one.gamepackage-away-wrap") %>% 
#       html_nodes("table") %>% html_table(fill=TRUE)
DT1<-html %>% html_nodes("table") %>%.[[2]]%>% html_table(fill=TRUE)
DT1<-data.table(DT1)
DT1<-DT1[MIN!="MIN"& MIN!="DNP-COACH'S DECISION"
         & MIN!="",-1]
name<-html %>% html_nodes("div.col.column-one.gamepackage-away-wrap") %>% 
  html_nodes("a") %>% html_attr("href")
name<-strsplit(name,split="/([0-9]+)/")
name<-lapply(name, function(x) x[2])
name<-unlist(str_replace_all(name,"-"," "))
name<-str_to_title(name) #首字母大写
DT1<-DT1[,Player := name]
DT1<-data.table(Team=Away,oppoent=Home,DT1,Home_dummy=0)
#3.2 home的表格
#DT2<-html %>% html_nodes("div.col.column-two.gamepackage-home-wrap") %>% 
#  html_nodes("table") %>% html_table(fill=TRUE)
DT2<-html %>% html_nodes("table") %>%.[[3]]%>% html_table(fill=TRUE)
DT2<-data.table(DT2)
DT2<-DT2[MIN!="MIN"& MIN!="DNP-COACH'S DECISION"
         & MIN!="",-1]
name<-html %>% html_nodes("div.col.column-two.gamepackage-home-wrap") %>% 
  html_nodes("a") %>% html_attr("href")
name<-strsplit(name,split="/([0-9]+)/")
name<-lapply(name, function(x) x[2])
name<-unlist(str_replace_all(name,"-"," "))
name<-str_to_title(name) #首字母大写
DT2<-DT2[,Player := name]
DT2<-data.table(Team=Home,oppoent=Away,DT2,Home_dummy=1)
#3.3判断是否是加时赛
txt<-html %>% html_node("span.game-time") %>%html_text()
OT_dummy=ifelse(grepl("OT",txt),1,0)
DT<-rbind(DT1,DT2)
DT<-DT[MIN!=0]
DT<-data.table(Year=year[i],Date=date[i],gameID=gameid[i],DT,OT_dummy)
table<-rbind(table,DT)
}
table<-data.table(table,playoff_dummy=0)

playoff<-read.csv("playoff.csv",header=TRUE)
playoff$X...<-NULL
playoff<-data.table(playoff)
setcolorder(playoff,c("Year","Date","Player","gameID","Team","oppoent","MIN",
            "FG_dummy","FG","FGM","FGA","X3PT_dummy","X3PT","X3PTM","X3PTA",
            "FT_dummy","FT","FTM","FTA","OREB","DREB","REB","AST","STL","BLK",
            "TO","PF","PTS","Home_dummy","OT_dummy","playoff_dummy"))
FT<-playoff$FT
FT<-str_split(FT,"-")
FTM<-unlist(lapply(FT,function(x) x[1]))
FTA<-unlist(lapply(FT,function(x) x[2]))
FT_dummy<-ifelse(FTA==0,0,1)

FTM<-as.numeric(FTM)
FTA<-as.numeric(FTA)
FT<-ifelse(FT_dummy!=0,FTM/FTA,0)
playoff$FT_dummy<-FT_dummy

playoff$Player<-tolower(playoff$Player)
rugular_tech<-read.csv("rugular_tech.csv",header=TRUE)
rugular_tech$Player<-tolower(rugular_tech$Player)
rugular_tech$Player<-str_replace_all(rugular_tech$Player,"[[:punct:]]","")
table1<-merge(rugular_tech,playoff)


#basketball 常规赛+季后赛 每一场比赛
library(rvest)
library(data.table)
library(stringr)
box<-NULL
month<-c("october","november","december","january",
         "february","march","april","may","june")
year<-c(2008:2017)
for(i in 1:10){
for(j in 1:9){
  tryCatch({
  url<-paste0("http://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_",
              year[i],"_games-",month[j],".html&div=div_schedule")
  html<-read_html(url,encoding="UTF-8")
  table<-html %>% html_nodes("table") %>%html_table(fill=TRUE)%>%.[[1]]
  names(table)<-c("Date","Start","Away","PTS1","Home","PTS2","Box","OT_dummy","Note")
  table<-data.frame(Month=month[j],table)
  box<-rbind(box,table)
  },
  error=function(e){"error"}
  )
  }
}
#
处理指标-时间
box<-read.csv("box.csv",header=TRUE)
x<-as.character(box$Date)
x<-as.Date(strptime(x, "%B%d,%Y"))
x<-str_replace_all(x,"-","")
box$Date<-x

team<-read.csv("team.csv",header=TRUE)
Long<-as.character(team$Team)
Short<-as.character(team$Short)
Away<-as.character(box$Away)
Home<-as.character(box$Home)
for(i in 1:length(Short)){
  a<-grepl(Long[i],Away)
  Away[a]<-Short[i]
  b<-grepl(Long[i],Home)
  Home[b]<-Short[i]
}
box$away<-Away
box$home<-Home

#常规赛每场技术指标
library(rvest)
library(stringr)
library(data.table)
box<-read.csv("box.csv",header=TRUE)
date<-box$Date
away<-box$Away
home<-box$Home
OT<-box$OT_dummy
n<-length(date)
rowname<-c("Starters","MP","FG","FGA","FG%","3P","3PA","3P%","FT","FTA","FT%","ORB","DRB","TRB",
"AST","STL","BLK","TOV","PF","PTS","+/-")
url_paste<-function(x,y,z){
  paste0("http://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fboxscores%2F",x,"0",y,".html&div=div_box_",z,"_basic")
}
table<-NULL
for(i in 9806:n){
  tryCatch({
    url_away<-url_paste(date[i],home[i],away[i])
    html_away<-read_html(url_away,encoding = "UTF-8")
    Away_table<-html_away%>% html_nodes("table") %>%html_table(fill=TRUE)
    Away_table<-data.table(Away_table[[1]])
    names(Away_table)<-rowname
    Away_table<-Away_table[-c(1,.N)]
    Away_table<-data.frame(Date=date[i],Home_dummy=0,Team=away[i],oppoent=home[i],Away_table,OT_dummy=OT[i])
    url_home<-url_paste(date[i],home[i],home[i])
    html_home<-read_html(url_home,encoding = "UTF-8")
    Home_table<-html_home%>% html_nodes("table") %>%html_table(fill=TRUE)
    Home_table<-data.table(Home_table[[1]])
    names(Home_table)<-rowname
    Home_table<-Home_table[-c(1,.N)]
    Home_table<-data.frame(Date=date[i],Home_dummy=1,Team=home[i],oppoent=away[i],Home_table,OT_dummy=OT[i])
    table<-rbind(table,Away_table,Home_table)
  },
  error=function(e){print(i)})
}

#basketball 所有赛季表格进行处理
a<-read.csv("tech1.csv",header=TRUE)
b<-read.csv("tech2.csv",header=TRUE)
d<-read.csv("tech3.csv",header=TRUE)
data<-rbind(a,b,d)
FG<-data$FG
FG<-as.character(FG)
FG<-as.numeric(FG)
data$FG<-FG
data<-na.omit(data)
X3P_dummy=ifelse(data$X3PA==0,0,1)
FT_dummy=ifelse(data$FTA==0,0,1)
FG_dummy=ifelse(data$FGA==0,0,1)
#修改时间
time<-strsplit(as.character(data$MP),":")
time<-unlist(lapply(time,function(x) as.numeric(x[1])*60+as.numeric(x[2])))
data$MP<-time
#修改数据playoff 删除总决赛
number<-c(20170601:20170618,20160602:20160619,20150604:20150616,20140605:20140615,
          20130606:20130620,20120612:20120621,20110531:20110612,20100603:20100617,
          20090604:20090614,20080605:20080617)
count<-ifelse(data$Date %in% number,1,0)
data<-data[-count,]
#得到季后赛虚拟变量
number<-c(20170415:20170618,20160416:20160619,20150418:20150616,20140419:20140615,
          20130420:20130620,20120428:20120621,20110416:20110612,20100417:20100617,
          20090418:20090614,20080419:20080617)
Playoff_dummy=ifelse(data$Date %in% number,1,0)
data$Playoff_dummy<-Playoff_dummy

?#算出工资占比
a<-read.csv("NBA_Salaries.csv",header=TRUE)
a<-data.table(a)
tt<-a[,.(Salary = Salary/sum(Salary)),by=.(Year,Team)]
tt$Player<-a$Player
tt$Player<-str_replace_all(tt$Player,"[:punct:]","")
tt$Player<-tolower(tt$Player)
b<-read.csv("rugular_tech.csv",header=TRUE)
b[is.na(b)]<-0
b$name<-b$Player
b$Player<-str_replace_all(b$Player,"[:punct:]","")
b$Player<-tolower(b$Player)
tt<-data.table(tt,key="Year,Team,Player")
b<-data.table(b,key="Year,Team,Player")
data<-merge(b,tt)


tech<-read.csv("tech.csv",header=TRUE)
tech$Player<-str_replace_all(tech$Player,"[:punct:]","")
tech$Player<-tolower(tech$Player)
tech<-data.table(tech,key="Year,Team,Player")
result<-merge(data,tech)

mvp<-read.csv("mvp.csv",header=TRUE)
n<-length(mvp$year)
all_star<-NULL
for(k in 2007:2017){
  sb<-data[data$Year==k,]
  sb2<-mvp[mvp$year==k,]
  count<-ifelse(sb$Player %in% sb2$player,1,0)
  all_star<-c(all_star,count)
  }
data$all_star<-all_star

#修改队伍
BOS<-ifelse(data$oppoent=="WAS",1,0)
data$WAS<-BOS

data<-read.csv("momentum_after30Sec_consistent.csv",header=TRUE)
data$slope<-abs(data$slope)
n<-length(data$game_id)
count<-NULL
for(i in 1:(n-1)){
  if(data$startpoint[i]==data$startpoint[i+1] | 
     data$endpoint[i]==data$endpoint[i+1]){
    num<-ifelse(data$slope[i]>data$slope[i+1],i+1,i)
    count<-c(count,num)
  }else{
    next
  }
}
data<-data[-count,]
write.csv(data,"momentum_after30Sec_consistent.csv",row.names = F)

#Program2.0
#deal with the teacher's NBA_casino.csv
library(stringr)
library(dplyr)
a<-read.csv("NBAcasino.csv",header = TRUE)
a$date<-format(strptime(a$date,format="%Y/%m/%d"),"%Y%m%d")
a$Date<-as.numeric(a$date)
a$date<-NULL
a<-a[a$Date >20070808,]
a[is.na(a)]<-0
b<-a%>% distinct(Date,spread_pts,total,.keep_all=TRUE)
names(a)<-c("Vs","DEN_Line","Date")
#去除重复b<-a %>% distinct(a$Date,a$Away,.keep_all = TRUE)

#the program is to get the NBA's odds lines of all 
#season from 2008 to 2017 years
library(rvest)
library(XML)
library(stringr)
url<-"http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/teams/pastresults/"
team<-c(404169,404117,404288,404083,404330,
        404198,404213,404153,404155,404011,
        404085,664421,404171,404013,404067,
        404065,403995,404316,403993,404031,
        404119,404135,403977,404029,403975,
        404047,404137,404049,404101,404302)
year<-c("2016-2017/team","2015-2016/team","2014-2015/team",
        "2013-2014/team","2012-2013/team","2011-2012/team",
        "2010-2011/team","2009-2010/team","2008-2009/team",
        "2007-2008/team")
odd_url<-lapply(year,function(x) str_c(url,x,team,".html"))
odd_url<-unlist(odd_url)
NBA_odd<-NULL
n<-length(odd_url)
for(i in 1:n){
  cover_date<-readHTMLTable(odd_url[i],encoding="UTF-8")
  if(length(cover_date)==1){
    cover_date<-as.data.frame(cover_date)
    names(cover_date)<-c("Date","Vs","Score","Type","DEN_Line","O/U")
  }else{
    cover_date<-rbind(as.data.frame(cover_date[1]),as.data.frame(cover_date[2]))
    names(cover_date)<-c("Date","Vs","Score","Type","DEN_Line","O/U")
  }
  NBA_odd<-rbind(NBA_odd,cover_date)
}
NBA_odd$DEN_Line<-str_replace_all(NBA_odd$DEN_Line,"L","")
NBA_odd$DEN_Line<-str_replace_all(NBA_odd$DEN_Line,"W","")
NBA_odd$DEN_Line<-str_replace_all(NBA_odd$DEN_Line,"P","")
NBA_odd$Date<-as.character(NBA_odd$Date)
NBA_odd$Date<-format(strptime(NBA_odd$Date,format="%m/%d/%y"),"%Y%m%d")
NBA_odd$Vs<-str_replace_all(NBA_odd$Vs,"@","")

write.csv(NBA_odd,"NBAcasino.csv",row.names = F)

#Program 2.1  赌博率
library(data.table)
library(dplyr)
odds<-read.csv("NBAcasino.csv",header = TRUE)
#names(odds)<-c("Team","DEN_Line","Date")
tech<-read.csv("NBA_tech_modify_edition2.csv",header=TRUE)
#odds<-data.table(odds,key="Date,Team")
#tech<-data.table(tech,key="Date,Team")
#dd<-merge(tech,odds)
#tech<-tech %>% distinct(Date,Team,.keep_all=TRUE)

#solvement2
odds$DEN_Line<-as.character(odds$DEN_Line)
odds$DEN_Line<-as.numeric(odds$DEN_Line)
day<-tech$Date
team<-tech$Team
opp<-tech$oppoent
n<-length(tech$Team)
line<-vector()
line[1:n]<-NA
for(i in 1:n){
  a<-day[i]
  b<-as.character(team[i])
  d<-as.character(opp[i])
  data<-odds[which(odds$Date==a &(odds$Vs==b|odds$Vs==d)),]
  #data<-subset(odds,odds$Date==day[i]&(odds$Vs==team[i]|odds$Vs==opp[i]))
  line[i]<-data$DEN_Line[1]
}
tech$line<-line

#Program 2.2 is about the NBA draft
library(data.table)
library(stringr)
draft<-read.csv("NBA_draft.csv",header=TRUE)
draft$Player<-str_replace_all(draft$Player,"[[:blank:]]$","")
mock<-read.csv("NBA_mock.csv",header=TRUE)
mock$Player<-str_replace_all(mock$Player,"[[:blank:]]$","")
cluter<-read.csv("draft_cluter.csv",header=TRUE)
a<-merge(mock,cluter,by=c("mock_draft_order"))
b<-merge(draft,a,by=c("Player"),all=TRUE)
b$Year.x<-NULL
b$Year.y<-NULL
b$Team<-NULL
b$Exp<-0
NBA_tech<-read.csv("NBA_tech_modify_edition3.csv",header = TRUE)
NBA_tech<-NBA_tech[order(NBA_tech$Year,NBA_tech$Team),]
d<-merge(NBA_tech,b,by=c("Exp","Player"),all = TRUE)
d<-d[!is.na(d$Year),]
write.csv(d,"NBA_tech_modify_edition4.csv",row.names = F)

#Program3
############LOSSO
library(glmnet)
library(MASS)
away_co<-read.csv("Away-consistent.csv",header = TRUE)
away_in<-read.csv("Away-inconsistent.csv",header=TRUE)
away<-cbind(away_co,away_in)
home_co<-read.csv("Home-consistent.csv",header=TRUE)
home_in<-read.csv("Home-inconsistent.csv",header=TRUE)
home<-cbind(home_in,home_co)

away<-rbind(away,home)
y<-away$slope
x<-away[,-1]
test_rows <- sample(1:15540,.34*15540)
y.test<-y[test_rows]
y<-y[-test_rows]
y<-as.matrix(y)
x.test<-x[test_rows,]
x<-x[-test_rows,]
x<-as.matrix(x)
y.test<-as.matrix(y.test)
x.test<-as.matrix(x.test)

fit.lasso <- glmnet(x, y, family="gaussian", alpha=1)
fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)
fit.elnet <- glmnet(x, y, family="gaussian", alpha=.5)


for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

yhat0 <- predict(fit0, newx=x.test, s=fit0$lambda.1se)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

cvfit<-cv.glmnet(x, y,type.measure = "mse",alpha=1,family="gaussian")
coef(cvfit, s="lambda.min")
