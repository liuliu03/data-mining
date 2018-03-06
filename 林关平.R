#数据可视化
library(Quandl)
SSEC<-Quandl(code="YAHOO/INDEX_SSEC",type="xts") 
data<-SSEC[,"Adjusted Close"]
summary(data)
plot(data)
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)
library("ggplot2")
qplot(data)

#转化成月指数
to.monthly(data)

##得到万科的股票
mydata1<-Quandl(code="YAHOO/SZ_000002",type="xts") 

install.packages("forecast")
library(forecast)
monthplot(mdata)
#绘制时间序列季节性或其他子系列
seasonplot(mdata,s=12,year.labels=T,col=rainbow(12))
lag.plot(mdata,12)

#返回适当的滞后和迭代的差异 diff:差分函数
rd<-na.omit(100*diff(log(data)))
summary(rd)
plot(rd)
##monthly return
rm<-ts(apply.monthly(rd,sum),start=c(1997,7),frequency=12)
plot(rm)
monthplot(rm)
seasonplot(rm,s=12,year.labels=T,col=rainbow(12))
lag.plot(rm,12)
Acf(rm,60,type="correlation")
Acf(rm,60,type="partial")
# monthly volatility is the sum of daily squared returns in a month
rv<-ts(apply.monthly(rd^2,sum),start=c(1997,7),frequency=12)
plot(rv)
monthplot(rv)
seasonplot(rv,s=12,year.labels=T,col=rainbow(12))
lag.plot(rv,12)
Acf(rv,60,type="correlation")
Acf(rv,60,type="partial")

# relationship between rm and rv
plot(cbind(rm,rv))
plot(log(rv)~rm)

data<-xts(SSEC[,"Adjusted Close"],index(SSEC))
summary(data)
# monthly average: regularly-space time series
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)
# select variable, setup train and test data 
#指数平滑验证
Y<-log(mdata)
Y.train<-window(Y,end=c(2014,12))  #取前半段
Y.test<-window(Y,start=c(2015,1))  #后半段验证

# ETS Analysis and Forecasts
library(forecast)
ets<-ets(Y.train,"ZZZ")
ets
plot(ets)

# h-step ahead forecasts
for1<-forecast(ets,h=60)  #预测后面60个时期
for1
plot(for1)
lines(Y.test)
accuracy(for1,Y.test)  #预测精度

# compare with drifted random-walk forecasts
#与其他预测方法比较
for2<-rwf(Y.train,drift=T,h=60)
for2
lines(for2$mean,col="red")
accuracy(for2,Y.test)

MSE.components<-function(x,p) {
  data<-ts.intersect(x,p)
  x<-data[,1]
  p<-data[,2]
  e<-x-p                           # prediction error
  mx<-mean(x)
  mp<-mean(p)
  sx<-sqrt(mean((x-mx)^2))
  sp<-sqrt(mean((p-mp)^2))
  r<-cor(x,p)
  MSE<-mean(e^2)                   # mean squared error
  # MSE Decomposition
  MSE.Um<-((mx-mp)^2)/MSE
  MSE.Us<-((sx-sp)^2)/MSE
  MSE.Uc<-(2*(1-r)*sp*sx)/MSE
  MSE.Ur<-((sp-r*sx)^2)/MSE
  MSE.Ud<-((1-r^2)*sx^2)/MSE
  stats2<-c(MSE,MSE.Um,MSE.Us,MSE.Uc,MSE.Ur,MSE.Ud)  # component percentage
  names(stats2)<-c("MSE","MSE.Um","MSE.Us","MSE.Uc","MSE.Ur","MSE.Ud") 
  results<-list(MSE.components=stats2)
  return(results)
}
MSE.components(Y.test,for1$mean)
MSE.components(Y.test,for2$mean)

data<-xts(SSEC[,"Adjusted Close"],index(SSEC))
summary(data)

# monthly average: regularly-space time series
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)

# select variable, setup train and test data 
Y<-log(mdata)
Y.train<-window(Y,end=c(2014,12))
Y.test<-window(Y,start=c(2015,1))

# ARIMA Analysis and Forecasts 另一种预测方法
library(forecast)
arma<-auto.arima(Y.train)
arma

# h-step ahead forecasts
for3<-forecast(arma,h=60)
for3
plot(for3)
lines(Y.test)
accuracy(for3,Y.test)

# compare with ETS forecasts
ets<-ets(Y.train,"ZZZ")
ets

# h-step ahead forecasts
for1<-forecast(ets,h=60)
for1
lines(for1$mean,col="red")
accuracy(for1,Y.test)

# compare with drifted random-walk forecasts
for2<-rwf(Y.train,drift=T,h=60)
for2
lines(for2$mean,col="yellow")
accuracy(for2,Y.test)

MSE.components<-function(x,p) {
  data<-ts.intersect(x,p)
  x<-data[,1]
  p<-data[,2]
  e<-x-p                           # prediction error
  mx<-mean(x)
  mp<-mean(p)
  sx<-sqrt(mean((x-mx)^2))
  sp<-sqrt(mean((p-mp)^2))
  r<-cor(x,p)
  MSE<-mean(e^2)                   # mean squared error
  # MSE Decomposition
  MSE.Um<-((mx-mp)^2)/MSE
  MSE.Us<-((sx-sp)^2)/MSE
  MSE.Uc<-(2*(1-r)*sp*sx)/MSE
  MSE.Ur<-((sp-r*sx)^2)/MSE
  MSE.Ud<-((1-r^2)*sx^2)/MSE
  stats2<-c(MSE,MSE.Um,MSE.Us,MSE.Uc,MSE.Ur,MSE.Ud)  # component percentage
  names(stats2)<-c("MSE","MSE.Um","MSE.Us","MSE.Uc","MSE.Ur","MSE.Ud") 
  results<-list(MSE.components=stats2)
  return(results)
}
MSE.components(Y.test,for1$mean)
MSE.components(Y.test,for2$mean)
MSE.components(Y.test,for3$mean)

# SSEC daily price index: irregularly-spaced time series
load(file="C:/Course17/WISE2017/data/SSEC.RData")

data<-xts(SSEC[,"Adjusted Close"],index(SSEC))
summary(data)

# monthly average: regularly-space time series
#不规则的时间序列，月平均
mdata<-ts(apply.monthly(data,mean),start=c(1997,7),frequency=12)
plot(mdata)

# select variable, setup train and test data 
Y<-log(mdata)
Y.train<-window(Y,end=c(2014,12))
Y.test<-window(Y,start=c(2015,1))

# Using bsts package
library(bsts)
ss <- AddLocalLinearTrend(list(), Y.train)
ss <- AddSeasonal(ss, Y.train, nseasons = 12)

# model estimation
model <- bsts(Y.train, state.specification = ss, niter = 1000)
names(model)
dim(model$state.contributions)

plot(model,"help")
plot(model)  # plot states
plot(model,"components")
plot(model,"components",same.scale=F)
plot(model,"residuals")
plot(model,"seasonal")

# prediction
pred <- predict(model, horizon = 60, burn = 100)
# par(mfrow = c(1,2))
plot(pred,plot.original=36)

# redefine the ts of pred for evaluation
for5<-ts(pred$mean,start=c(2015,1),frequency=12)

MSE.components<-function(x,p) {
  data<-ts.intersect(x,p)
  x<-data[,1]
  p<-data[,2]
  e<-x-p                           # prediction error
  mx<-mean(x)
  mp<-mean(p)
  sx<-sqrt(mean((x-mx)^2))
  sp<-sqrt(mean((p-mp)^2))
  r<-cor(x,p)
  MSE<-mean(e^2)                   # mean squared error
  # MSE Decomposition
  MSE.Um<-((mx-mp)^2)/MSE
  MSE.Us<-((sx-sp)^2)/MSE
  MSE.Uc<-(2*(1-r)*sp*sx)/MSE
  MSE.Ur<-((sp-r*sx)^2)/MSE
  MSE.Ud<-((1-r^2)*sx^2)/MSE
  stats2<-c(MSE,MSE.Um,MSE.Us,MSE.Uc,MSE.Ur,MSE.Ud)  # component percentage
  names(stats2)<-c("MSE","MSE.Um","MSE.Us","MSE.Uc","MSE.Ur","MSE.Ud") 
  results<-list(MSE.components=stats2)
  return(results)
}
MSE.components(Y.test,for5)

####quantmod
library(quantmod)
SSE <- getSymbols("000001.SS",src="yahoo",from="2011-03-04"
                  ,to="2017-03-31")
data<-`000001.SS`[,"000001.SS.Adjusted"]
names(data)<-c("SZ300")
as.data.frame(data)


SSEC <- getSymbols("601992.SS",src="yahoo",from="2011-03-04"
                  ,to="2017-03-31")
data2<-`601992.SS`[,"601992.SS.Adjusted"]
names(data2)<-c("金隅股份")
write.csv(data2,"金隅股份.csv",row.names = T)

data3<-cbind(data,data2)
as.data.frame(data3)
data3[,1]
names(data3)<-c("时间","SZ300","金隅股份")
write.table(data3,"合并.csv",sep = ",",row.names = T)
?Quandl
quandldata = Quandl("YAHOO/INDEX_SSEC", collapse="monthly", start_date="2011-03-04", type="xts")
data<-quandldata[,"Close"]
names(data)<-c("SZ300")
quandldata = Quandl("^SSEC", collapse="monthly", start_date="2011-03-04", type="xts")
data2<-quandldata[,"Close"]
write.csv(data,"大盘.csv")

par(mfrow=c(2,2))
for(n in c(63,60,76,74)){
  set.seed(123)
  plot.new()
  size=replicate(n,1/rbeta(2,1.5,4))
  center=t(replicate(n,runif(2)))
  center=center[rep(1:n,each=2),]
  color=apply(re)
}


with(airquality,plot(Wind,Ozone,main="Ozone and wind in New York"))
with(subset(airquality,Month==5),points(Wind,Ozone,col="blue"))
with(subset(airquality,Month!=5),points(Wind,Ozone,col="red"))
legend("topright",pch=1,col=c("blue","red"),legend=c("May","Other Months"))
      

library(rvest)
formurl <- "http://open.xmu.edu.cn/oauth2/authorize?client_id=1010&response_type=code"
session <- html_session(formurl) #创建会话
form <- html_form(session) #得到网页内的所有表单，以list形式返回
str(form)
form <- form[[1]] #提取我们想要的表单
UserName <- "15420161152149"
Password <- "lls051371"
form <- set_values(form,'UserName'=UserName,'Password'=Password) #填写表单内容
out_url <- submit_form(session,form) #在会话中提交表单，实现表单穿越
out_url

install.packages("TTR")
library(TTR)
library(zoo)
library(forecast)
x<-c(700,790,878,983,1110,1230)
SMA(x,3)
EMA(x,n=5,ratio=1/3)


cat(getURL(url,userpwd="15420161152149:lls051371",followlocation=TRUE))