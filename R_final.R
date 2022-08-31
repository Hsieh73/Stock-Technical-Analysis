install.packages("quantmod")
install.packages("dplyr")
install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(quantmod)
library(dplyr)
library(zoo)
library(xts)
library(TTR)
library(PerformanceAnalytics)
library(psych)
library(corrplot)

setwd('C:/')
NO <- read.csv('nostock.csv')
colnames(NO) <- c("no")
fix_data <- matrix(NA, nrow = nrow(NO), ncol = 8)
colnames(fix_data) <- c("name","average of return rate(%)", "number of trade(times)", "winning rate(%)", "standard deviation of return rate(%)", "maximum return rate(%)","minimum return rate(%)","average held days(days)") 


for (x in c(1:nrow(NO)))
{

C<- as.character(NO[x,1])
C    

getSymbols(C);

stockData <- get(C)
stockData<-na.omit(stockData)

if (nrow(stockData) > 20)
 { 
stockData <- as_data_frame(stockData)
colnames(stockData) <- c("open", "high", "low", "close", "volume","adjusted") 
stockData$date <- as.Date(row.names(stockData))

stockData <- stockData %>%
  
  select(c(date,open:volume)) %>%
  
  filter((volume>0)|(!is.na(volume)))  
stockData 
stockData <- stockData %>%
  mutate(
    lagClose1=lag(close,1), 
    lagClose2=lag(close,2),
    lagOpen1=lag(open,1),
    lagOpen2=lag(open,2),
    kbarValue=abs(close/open-1), 
    lagKbarValue1=lag(kbarValue,1),
    lagKbarValue2=lag(kbarValue,2))   
inSiteTable <- stockData %>%
  filter(
    close>open,
    lagClose2>lagOpen2, 
    open>lagClose1*(1-0.01),
    open<lagClose1*(1+0.01),
    lagClose2>lagOpen1*(1-0.01),
    lagClose2<lagClose1*(1+0.01),
    kbarValue>0.01,
    lagKbarValue1<0.005,
    lagKbarValue2>0.01) %>%
  select(inDate=date, buyPrice=close)      
inSiteTable
outSiteTable <- stockData %>%
  mutate(
    MA20=SMA(close, 20),
    lagMA20=lag(MA20,1)) %>% 
  filter(
    close<MA20,
    lagClose1>MA20) %>%
  select(outDate=date, sellPrice=close)  
outSiteTable
tradeDetailTable <- NULL   

if (nrow(inSiteTable) > 0)
{
for(ix in 1:nrow(inSiteTable)){
  inDate <- inSiteTable$inDate[ix] 
  outSite <- which(outSiteTable$outDate>inDate)[1]  
  if(length(outSite)>0){                            
    tradeDetailTable <- bind_rows(tradeDetailTable, bind_cols(inSiteTable[ix,], outSiteTable[outSite,]))
    
}}
tradeDetailTable
buyCostR <- 0.002 
sellCostR <- 0.002  
tradeDetailTable <- tradeDetailTable %>%
  mutate(
    ret=sellPrice*(1-sellCostR)/(buyPrice*(1+buyCostR))-1,
    holdDays=as.numeric(outDate-inDate))           
tradeDetailTable

meanRet <- mean(tradeDetailTable$ret)
sdRet <- sd(tradeDetailTable$ret) 
tradeNums <- nrow(tradeDetailTable) 
winRatio <- sum(as.numeric(tradeDetailTable$ret>0))/tradeNums
maxRet <- max(tradeDetailTable$ret)
minRet <- min(tradeDetailTable$ret) 
avgHoldDays <- mean(tradeDetailTable$holdDays)   


fix_data[x,1] <- C
fix_data[x,2] <- round(meanRet*100,2)
fix_data[x,3]<-tradeNums
fix_data[x,4]<-round(winRatio*100,2)
fix_data[x,5]<-round(sdRet*100,2)
fix_data[x,6]<-round(maxRet*100,2)
fix_data[x,7]<-round(minRet*100,2)
fix_data[x,8]<-round(avgHoldDays,2)
x
}}
else if (nrow(stockData) <= 20)
  {next}
}

cfix_data<-na.omit(fix_data)
A<- matrix(NA, nrow = nrow(cfix_data), ncol =8 )
for (x in c(1:nrow(cfix_data))) {
  for(y in c(1:8)){
  A[x,y] <- as.numeric(cfix_data[x,y])
   
  }
}
colnames(A) <- c("name","return", "trade", "win", "SDreturn", "maximum return rate(%)","minimum return rate(%)","held") 
rownames(A) <- cfix_data[,1]
A <- A[,-1]
AA <- data.frame(A)
head (A,10)





chartSeries(`8464.TW`,up.col='red', dn.col='green',theme="white",subset="2017-01-03::2018-02-01",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))
chartSeries(`8422.TW`,up.col='red', dn.col='green',theme="white",subset="2012-07-23::2013-07-17",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))
chartSeries(`8422.TW`,up.col='red', dn.col='green',theme="white",subset="2015-02-03::2018-04-26",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))
chartSeries(`2458.TW`,up.col='red', dn.col='green',theme="white",subset="2007-02-03::2007-04-10",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))
chartSeries(`2458.TW`,up.col='red', dn.col='green',theme="white",subset="2013-02-18::2015-09-25",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))
chartSeries(`4952.TW`,up.col='red', dn.col='green',theme="white",subset="2013-04-15::2015-12-10",type = "candlesticks",TA=c(addBBands(),addMACD(),addADX(),addVo()))

summary(`8464.TW`)
summary(`8422.TW`)
summary(`2458.TW`)
summary(`4952.TW`)

`8464.TW` <- Delt(Cl(get("8464.TW", env = new.environment)))
length(`8464.TW`[which(`8464.TW` > 0.02), ])
plot(`8464.TW`[which(`8464.TW` > 0.02), ])

`8422.TW` <- Delt(Cl(get("8422.TW", env = new.environment)))
length(`8422.TW`[which(`8422.TW` > 0.02), ])
plot(`8422.TW`[which(`8422.TW` > 0.02), ])

`2458.TW` <- Delt(Cl(get("2458.TW", env = new.environment)))
length(`2458.TW`[which(`2458.TW` > 0.02), ])
plot(`2458.TW`[which(`2458.TW` > 0.02), ])

`4952.TW` <- Delt(Cl(get("4952.TW", env = new.environment)))
length(`4952.TW`[which(`4952.TW` > 0.02), ])
plot(`4952.TW`[which(`4952.TW` > 0.02), ])

m <- cbind(Ad(get("8464.TW", env = new.environment)),
           Ad(get("8422.TW", env = new.environment)), 
           Ad(get("2458.TW", env = new.environment)),
           Ad(get("4952.TW", env = new.environment)))

n<-na.omit(m)
corr.test(as.data.frame(n))
corrplot.mixed(cor(n), lower = "ellipse", upper = "circle")







new.environment <- new.env()
m

str(get("AAPL", env = new.environment))
str(get("ORCL", env = new.environment))
str(get("MSFT", env = new.environment))
str(get("GOOG", env = new.environment))



getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment)
rate=periodReturn(close,period="daily",type="log")
head(rate)

8464 <- Delt(Cl(get("8464.TW", env = new.environment)))




getSymbols(c("8464.TW", "8422.TW", "2458.TWT", "4976.TW"), 
           src = "yahoo", env = new.environment, from = "2017-01-03", to = "2017-07-01"))
getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), 
           src = "yahoo", env = new.environment, from = "2017-01-03", to = "2017-07-01")

periodicity(get("8464.TW", env = new.environment))






#install.packages("ggplot2")
library(ggplot2)
cor(AA)[1,]

ggplot(AA, aes(x = trade, y =return)) + geom_point(aes(color = trade))
ggplot(AA, aes(x = win, y =return)) + geom_point(aes(color = win))
ggplot(AA, aes(x = held, y =return)) + geom_point(aes(color = held))



ggplot(AA, aes(x = trade, y =return)) + geom_boxplot(aes(fill = trade))
ggplot(AA, aes(x = win, y =return)) + geom_boxplot(aes(fill = win))
ggplot(AA, aes(x = held, y =return)) + geom_boxplot(aes(fill = held))

mlm <- lm( return ~ trade + win + held +SDreturn  , data = AA)
summary(mlm)


Y = -6.806679+(-0.024565)*x1+(0.99639)*x2+(0.030299)*x3+(0.390599)*x4 








write.table(AA,file="C:\\Data.csv")

stockData <- stockData %>%
  
  mutate(
    
    MA5=SMA(close,5),     # 5???移???平??????
    
    MA20=SMA(close,20),   # 20???移???平??????
    
    MA60=SMA(close,60))   # 60???移???平??????   
# 繪製???交???樣???(???交??????細表?????????置)

plotSample <- 2   



# 繪製交???樣?????????出?????????

inDate <- tradeDetailTable$inDate[plotSample]

outDate <- tradeDetailTable$outDate[plotSample]



# 繪???起始日(???場??????35???交???日)???此???用ifelse??????繪?????????????????????

matchSite <- which(stockData$date==inDate)-35

plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]                           



# 繪?????????日(????????????35???交???日)???此???用ifelse??????繪?????????????????????

matchSite <- which(stockData$date==outDate)+35

plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)]



# ??????繪製??????????????????????????????????????????

plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]



# ??????繪????????????????????????

plotData <- plotData %>% select(date:volume, MA5:MA60)



# ?????????場位置?????????訊???用???繪?????????註???場??????

plotData$inSite <- rep(NA, nrow(plotData))

plotData$inSite[which(plotData$date==inDate)] <- plotData$open[which(plotData$date==inDate)]*0.97



# ????????????位置?????????訊???用???繪?????????註????????????

plotData$outSite <- rep(NA, nrow(plotData))

plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]*1.03



# 將plotData??????由tibble???式???為xts???式???符???chart_Series繪??????式??????

plotData <- xts(plotData[,-1], order.by= plotData$date)
# 設???K棒???色

myTheme <- chart_theme()

myTheme$col$dn.col <- c("chartreuse3")  # 跌???K棒???色

myTheme$col$up.col <- c("firebrick3")   # 漲???K棒???色




chart_Series(x=plotData[,1:5]), theme=myTheme)
add_Vo()
add_TA(x=plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
add_TA(x=plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
add_TA(x=plotData$MA60, on=1, type="l", col="green", lwd=1.5)
add_TA(x=plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=1.5)
add_TA(x=plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=1.5)
PlotGraph <- function(plotSample){
  
  
  
  # 繪製交???樣?????????出?????????
  
  inDate <- tradeDetailTable$inDate[plotSample]
  
  outDate <- tradeDetailTable$outDate[plotSample]
  
  
  
  # 繪???起始日(???場??????35???交???日)???此???用ifelse??????繪?????????????????????
  
  matchSite <- which(stockData$date==inDate)-35
  
  plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]                           
  
  
  
  # 繪?????????日(????????????35???交???日)???此???用ifelse??????繪?????????????????????
  
  matchSite <- which(stockData$date==outDate)+35
  
  plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)]
  
  
  
  # ??????繪製??????????????????????????????????????????
  
  plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]
  
  plotData <- plotData %>% select(date:volume, MA5:MA60)
  
  
  
  # ?????????場位置????????????
  
  plotData$inSite <- rep(NA, nrow(plotData))
  
  plotData$inSite[which(plotData$date==inDate)] <- plotData$open[which(plotData$date==inDate)]*0.95
  
  
  
  # ????????????位置????????????
  
  plotData$outSite <- rep(NA, nrow(plotData))
  
  plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]*1.05
  
  
  
  # 將plotData??????由tibble???式???為xts???式???符???chart_Series繪??????式??????
  
  plotData <- xts(plotData[,-1], order.by= plotData$date)
  
  
  
  # 設???K棒???色
  
  
  
  myTheme <- chart_theme()
  
  myTheme$col$dn.col <- c("chartreuse3")  # 跌???K棒???色
  
  myTheme$col$up.col <- c("firebrick3")   # 漲???K棒???色
  
  
  
  # 繪製主???形
  
  pic <- chart_Series(x=plotData[,1:5], name=paste0(googleStockCode," ???????????????形"), theme=myTheme)
  
  
  
  # ?????????交??????形
  
  pic <- add_Vo()
  
  
  
  # ??????5???移???平??????
  
  pic <- add_TA(x=plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
  
  
  
  # ??????20???移???平??????
  
  pic <- add_TA(x=plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
  
  
  
  # ??????60???移???平??????
  
  pic <- add_TA(x=plotData$MA60, on=1, type="l", col="green", lwd=1.5)
  
  
  
  # 標註???場位置
  
  pic <- add_TA(x=plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=1.5)
  
  
  
  # 標註??????位置
  
  pic <- add_TA(x=plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=1.5)
  
  
  
  return(pic)
  
}
PlotGraph(plotSample=5)
PlotGraph(plotSample=11)
PlotGraph(plotSample=16)

