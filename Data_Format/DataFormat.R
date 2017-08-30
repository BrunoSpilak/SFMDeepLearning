###################################
########### Packages ##############
###################################

package.list = c("dplyr","xts","smooth","abind","zoo","parallel","doParallel","rnn","RSNNS")

if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("xts")) install.packages("xts"); library("xts")
if(!require("smooth")) install.packages("smooth"); library("smooth")
if(!require("abind")) install.packages("abind"); library("abind")
if(!require("zoo")) install.packages("zoo"); library("zoo")
if(!require("parallel")) install.packages("parallel"); library("parallel")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("rnn")) install.packages("rnn"); library("rnn")
if(!require("RSNNS")) install.packages("RSNNS"); library("RSNNS")

######################
### Format market data
######################
Variables = list.files(path=paste(getwd(),"/Other/",sep=""))

Market = lapply(Variables,function(i){
  i <- paste(paste(getwd(),"/Other/",sep=""),i,sep="")
  read.csv2(i, header=TRUE,sep=";",dec = ".")
  # ,colClasses=c("character","numeric"))
})


names(Market) <- gsub(".csv","",Variables)
for( var in names(Market)){
  Market[[var]]=Market[[var]][nrow(Market[[var]]):1,]
}

for(var in names(Market)){
  Market[[var]]$Date = as.character(Market[[var]]$Date)
}

for(var in names(Market)){
  for(rep in list(
    c('janv','01'),
    c('Feb','02'),
    c('mars','03'),
    c('Apr','04'),
    c('May','05'),
    c('Jun','06'),
    c('Jul','07'),
    c('Aug','08'),
    c('sept','09'),
    c('oct','10'),
    c('nov','11'),
    c('Dec','12')
  )
  ){
    Market[[var]]$Date[grep(rep[1],as.character(Market[[var]]$Date))]=
      gsub(rep[1],rep[2],as.character(Market[[var]]$Date))[grep(rep[1],as.character(Market[[var]]$Date))]
  }
}

# #Date format
for(var in names(Market)){
  Market[[var]]$Date = as.Date(format(as.Date(Market[[var]]$Date,"%d-%m-%Y"),"20%y-%m-%d"))
}

MarketData = data.frame("Date"=seq(as.Date("2014-07-30"), as.Date("2017-07-17"), by=1))%>%
  left_join(
    left_join(
      Market$Euribor_1Y,
      left_join(
        Market$Euribor_3M,
        left_join(
          Market$Euribor_6M,
          left_join(
            Market$Euro_UK_rate,
            left_join(
              Market$Euro_US_rate,
              Market$US_JPY_rate,
              by="Date"
            ),
            by="Date"
          ),
          by="Date"
        ),
        by="Date"
      ),
      by="Date"
    ),
    by="Date"
  )
MarketData = xts(MarketData[,2:ncol(MarketData)], order.by = MarketData$Date)

#We impute the missing values (weekends, holidays) with the last value observec:
MarketData = na.locf(MarketData)
print(apply(MarketData,2,function(x) sum(is.na(x))))
#No NA
remove(Market)

###########################
##### CRIX data
load(paste(getwd(),"/CompleteData.RData",sep=""))

### Missing value
print(apply(crix,2,function(x) sum(is.na(x))))


#34% of missing values for eth

#eth has a lot of NAs, for now we delete it
#Endogene variables which are modelized one by one:
endogene=c("btc","dash","xrp","xmr","ltc","doge","nxt","nmc")
crypto = price[,endogene]

print(apply(crypto,2,function(x) sum(is.na(x))))

#We keep the shared dates
prices = cbind(
  crix,
  cbind(crypto[time(crypto)[time(crypto) %in% time(MarketData)]],
        MarketData[time(MarketData)[time(MarketData) %in% time(crypto)]]
  )
)
colnames(prices)[1]="crix"


ma = apply(prices,2,function(x) sma(x,order=30,silent="graph"))
xl = do.call(mapply, c('abind', ma, rev.along = 0))

sd = rollapply(prices, 30, stats::sd)
print(apply(sd,2,function(x) sum(is.na(x))))
#We have NAs on the first rolling window, we replace it by the sd on the 29 first observations

for(var in colnames(sd)){
  sd[is.na(sd[,var]),var]<-apply(prices[1:29,],2,stats::sd)[var]
}
middleBand = xts(xl$fitted[,1,],order.by = time(prices))
upperBand = middleBand+sd*2
lowerBand = middleBand-sd*2

dimnames(upperBand)[[2]] = paste("upperBand_",dimnames(upperBand)[[2]],sep="")
dimnames(lowerBand)[[2]] = paste("lowerBand_",dimnames(lowerBand)[[2]],sep="")
dimnames(middleBand)[[2]] = paste("middleBand_",dimnames(middleBand)[[2]],sep="")
prices = cbind(prices,
               lowerBand,
               middleBand,
               upperBand)


# #We impute the log returns
# log_returns = diff(log(prices[,colnames(prices) %in% colnames(crypto)]),lag=1)
# # print(apply(log_returns,2,function(x) sum(is.na(x))))
# #It creates one missing value
# log_returns = log_returns[-c(1),]
# 
# #Moving average on a wholemonth (30 days), same for sd
# ma = apply(log_returns,2,function(x) sma(x,order=30,silent="graph"))
# xl = do.call(mapply, c('abind', ma, rev.along = 0))
# 
# sd = rollapply(log_returns, 30, stats::sd)
# # print(apply(sd,2,function(x) sum(is.na(x))))
# #We have NAs on the first rolling window, we replace it by the sd on the 29 first observations
# for(var in colnames(sd)){
#   sd[is.na(sd[,var]),var]<-apply(log_returns[1:29,],2,stats::sd)[var]
# }
# middleBand = xts(xl$fitted[,1,],order.by = time(prices[-c(1),]))
# upperBand = middleBand+sd*2
# lowerBand = middleBand-sd*2
# 
# dimnames(upperBand)[[2]] = paste("LogReturns_UpperBand_",dimnames(upperBand)[[2]],sep="")
# dimnames(lowerBand)[[2]] = paste("LogReturns_lowerBand_",dimnames(lowerBand)[[2]],sep="")
# dimnames(middleBand)[[2]] = paste("LogReturns_middleBand_",dimnames(middleBand)[[2]],sep="")

# completeData = cbind(log_returns,
#                      prices[-c(1),!(colnames(prices) %in% colnames(log_returns))],
#                      lowerBand,
#                      middleBand,
#                      upperBand)
# print("Missing values test in completeData table")
# print(apply(completeData,2,function(x) sum(is.na(x))))

print("Missing values test in \"prices\" table")
print(apply(prices,2,function(x) sum(is.na(x))))
print("################ NO MISSING VALUE IN THE FINAL TABLE ################")
write.zoo(prices,file="C:\\Users\\etien\\Documents\\HU\\Master_Thesis\\Crypto\\Chapter_Final\\CompleteData.csv",sep=",")
rm(list=setdiff(ls(), "prices"))

