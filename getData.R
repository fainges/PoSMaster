library(jsonlite)
library(data.table)
library(xts)
library(tidyr)
library(lubridate)
library(dplyr)

#below paths use a switch as I develop on a Win machine, but the server is Linux. You can remove if you only use one OS
# path to dcrdata
REST <- paste("http://", ifelse(Sys.info()["sysname"] == "Windows", "54.205.194.66", "127.0.0.1"), ":7777/api/", sep = "")

#path to dcrd
cmd <- paste(ifelse(Sys.info()["sysname"] == "Windows", "d:\\decred\\", "/home/ubuntu/decred/decred-linux-amd64-v1.1.0/"), "dcrctl ", sep = "")

getDCRBTC <- function(){
  raw <- fromJSON("https://poloniex.com/public?command=returnTicker")
  curr <- names(raw)
  raw <- rbindlist(raw)
  return(raw[grep("DCR", curr,fixed = TRUE),])
}

getBTCUSD <- function() {
  raw <- fromJSON("https://api.coinbase.com/v2/exchange-rates?currency=BTC")[[1]][[2]]
  return(as.numeric(raw$USD))
}

getOHLC <- function(){
  period <- as.numeric(Sys.time()) - as.numeric(months(1))
  dcr <- fromJSON(paste("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_DCR&start=", period, "&end=9999999999&period=14400", sep = ""))
  dcr$date <- as.POSIXct(dcr$date, origin="1970-01-01")
  
  usd <- fromJSON(paste("https://poloniex.com/public?command=returnChartData&currencyPair=USDT_BTC&start=", period, "&end=9999999999&period=14400", sep = ""))
  usd$date <- as.POSIXct(usd$date, origin="1970-01-01")

  #get DCR value in USD
  dcr$open <- dcr$open * usd$close
  dcr$high <- dcr$high * usd$close
  dcr$low <- dcr$low * usd$close
  dcr$close <- dcr$close * usd$close
  
  dcr <- as.xts(dcr[,-1], order.by = dcr[,1])
  usd <- as.xts(usd, order.by = usd$date)

  return(dcr)
}

getBlockData <- function() {
  raw <- as.data.frame(fromJSON(paste(REST, "block/best", sep = "")), stringsAsFactors = FALSE)
  return(raw)
}

getBlockTime <- function() {
  blockToGet <- blockData$height - 1000
  raw <- as.data.frame(fromJSON(paste(REST, "block/", blockToGet, sep = "")), stringsAsFactors = FALSE)
  sec <- (blockData$time - raw$time) / 1000 / 60
  blockTime <- paste(floor(sec), ":", round(60 * (sec - floor(sec)), 0), sep = "")
  return (ms(blockTime))
}

getWindowTime <- function() {
  if(is.na(blockTimeData)) {
    return("Block time information currently unavailable.")
  }
    
  windowTime <- ms(paste("0M", seconds(as.numeric(seconds(blockTimeData)) * (144 - poolData$window_block_index))), roll = TRUE)
  return(sprintf('%d hours, %d minutes', lubridate::hour(windowTime), lubridate::minute(windowTime)))
}

getPoolData <- function() {
  raw <- as.data.frame(fromJSON(paste(REST, "block/best/pos", sep = "")), stringsAsFactors = FALSE)
  return(raw)
}

getStakeData <- function() {
  raw <- as.data.frame(fromJSON(paste(REST, "stake/diff", sep = "")), stringsAsFactors = FALSE)
}

getNetworkHash <- function() {
  raw <- as.numeric(system(paste(cmd, "getnetworkhashps"), intern = TRUE)) / 1E12
}

getTicketFees <- function() {
  raw <- fromJSON(paste(REST, "mempool/sstx/details", sep = ""))
  if(raw[[3]] == 0) return(NULL)

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(raw) <- c("height", "time", "length", "total", "hash", "absolute_fee", "fee", "size", "height_received")
  #raw <- read.csv("D:\\temp\\fees.csv")
  raw$feeFactor <- as.factor(round(raw$fee, 4))
  return(raw)
}

getCoinSupply <- function() {
  mined <- round(as.numeric(system(paste(cmd, "getcoinsupply"), intern = TRUE)) / 1E8, 0)
  pos <-   round(as.numeric(system(paste(cmd, "getticketpoolvalue"), intern = TRUE)), 0)
  return(c(mined, pos))
}

getPoolValue <- function() {
  raw <- fromJSON(paste(REST, "stake/pool", sep = ""))
  return (raw)
}

getReward <- function() {
  reward <- 3119582664
  cycle <- floor(blockData$height / 6144)
  for (i in 1:cycle)
    reward <- reward * 100 / 101
  return (reward / 1E8)
}

#getvoteinfo version needs to be manually changed whenever a new agenda is up.
getVoteInfo <- function() {
  raw <- fromJSON(paste(system(paste(cmd, "getvoteinfo 5"), intern = TRUE), collapse = ""))
}

getAgendaItems <- function() {
  items <- voteInfo["agendas"]$agendas$id
  status <- voteInfo["agendas"]$agendas$status
  desc <- voteInfo["agendas"][[1]]$description
  for (i in 1:length(items)){
    tmp <- voteInfo["agendas"][[1]]$choices[[i]]
    tmp$item <- items[i]
    tmp$status <- status[i]
    #drop abstains
    tmp <- tmp %>% filter(bits != 0)
    
    tmp$votes <- sum(tmp$count)
    tmp$pc <- round(tmp$count / tmp$votes * 100, 2)
    if(i == 1) {
      votes <- tmp
    } else {
      votes <- bind_rows(votes, tmp)
    }
  }
  
  return(votes)
}

getAgendaTable <- function() {
  return(select(voteInfo$agendas, id, description))
}