library(shiny)
library(tidyr)
library(dplyr)
library(DT)
library(lubridate)

options(shiny.sanitize.errors = FALSE)

#timer
observe({
  invalidateLater(60000, NULL)
  print("Reloading data")
  blockData <<- getBlockData()
  blockTimeData <<- getBlockTime()
  poolData <<- getPoolData()
  hashData <<- getNetworkHash()
  stakeData <<- getStakeData()
  feeData <<- getTicketFees()
  windowTimeData <<- getWindowTime()
  BTCUSD <<- getBTCUSD()
  DCRBTC <<- getDCRBTC()
  DCROHLC <<- getOHLC()
  coinSupply <<- getCoinSupply()
  posReward <<- getReward() *.06
  poolValue <<- getPoolValue()
  voteInfo <<- getVoteInfo()
})

shinyServer(function(input, output, session) {
  v <- reactiveValues(reactGrid = NULL)

  grid <- readRDS("grid.rds")
  
  observe({
    #assign variables
    priceNow <- as.numeric(input$priceNow)
    feesNow <- as.numeric(input$feesNow)
    stakePool <- input$stakePool
    feeSlider <- input$feeSlider
    priceSlider <- input$priceSlider
    invest <- input$invest
    
    info <- ""
    
    #validate
    if(is.na(invest) || invest < priceNow)
      invest <- 1000
    
    if(is.na(priceNow)){
      priceNow <- 20
    }
    else if(priceNow > 200){
      priceNow <- 200
      updateNumericInput(session, "priceNow", value = 200)
    }
    else if(priceNow < 20 | is.na(priceNow)){
      priceNow <- 20
      updateNumericInput(session, "priceNow", value = 20)
    }
    
    if(is.na(feesNow)){
      feesNow <- 0.01
    }
    else if(feesNow > 2){
      feesNow <- 2
      updateNumericInput(session, "feesNow", value = 2)
    }
    else if(feesNow < 0.01){
      feesNow <- 0.01
      updateNumericInput(session, "feesNow", value = 0.01)
    }
    
    poolFee <-  ifelse(input$stakePool, 0.05, 0) 
    transSize <- ifelse(input$stakePool, 0.539, 0.296) 
    grid$ticketReturn <- posReward * (1 - as.numeric(poolFee)) - grid$fee * as.numeric(transSize)
    
    userGrid <- grid %>% filter(fee >= as.numeric(feeSlider[1]), fee <= as.numeric(feeSlider[2]),
                                price >= priceSlider[1], price <= priceSlider[2])
    
    userGrid$returnPc <- userGrid$ticketReturn / userGrid$price

    grid$numTickets <- floor(as.numeric(invest) / (grid$price + grid$fee * transSize))
    grid$returnAbs <- round(grid$ticketReturn * grid$numTickets, 2)
    p <- round(as.numeric(priceNow), 0)
    f <- round(as.numeric(feesNow), 2)
    row <- grid %>% filter(price == round(priceNow, 0), fee <= round(feesNow, 2) + 0.01, fee >= round(feesNow, 2) - 0.01)
    fmt <- spread(userGrid,price,returnPc) #[,c(1:2, 3:8)]
    info <- paste("At the current price, you can buy", row$numTickets, "tickets. This will give you an absolute return of",
                  round(row$returnAbs, 2), "DCR using the average ticket fee. This is a return of", round(
                  round(row$returnAbs, 2) / (row$numTickets * (row$price + row$fee)) * 100, 2), "percent.",
                  "With continuous buying, this value works out to be return per month. With at least ~100 tickets you",
                  "can expect to see this return each month. Fewer tickets will still average to this value each month",
                  "over time, but you will see increased variation on a month-to-month basis.",
                  sep = " ")
    v$priceGrid <- fmt %>% datatable(fmt, rownames=FALSE) %>% formatPercentage(3:length(fmt), 2) %>% formatRound(2, 4)
    v$curPriceInfo <- info
    simRet <- grid %>% filter(returnAbs < row$returnAbs * 1.05, returnAbs > row$returnAbs * 0.95) 
    if(nrow(simRet == 0))
      v$similarReturns <- simRet
    else
      v$similarReturns <- simRet %>% distinct(price, .keep_all = TRUE) %>% select(fee, price, returnAbs)
    v$currentPrice <- poolData$stakediff
  })
  
  output$priceGrid <- renderDataTable(v$priceGrid)
  output$curPriceInfo <- renderText(v$curPriceInfo[1])
  output$similarReturns <- renderDataTable(v$similarReturns)
  
  #Market
  output$USD <- renderText(sprintf("$%.2f", round(as.numeric(DCRBTC$last) * BTCUSD, digits = 2)))
  output$BTC <- renderText(sprintf("%.8f BTC",round(as.numeric(DCRBTC$last), digits = 8)))
  output$low <- renderText(sprintf("$%.2f", round(as.numeric(DCRBTC$low24hr) * BTCUSD, digits = 2)))
  output$high <- renderText(sprintf("$%.2f", round(as.numeric(DCRBTC$high24hr) * BTCUSD, digits = 2)))
  output$OHLC <- renderPlot(getOHLCPlot())
  
  #Network
  output$height <- renderText(blockData$height)
  output$time <- renderText(sprintf('%d minutes, %d seconds', lubridate::minute(blockTimeData), lubridate::second(blockTimeData)))
  output$hash <- renderText(paste(round(hashData, 2), "Th/s"))
  output$diff <- renderText(blockData$diff)
  
  #Voting
  output$voteLine <- renderPlot(getVotingLine())
  output$quromProgress <- renderPlot(getQuorumProgress())
  output$agendaItems <- renderTable(getAgendaTable())
  output$agendaPlots <- renderPlot(getAgendaPlots())

  #PoS and Pool
  output$price <- renderText(round(poolData$stakediff, 2))
  output$pool <- renderText(blockData$ticket_pool.size)
  output$mem <- renderText(ifelse(is.null(feeData), 0, feeData$length[1]))
  output$avgfee <- renderText(round(mean(feeData$fee), 2))
  output$estMin <- renderText(round(stakeData$estimates.min, 2))
  output$estMax <- renderText(round(stakeData$estimates.max, 2))
  output$estExp <- renderText(round(stakeData$estimates.expected, 2))
  output$changeBlocks <- renderText(paste("Block", poolData$window_block_index, "of 144"))
  output$changeTime <- renderText(getWindowTime())
  output$poolVal <- renderText(poolValue[[2]])
  output$poolAvg <- renderText(poolValue[[3]])
  output$poolPct <- renderText(round(poolValue[[2]] / coinSupply[1] * 100), 2)
  
  #Graphs
  output$ticketSplit <- renderPlot(getTypeSplit())
  output$feeBar <- renderPlot(getFeeDetailBar())
  output$feeSummary <- renderPlot(getFeeBoxplot())
  output$feeFreqpoly <- renderPlot(getFeeDetailFreqpoly())
  
  #Update dynamic panels
  updateTextInput(session, "priceNow", value = round(poolData$stakediff, 2))
  updateTextInput(session,"feesNow", value = round(mean(feeData$fee), 2))
  updateSliderInput(session, "priceSlider", value = c(max(20,round(poolData$stakediff, 0) - 10), min(200, round(poolData$stakediff, 0) + 10)))
  updateSliderInput(session, "feeSlider", value = c(max(0.01,round(poolData$feeinfo.mean, 0) - 0.1), min(2, round(poolData$feeinfo.mean, 0) + 0.1)))
  })

#generate grid
# price <- seq(20, 200, 1)
# fee <- c(0.001,seq(0.01, 2.00, 0.01))
# grid <- expand.grid(fee, price)
# names(grid) <- c("fee", "price")
# saveRDS(grid, "grid.rds")
