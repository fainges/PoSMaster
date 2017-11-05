library(ggplot2)
library(dplyr)
library(quantmod)

# start val, end val, current val, title
TimeLine <- function(sx, ex, current, title) {
  blocks <- data.frame(sx = sx, sy = 0, ex = ex, ey = 0)
  p <- ggplot() + geom_segment(data = blocks , aes(x = sx, y = sy, xend = ex, yend = ey)) +
    geom_segment(data = blocks , aes(x = sx, y = -0.075, xend = sx, yend = 0)) +
    geom_segment(data = blocks , aes(x = ex, y = -0.075, xend = ex, yend = 0)) +
    geom_segment(data = blocks , aes(x = current, y = 0, xend = current, yend = 0.075)) +
    annotate("text", x = blocks$sx, y = -0.12, label = as.character(blocks$sx)) +
    annotate("text", x = blocks$ex, y = -0.12, label = as.character(blocks$ex)) +
    annotate("text", x = current, y = 0.12, label = as.character(current)) +
    ylim(-0.15, 0.15) + theme(plot.background = element_rect(fill = "#F5F5F5"), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(x = "", y = "") + ggtitle(title)
  return(p)
}

getFeeDetailBar <- function(){
  if(is.null(feeData)) return(NULL)
  
  if(nrow(feeData) > 20){
  breaks <- feeData[seq(20, nrow(feeData), 20),]
  p <- ggplot(feeData, aes(feeFactor)) + scale_x_discrete() + geom_vline(data=breaks, colour = "red", linetype = "dotted", aes(xintercept = as.numeric(feeFactor))) +
       geom_bar(width = 0.25) + labs(x="Ticket Fee per KB (red lines show fee cutoffs)", y="Number of Tickets") + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  else {
    p <- ggplot(feeData, aes(feeFactor)) + geom_bar(width = 0.25) + labs(x="Ticket Fee per KB", y="Number of Tickets") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(p)
}

getTypeSplit <- function(){
  if(is.null(feeData)) return(NULL)
  
  split <- feeData
  split$solo <- ifelse(split$size < 500, "SOLO", "POOL")
  p <- ggplot(split, aes(solo)) + geom_bar(width = 0.25) + labs(x="", y="Tickets")
  return(p)
}

getOHLCPlot <- function(){
  return(chartSeries(DCROHLC, type = "candlesticks", subset = NULL, theme = chartTheme("white"), name = "1 Month DCR/USD"))
}

getSupplyChart <- function() {
  totalCoins <- 21000000
  mined <- coinSupply[1]
  pos <- coinSupply[2]
  minedStr <- paste(substring(mined, 1, 1), substring(mined, 2, 4), substring(mined, 5, 7), sep = ",")
  posStr <- paste(substring(pos, 1, 1), substring(pos, 2, 4), substring(pos, 5, 7), sep = ",")
  df <- data.frame(labs=c(paste("UNMINED", total - mined), paste("MINED", minedStr), paste("POS", posStr)), value=c((mined - pos) / mined, pos / mined))
  pie(df$value, col = c("blue","green"), labels = df$labs, main = "21,000,000 DCR Max") 
}

getPriceLine <- function() {
  p <- TimeLine(sprintf("$%.2f", round(as.numeric(DCRBTC$low24hr) * BTCUSD, digits = 2)), sprintf("$%.2f", round(as.numeric(DCRBTC$high24hr) * BTCUSD, digits = 2)), sprintf("$%.2f", round(as.numeric(DCRBTC$last) * BTCUSD, digits = 2)))
  return(p)
}

getVotingLine <- function() {
  p <- TimeLine(as.numeric(voteInfo["startheight"]), as.numeric(voteInfo["endheight"]), blockData$height, "Voting Progress in Blocks")
  return(p)
}

getAgendaPlots <- function() {
  votes <- getAgendaItems()
  if (all(votes$status == "started")) {
    p <- ggplot(votes, aes(x = "", y = pc, fill = id)) +
    geom_bar(width = 1, stat = "identity") +
    scale_fill_manual(values = c("blue", "green", "cyan")) +
    coord_polar(theta = "y")
  } else {
      p <- ggplot(votes, aes(x = rep(0, length(status)), y = rep(0, length(status)), label = status)) +
        geom_text()
    }
  p <- p + theme(panel.background = element_rect(fill = "#F5F5F5"), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(x = "", y = "") + facet_wrap(~item)
  return(p)
}

getQuorumProgress <- function() {
  quorum <- 4032 #number of votes required for quroum
  votes <- getAgendaItems()
  if (all(votes$status == "started")) {
    votes$type <- "vote"
    votes$all <- 40320 #total votes in one window
    votes <- votes %>% group_by(id) %>% top_n(1)
    votes$votes <- ifelse(votes$votes > quorum, quorum, votes$votes)
    votes$total <- sum(votes$votes)
    p <- ggplot(votes, aes(x = "", y = votes)) +
      geom_bar(width = 1, stat =  "identity") + ylim(c(0,quorum))
      labs(x = "", y = "") + facet_wrap(~item)
  } else {
    p <- ggplot(votes, aes(x = rep(0, length(status)), y = rep(0, length(status)), label = status)) +
      geom_text() + theme(panel.background = element_rect(fill = "#F5F5F5"), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(x = "", y = "") + facet_wrap(~item)
  }
  return(p)
}
