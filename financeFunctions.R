process_transactions <- function(data, ticker) {
  #only for selected ticker
  sub <-
    data[data$Ticker == ticker ,]
  sub <-
    sub[order(sub$Date, decreasing = TRUE), ]
  avgPurchasePrice_cb_total <- vector()
  for (d in sub$Date) {
    avgPurchasePrice_cb_total <-
      rbind(avgPurchasePrice_cb_total,
            c(
              "CostBasis"=round(sum(sub$Amount[sub$Date <= d]), digits = 2)
              ,
              "TotalShares"=round(sum(sub$NumShares[sub$Date <= d]), digits = 2)
            ))
  }
  avg<-avgPurchasePrice_cb_total[,"CostBasis"]/avgPurchasePrice_cb_total[,"TotalShares"]
  cbind(sub, avgPurchasePrice_cb_total, "AvgPurchasePrice" = avg)
}

avgOfSeq <- function(data, tickers) {
  result <- vector()
  for (t in tickers) {
    result <- rbind(result, process_transactions(data, t))
  }
  result
}

trans <- read.csv("401KCleaned.csv")
# remove rows with amount less than one dollar. messes up data visualizations 
trans <- trans[abs(trans$Amount) > 1, ]
# set date column as actual date
trans$Date <- as.Date(trans$Date, "%m/%d/%Y")
trans <- trans[order(trans$Date, decreasing = TRUE), ]
# round up price to two
trans$Price<- round(trans$Price, digits = 2)
exploded <- avgOfSeq(trans, unique(trans$Ticker))
library(ggplot2)
qplot(Date, CostBasis, data=exploded, color = Ticker, geom = "line")
