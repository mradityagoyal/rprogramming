avgPurchasePrice <- function(data, ticker){
  price_quant <- data[data$Ticker==ticker & data$Action == "BUY", c("Price", "NumShares")] 
  weighted.mean(price_quant$Price, price_quant$NumShares)
}

avgOfSeq <- function(data, tickers){
  result <- vector()
  for(t in tickers){
    result <- rbind(result, c(t, avgPurchasePrice(data, t)))
  }
  colnames(result) <- c("Ticker", "AvgPurchasePrice")
  colnames
  result
}
trans <- read.csv("401KCleaned.csv")
avgPrices <- avgOfSeq(trans, unique(trans$Ticker))
 
