avgPurchasePrice <- function(data, ticker){
  price_quant <- data[data$Ticker==ticker & data$Action == "BUY", c("Price", "Quantity")] 
  weighted.mean(price_quant$Price, price_quant$Quantity)
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


