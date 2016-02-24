process_transactions <- function(data, investment) {
  #only for selected ticker
  sub <-
    data[data$Investment == investment ,]
  sub <-
    sub[order(sub$Date, decreasing = TRUE), ]
  avgPurchasePrice_cb_total <- vector()
  for (d in sub$Date) {
    avgPurchasePrice_cb_total <-
      rbind(avgPurchasePrice_cb_total,
            c(
              "CostBasis" = round(sum(sub$Amount[sub$Date <= d]), digits = 2)
              ,
              "TotalShares" = round(sum(sub$Shares[sub$Date <= d]), digits = 2)
            ))
  }
  avg <-
    avgPurchasePrice_cb_total[, "CostBasis"] / avgPurchasePrice_cb_total[, "TotalShares"]
  cbind(sub, avgPurchasePrice_cb_total, "AvgPurchasePrice" = avg)
}

avgOfSeq <- function(data, investments) {
  result <- vector()
  for (inv in investments) {
    result <- rbind(result, process_transactions(data, inv))
  }
  result
}

findTicker <- function(x) {
  tickers <-
    cbind(
      investment =
        c(
          "VANG INST INDEX PLUS",
          "VANG TOT INTL STK IP",
          "VANG EXT MKT IDX ISP",
          "FID GROWTH CO POOL",
          "VANGUARD TARGET 2050",
          "VANG TOT INTL STK IS",
          "VANGUARD TARGET 2060"
        ),
      ticker = c(
        "VIIIX",
        "VTPSX" ,
        "VEMPX",
        "MUTF:FIDG",
        "MUTF:VT2050",
        "VTSNX",
        "MUTF:VT2060"
      )
    )
  tickers <- as.data.frame(tickers)
  as.character(tickers$ticker[as.character(tickers$investment) == x])
}

equityOnDate <- function(exploded, date = Sys.Date()) {
  result <- vector()
  for (ticker in unique(exploded$Ticker)) {
    maxDate <-
      max(exploded$Date[exploded$Ticker == ticker &
                          exploded$Date < date])
    if (exploded$Shares[exploded$Date == maxDate &
                        exploded$Ticker == ticker] > 0) {
      result <-
        rbind(result, c(ticker, exploded$CostBasis[exploded$Date == maxDate &
                                                     exploded$Ticker == ticker]))
    }
  }
  result
}

trans <-
  read.csv("history.csv",
           colClasses = c("character", "factor", "factor", "numeric", "numeric"))
#keep change in market value rows for future.
changeInMkt <-
  trans[trans$Transaction.Type == "Change In Market Value",]
#remove change in market value rows from transactions
trans <-
  trans[!trans$Transaction.Type == "Change In Market Value",]
# remove rows with amount less than one dollar. messes up data visualizations
trans <- trans[abs(trans$Amount) > 1, ]
# set date column as actual date
trans$Date <- as.Date(trans$Date, "%m/%d/%Y")
trans <- trans[order(trans$Date, decreasing = TRUE), ]
trans$Price <- round(trans$Amount / trans$Shares, digits = 2)
exploded <- avgOfSeq(trans, unique(trans$Investment))
exploded$Ticker <-
  as.factor(sapply(exploded$Investment, findTicker))
library(ggplot2)
#qplot(Date,
#      CostBasis,
#      data = exploded,
#      color = Ticker,
#      geom = "line")
