library(quantmod) # Load package

Symbols = c("AAPL","GOOG") # Define stock quotes
column <- c("Date","Open","High","Low","Close","Volume","Adj.Close") # df col names
startdate = Sys.Date()-30 # Two months
enddate = Sys.Date()

# Extract and assign data to corresponding df
for (i in 1:length(Symbols)){
  df <- as.data.frame(getSymbols(Symbols = Symbols[i], src = "yahoo", 
                                 from = startdate, to = enddate, env = NULL))
  assign(Symbols[i], setNames(data.frame(rownames(df), df), column))
}

month <- unique(months(as.Date(AAPL$Date))) # Trace the months

# Create empty multiple lists with identical item names
for (i in 1:length(month)){ 
  assign(month[i],as.list(setNames(data.frame(matrix(0, ncol = 2)),Symbols)))
}

# Assign df to corresponding list items
February[["AAPL"]] <- AAPL[months(as.Date(AAPL$Date))==month[1],]
February[["GOOG"]] <- GOOG[months(as.Date(GOOG$Date))==month[1],]
March[["AAPL"]] <- AAPL[months(as.Date(AAPL$Date))==month[2],]
March[["GOOG"]] <- GOOG[months(as.Date(GOOG$Date))==month[2],]
