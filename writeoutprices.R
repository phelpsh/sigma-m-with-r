	tickers <- c("HLI", "MCY")
for(i in 1:length(tickers)) {
	curQuote <- getQuote(tickers[i], what="l1")
	print(paste(tickers[i], curQuote[,2]), sep=" ")
	
	}
	
	
	

	
	