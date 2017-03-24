somePDFPath = "some.pdf"
pdf(file=somePDFPath)  

for(i in 1:length(tick20)) {
	jj <- last(get(tick20[i]), '20 days')	
	if (nrow(jj) != 20) { next() }
	jh <- data.frame(jj)
	m20 <- (last(jh[,4]) - first(jh[,4]))/20
	
	yax = jh[,4]
	lm.r20 = lm(yax ~ xax20)
	s20 = summary(lm.r20)
	sig20 = s20$sigma
	if (m20 < 1) {
		if (sig20 < 1) {
			if (m20 > .2){
				
				jmean <- mean(jh[,5])
				vlast <- last(jh[,5])
				difff <- vlast - jmean
			    k <- difff*100 / jmean
			
				chartSeries(jj, type='line', theme='white', name=paste(tick20[i], k, sep=" --- "))		
				print(paste(tick20[i], k, sep="   "))
				}
		}
	}
}
dev.off() 		




#chartall tickers

somePDFPath = "some.pdf"
pdf(file=somePDFPath)  

for(i in 1:length(tickers)) {
	result = tryCatch({
    	df <- data.frame(get(tickers[i]))
		}, 
		error = function(e) {
    		print(paste(tickers[i], " should be deleted"))
    		return("error")
    })
    
    if (result == "error") { next() }
    
	df <- data.frame(get(tickers[i]))
	#print(paste(nrow(df), tickers[i], sep="    "))
	
	if (nrow(df) == 1) { next() } #skip to next i
	if (nrow(df) == 0) { next() } #skip to next i
	if (nrow(df) < 20) { next() } #skip to next i
	
	jj <- last(get(tickers[i]), '20 days')	
	if (nrow(jj) != 20) { next() } #skip to next i
		
	chartSeries(jj, type='line', theme='white', name=tickers[i])		

}
dev.off() 



result = tryCatch({
    df <- data.frame(get(tickers[i]))
}, error = function(e) {
    #next{}
    print(paste(tickers[i], " should be deleted"))
})




#### this doesn't work
somePDFPath = "some.pdf"
pdf(file=somePDFPath)
jj <- last(get(tickers[18]), '20 days')

	
chartSeries(jj, type='line', theme='white', name=tickers[18])


######################## START print out bad tickers ################################ 

for(i in 1:length(tickers)) {
	df <- data.frame(tickers[i])
	if (nrow(df) == 1) { 
		print(tickers[i])
	}			
}

######################## END print out bad tickers ################################ 



# for(i in 1:length(tick20)) {
	# jj <- last(get(tick20[i]), '20 days')	
	# if (nrow(jj) != 20) { next() }
	# jh <- data.frame(jj)
	# m20 <- (last(jh[,4]) - first(jh[,4]))/20
	# print(paste(tick20[i], m20, sep=" "))
	# yax = jh[,4]
	# lm.r20 = lm(yax ~ xax20)
	# s20 = summary(lm.r20)
	# sig20 = s20$sigma
	# if (m20 < 1) {
		# if (sig20 < 1) {
			# if (m20 > .2){
			# #chartSeries(jj, type='line', theme='white', name=paste(tick20[i], m20, sig20, sep=" "))
			# vlast <- last(jh[,5])
			# jmean <- mean(jh[,5])
			
			# difff <- vlast - jmean
			# k <- difff*100 / jmean

			# #print(paste(tick20[i], m20, sig20, last(jh[,4]), k, sep=" "))
			# final20 = c(final20, tick20[i])
			# }
		# }
	# }
# }		



for(i in 1:length(tickers)) {
	errcheckkk <- get(tickers[i]) #get() returns the instance so I can call from a variable
	
	if (nrow(errcheckkk) < 20) { next() }	 #make it a dataframe
	
	jj <- last(get(tickers[i]), '20 days')	
	
	if (nrow(jj) != 20) { next() }
	jdf <- data.frame(jj)
	m20 <- (last(jdf[,4]) - first(jdf[,4]))/20
	#print(paste(tickers[i], "slope:", m20, sep=" "))
	
	 if (m20 > 0) {
		jh <- data.frame(jj)	
		jmean <- mean(jh[,5])
		vlast <- last(jh[,5])
		difff <- vlast - jmean
		k <- difff*100 / jmean
			
		print(paste(tickers[i], k, sep="   "))
	}					
}

#31 Jan tickers
tickers <- c("PPH","LOGM","AZO","MNTA","GRMN","CYNO","SANM","NTCT","COH","FRT","LEG","UBSI","ALLY","AXE","SFR","CLH","MAC","NNN","TUP","STML","PRA","NJR","PEI","HUBB","GKOS")









