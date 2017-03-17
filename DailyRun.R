library(quantmod)

########################################################
# Makes a dynamic ticker array from google spreadsheet #
########################################################
# removed
radio = read.table(abc, header = TRUE, sep = ",", stringsAsFactors = FALSE)
boog <- c()
for (u in 1:nrow(radio)) {
	boog <- c(boog, radio[u,1])
	}
	
tickers <- boog
todaysdate30 = Sys.Date()-34 #20 business days ago
#todaysdate7 = Sys.Date()-7 #5 business days ago (account for long weekend)


tempdate1 <- "2017-01-17"
tempdate2 <- "2017-03-02"

tick20 <- c() #create an empty vector (AKA array) #a <- c(a, 5)  (to append 5 to vector a)
tick5 <- c() # create an empty vector


setDefaults(getSymbols,verbose=FALSE,src='google')

getSymbols("FB", env = .GlobalEnv, from = todaysdate30, to = Sys.Date())


for(i in 1:length(tickers)) {
	getSymbols(tickers[i], env = .GlobalEnv, from = todaysdate30, to = Sys.Date())
	
	#getSymbols.google(tickers[i], env = .GlobalEnv, from = todaysdate30, to = Sys.Date()) #only gets dates specified
	#getSymbols(tickers[i], env = .GlobalEnv, from = tempdate1, to = tempdate2)
	#getSymbols.google(tickers[i], env = .GlobalEnv, from = tempdate1, to = tempdate2) #only gets dates specified
 }

#sgetSymbols.google(tickers, env = .GlobalEnv, from = todaysdate30, to = Sys.Date()) #only gets dates specified
#getSymbols.yahoo(tickers, env = .GlobalEnv, from = todaysdate30, to = Sys.Date()) #only gets dates specified, in this case 30 days #updates from yesterday sooner

#iterate through tickers to build tick20 and tick5 lists
for(i in 1:length(tickers)) {
	 p <- get(tickers[i]) #get() returns the instance so I can call from a variable
	 if (nrow(p) < 20) { next }	 #make it a dataframe
	 pdf <- data.frame(p)
	 if (last(pdf[,4]) > first(pdf[,4])) {
	 	m <- (last(pdf[,4]) - first(pdf[,4]))/nrow(pdf)
	 	if(m > .15){
	 	#if the close today is higher than 30 days ago and the slope isn't crazy
	 	#add the ticker to the tick20 list
	 	tick20 <- c(tick20, tickers[i])	 	}
	 }
	 #get last five days only
	 lastfive <- last(p,'5 days') #gets last 5 days 
	 lastfivepdf <- data.frame(lastfive)
	 if (last(lastfivepdf[,4]) > first(lastfivepdf[,4])) {
	 	tick5 <- c(tick5, tickers[i])
	 }
}

#set up plot for tick5
#l <- round(length(tick5)/2,0) +1
#par(mfrow=c(l,2))

xax5 <- c(1,2,3,4,5)
xax20 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
final5 <- c()
final20 <- c()


for(i in 1:length(tick5)) {
	jk <- last(get(tick5[i]), '5 days')
	#find slope of jk
	jh <- data.frame(jk) #slope is coming out the same for all tickers...
	m5 <- (last(jh[,4]) - first(jh[,4]))/5
	yax = jh[,4]
	lm.r5 = lm(yax ~ xax5)
	s5 = summary(lm.r5)
	sig5 = s5$sigma
	if (m5 < 1) { 
		if (m5 > .2){
			if (sig5 < 0.5) {
			#chartSeries(jk, type='line', theme='white', name=paste(tick5[i], m5, sig5, sep=" "))
			print(paste(tick5[i], m5, sig5, last(jh[,4]), sep=" "))
			final5 = c(final5, tick5[i])
			}
		}
	}
}

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
			#chartSeries(jj, type='line', theme='white', name=paste(tick20[i], m20, sig20, sep=" "))
			vlast <- last(jh[,5])
			jmean <- mean(jh[,5])
			
			difff <- vlast - jmean
			k <- difff*100 / jmean

			print(paste(tick20[i], m20, sig20, last(jh[,4]), k, sep=" "))
			final20 = c(final20, tick20[i])
			}
		}
	}
}		
	
	
