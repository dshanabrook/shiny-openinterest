#functions for openinterest
#removes unused chain fields
#merges puts and calls 
#fields: expiry, strike, putOI, callOI
mergePutsCalls <- function(googChains) {
	if (doDebug) print("mergePutsCalls")
	if (is.null(googChains)) return(NULL)
	
	optionMin <- subset(googChains, select = c(expiry, type, strike, open.interest))
	puts <- subset(optionMin, type == "Put")
	calls <- subset(optionMin, type == "Call")
	putCall <- merge(puts, calls, by = c("expiry", "strike"), sort=F)
	putCall <- subset(putCall, select = -c(type.x, type.y))
	names(putCall) <- c("expiry", "strike", "putOI", "callOI")
	return(putCall)
}

#extract the expriation to be plotted.
#allExpiry combines all expirations
getOneExpiration <- function(chains, expiry="", allExpiry=FALSE) {
	if (doDebug) print("getOneExpiration")
	if (is.null(expiry)) return(NULL)
	
#initially, exp="" because expirations haven't propaged to UI
	if (expiry=="")
		expiry <- chains[1, ]$expiry
	if (!allExpiry){
		return(chains[(chains$expiry == expiry), ])
	} else {
		allChains <- subset(chains, select=-expiry)
		allChains <- aggregate(.~strike, data=allChains, sum)
		return(allChains)}
}

#keep only strike range of chain
truncateChain <- function(chain, strikeData) {
	if (doDebug) print("truncateChain")
	
	chainx <- subset(chain, chain$strike>= strikeData$lower)
	chainy <- subset(chainx, chainx$strike <= strikeData$upper)
	return(chainy)

}

#change OI to cumulative OI below puts, above calls
useCumulative <- function(chain, strikeData) {
	if (doDebug) print("useCumulative")

		chain <- truncateChain(chain, strikeData)
		chain$callOI <- cumsum(chain$callOI)
		chain$putOI <- rev(cumsum(rev(chain$putOI)))

	return(chain)
}


#determine strike price range based on the # strikes requested
getStrikes <- function(chain, inputStrikes, quote) {
	if (doDebug) print("getStrikes")
	if (is.null(chain)) return(NULL)
	
		midIndex <- which.min(abs(chain$strike-quote))
		lowerIndex <- midIndex - inputStrikes %/% 2
		upperIndex <- midIndex + inputStrikes %/% 2
		if (lowerIndex < 1) lowerIndex <- 1
		if (upperIndex > nrow(chain)) upperIndex <- nrow(chain)
		
		lower <- chain[lowerIndex,"strike"]
		upper <- chain[upperIndex,"strike"]
		range <- round(chain[lowerIndex:upperIndex,"strike"])
		strikeData <- list(upper = upper, lower = lower, range = range)

		return(strikeData)
}

#remove na from chains
naToZero <- function(chain){
	options(warn=-1)
	if (doDebug) print("naToZero")
	if (is.null(chain)) return(NULL)
	
	naCalls <- is.na(chain$callOI)
	naPuts <- is.na(chain$putOI)
	chain[naCalls]$callOI <- 0
	chain[naPuts]$putOI <- 0
	options(warn=1)
	return(chain)
}

getGoogleQuote <- function(googChains) {
	quote = tryCatch({ round(googChains[1,"underlying.price"], digits=3)},
			error = function(cond) return(NULL)
	)
	return(quote)
}