library(shiny) 
library(ggplot2)
library(quantmod)
library(scales)
source("./source/googleInput.R")
doDebug <<- T
theSize <- 12

getAQuote <- function(sym){
	if (doDebug) print("getLastQuote")
	theQuote <- getQuote(sym)
	return(theQuote$Last)
}

#removes unused chain fields
#merges puts and calls 
#fields: expiry, strike, putOI, callOI
mergePutsCalls <- function(googChains) {
	if (doDebug) print("mergePutsCalls")
	
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

#change OI to cummulative OI below puts, above calls
useCummulative <- function(chain, strikeData) {
	if (doDebug) print("useCummulative")

		chain <- truncateChain(chain, strikeData)
		chain$callOI <- cumsum(chain$callOI)
		chain$putOI <- rev(cumsum(rev(chain$putOI)))

	return(chain)
}


#determine strike price range based on the # strikes requested
getStrikes <- function(chain, inputStrikes, quote) {
	if (doDebug) print("getStrikes")
	
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
	try(chain[is.na(chain$callOI),]$callOI <- 0, silent=T)
	try(chain[is.na(chain$putOI),]$putOI <- 0, silent=T)
	return(chain)
}

shinyServer(function(input, output, clientData, session) {
	if (doDebug) print("update")
	
	googChains <- reactive(withProgress(message="Getting data from Google", value=10,
		getOptionChainGoogle(input$ticker)))
			
	quote <- reactive(getAQuote(input$ticker))
	expirations <- reactive(levels(as.factor(googChains()[,"expiry"])))
	
	chains <- reactive(mergePutsCalls(googChains()))
	chain1 <- reactive(getOneExpiration(chains(),input$expiry,input$allExpiry))
	chain2 <- reactive(naToZero(chain1()))
	strikeData <- reactive(getStrikes(chain2(),input$strikes, quote()))
	chain <- reactive({
		if (input$graphType == "cummulative")
			useCummulative(chain2(),strikeData())
		else
			chain2()
			})
    observe(
   		updateSelectInput(session,"expiry",choices= expirations()))
   		
	output$tickerText <- renderText({paste("Last quote (delayed) ",input$ticker,": $", quote(), sep="")})
		
output$OIplot <- renderPlot({
withProgress(message="Now Plot the Data", value=10,{
	if (doDebug) print("oiPlot")
		
	p <- ggplot(chain(), aes(x = strike))
	p <- p + geom_vline(xintercept=quote(), linetype=3)

	p <- p + geom_area(aes(y = putOI,  fill = " put",  stat = "bin"), alpha = 0.5)
	p <- p + geom_area(aes(y = callOI, fill = "call",  stat = "bin"), alpha = 0.5)
	       
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue", na.rm=T)
	p <- p + geom_point(aes(y = putOI),  size = 1.5, alpha = 0.5, color = "red", na.rm=T)
	       
	p <- p + scale_x_continuous(limits = c(strikeData()$lower, strikeData()$upper), breaks = strikeData()$range) + scale_y_continuous(labels=comma) 
	
	p <- p + theme(legend.title = element_blank())
	p <- p + theme(axis.text.x = element_text(angle = 90, size = theSize))
	p <- p + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
	p <- p + theme(legend.justification = c(1, 1), legend.position = c(1, 1))
	p <- p + ylab("open interest")
	return(p)
	})	
	})
})
