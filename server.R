library(shiny)
library(ggplot2)
library(jsonlite)
library(quantmod)
source("source/googleInput.R")
doDebug <<- T
theSize <- 12

getAQuote <- function(sym){
	if (doDebug) print("getLastQuote")
	theQuote <- getQuote(sym)
	return(theQuote$Last)
}

mergePutsCalls <- function(googChains) {
	if (doDebug) 
		print("mergePutsCalls")
	optionMin <- subset(googChains, select = c(expiry, type, strike, 
		open.interest))
	puts <- subset(optionMin, type == "Put")
	calls <- subset(optionMin, type == "Call")
	putCall <- merge(puts, calls, by = c("expiry", "strike"), sort=F)
	putCall <- subset(putCall, select = -c(type.x, type.y))
	names(putCall) <- c("expiry", "strike", "putOI", "callOI")
	return(putCall)
}
getOneExpiration <- function(chains, exp = "",allExpiry=FALSE) {
	if (doDebug) 
		print("getOneExpiration")
	if (exp=="")
		exp <- chains[1, ]$expiry
	if (!allExpiry){
		return(chains[(chains$expiry == exp), ])
	} else {
		allChains <- subset(chains, select=-expiry)
		allChains <- aggregate(.~strike, data=allChains, sum)
		return(allChains)}
}

getStrikes <- function(chain, inputStrikes,  quote, allStrikes=FALSE, method="quote") {
	if (doDebug)
		print("getStrikes")
    
    if ((method=="quote") & !allStrikes)
    		midIndex <- which.min(abs(chain$strike-quote))
    	else
		midIndex <- which.max(chain$callOI+chain$putOI)
	print(chain[midIndex,"strike"])
	
	
	if (allStrikes) {
		lowerIndex <- 1
		upperIndex <- nrow(chain)
	} else {
		lowerIndex <- midIndex - inputStrikes %/% 2
		upperIndex <- midIndex + inputStrikes %/% 2
		if (lowerIndex < 1) lowerIndex <- 1
		if (upperIndex > nrow(chain)) upperIndex <- nrow(chain)
		}
	
	lower <- chain[lowerIndex,"strike"]
	upper <- chain[upperIndex,"strike"]
	range <- round(chain[lowerIndex:upperIndex,"strike"])
	print(range)
	strikeData <- list(upper = upper, lower = lower, range = range)
	return(strikeData)
}
shinyServer(function(input, output, clientData, session) {
	if (doDebug) 
		print("update")
	
	googChains <- reactive(
	withProgress(message="Getting data from Google", value=10,
		getOptionChainGoogle(input$ticker)
	))
	quote <- reactive(getAQuote(input$ticker))
	expirations <- reactive(levels(as.factor(googChains()[,"expiry"])))
	chains <- reactive(mergePutsCalls(googChains()))
#	chain <- reactive(getOneExpiration(chains(),input$expiry, input$allExpiration))
	chain <- reactive(getOneExpiration(chains(),input$expiry,input$allExpiry))
	strikeData <- reactive(getStrikes(chain(),input$strikes, quote(), input$allStrikes))
    observe(updateSelectInput(session,"expiry",choices= expirations()))
    

output$tickerText <- renderText({paste("Last quote (delayed) ",input$ticker,": $", quote(), sep="")})
		
output$OIplot <- renderPlot({
withProgress(message="Now Plot the Data", value=10,{
	if (doDebug) 
		print("oiPlot")
	p <- ggplot(chain(), aes(x = strike))
	p <- p + geom_vline(xintercept=quote(),show_guide=T, linetype=3)
	p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", stat = "bin"), alpha = 0.5) + geom_area(aes(y = callOI, fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
	       
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue") + geom_point(aes(y=putOI), size = 1.5, alpha = 0.5, color = "red")
	       
	p <- p + scale_x_continuous(limits = c(strikeData()$lower, strikeData()$upper)
	, breaks = strikeData()$range)
	
	#pretty(strikes()$lower:strikes()$upper, n = strikes()$strikesToPlot)))
	p <- p + theme(legend.title = element_blank()) + theme(axis.text.x = element_text(angle = 90, 
		size = theSize)) + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) + 
		theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ylab("open interest")
	return(p)
	})	
	})
})
