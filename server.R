#server
library(shiny)
#library(quantmod)
library(ggplot2)
library(RCurl)
library(jsonlite)
library(plyr)
source("source/googleInput.R")
doDebug <<- T
theN <- 30
theSize <- 12
maxStrikes <<- 40

doPlot <- function(chain, lower, upper) {
	p <- ggplot(chain, aes(x = strike))
	p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", stat = "bin"), alpha = 0.5)+ 
		geom_area(aes(y = callOI, fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue") + geom_point(aes(y=putOI), size = 1.5, alpha = 0.5, color = "red")
	p <- p + scale_x_continuous(limits = c(lower, upper), breaks = pretty(lower:upper, n = 30))
	p <- p + theme(legend.title = element_blank()) + theme(axis.text.x = element_text(angle = 90, 
		size = theSize)) + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) + 
		theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ylab("open interest")
	return(p)
}
mergePutsCalls <- function(googChains) {
	if (doDebug) 
		print("stripGoogleChain")
	optionMin <- subset(googChains, select = c(expiry, type, strike, 
		open.interest))
	puts <- subset(optionMin, type == "Put")
	calls <- subset(optionMin, type == "Call")
	putCall <- merge(puts, calls, by = c("expiry", "strike"), sort=F)
	putCall <- subset(putCall, select = -c(type.x, type.y))
	names(putCall) <- c("expiry", "strike", "putOI", "callOI")
	return(putCall)
}
getOneExpiration <- function(chain, exp = "") {
	if (doDebug) 
		print("getOneExpiration")
	if (exp=="")
		exp <- chain[1, ]$expiry
	return(chain[(chain$expiry == exp), ])
}
getGoodChain <- function(sym) {
	googChains <- getOptionChainGoogle(sym)
	chains <- stripGoogleChain(googChains)
	chain <- getOneExpiration(chains)
	return(chain)
}
getStrikes <- function(chain, strikesWanted) {
	if (doDebug)
		print("getStrikes")
	min <- chain[1, ]$strike
	max <- chain[nrow(chain), ]$strike
    
	midIndex <- which.max(chain$callOI+chain$putOI)
	interval <- chain[midIndex + 1, ]$strike - chain[midIndex, ]$strike
	lowerIndex <- midIndex - strikesWanted %/% 2
	upperIndex <- midIndex + strikesWanted %/% 2
	if (lowerIndex < 1) lowerIndex <- 1
	if ((upperIndex > nrow(chain))|(strikesWanted==maxStrikes)) upperIndex <- nrow(chain)
	lower <- chain[lowerIndex,"strike"]
	upper <- chain[upperIndex,"strike"]
	strikes <- data.frame(min = min, max = max, upper = upper, lower = lower, 
		interval = interval)
	return(strikes)
}
shinyServer(function(input, output, clientData, session) {
	if (doDebug) 
		print("update")
	
	googChains <- reactive({getOptionChainGoogle(input$ticker)})
	chains <- reactive({mergePutsCalls(googChains())})
	chain <- reactive({getOneExpiration(chains(),input$expiry)})
	strikes <- reactive({getStrikes(chain(),input$strikes)})
   # observe({updateTextInput(session, "expiry",label="default expiration", value=googChains()[1,"expiry"])})
   observe({updateSelectInput(session, "expiry",
   	choices=googChains()[,"expiry"])})
     
			
output$OIplot <- renderPlot({
	if (doDebug) 
		print("oiPlot")
		p <- ggplot(chain(), aes(x = strike))
	p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", stat = "bin"), alpha = 0.5)+ 
		geom_area(aes(y = callOI, fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue") + geom_point(aes(y=putOI), size = 1.5, alpha = 0.5, color = "red")
	p <- p + scale_x_continuous(limits = c(strikes()$lower, strikes()$upper), breaks = pretty(strikes()$lower:strikes()$upper, n = 30))
	p <- p + theme(legend.title = element_blank()) + theme(axis.text.x = element_text(angle = 90, 
		size = theSize)) + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) + 
		theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ylab("open interest")
		return(p)
	})

})
