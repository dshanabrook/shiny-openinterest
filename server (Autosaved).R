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

stripGoogleChain <- function(googChains) {
	if (doDebug) 
		print("stripGoogleChain")
	optionMin <- subset(googChains, select = c(expiry, type, strike, 
		open.interest))
	puts <- subset(optionMin, type == "Put")
	calls <- subset(optionMin, type == "Call")
	putCall <- merge(puts, calls, by = c("expiry", "strike"))
	putCall <- subset(putCall, select = -c(type.x, type.y))
	names(putCall) <- c("expiry", "strike", "putOI", "callOI")
	return(putCall)
}
getOneExpiration <- function(chain, exp = NULL) {
	if (doDebug) 
		print("getOneExpiration")
	if (is.null(exp)) 
		exp <- chain[1, ]$expiry
	return(chain[(chain$expiry == exp), ])
}
getGoodChain <- function(sym) {
	googChains <- getOptionChainGoogle(sym)
	chains <- stripGoogleChain(googChains)
	chain <- getOneExpiration(chains)
	return(chain)
}

doPlot <- function(chain, lower, upper) {
	p <- ggplot(chain, aes(x = strike))
		p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", 
			stat = "bin"), alpha = 0.5) + geom_area(aes(y = callOI, 
			fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
		p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, 
			color = "blue") + geom_point(aes(y = putOI), size = 1.5, 
			alpha = 0.5, color = "red")
		p <- p + scale_x_continuous(limits = c(lower, upper), 
			breaks = pretty(lower:upper, n = 30))
		p <- p + theme(legend.title = element_blank()) + theme(axis.text.x = element_text(angle = 90, 
			size = theSize)) + theme(panel.grid.minor.y = element_blank(), 
			panel.grid.major.y = element_blank()) + theme(legend.justification = c(1, 
			1), legend.position = c(1, 1)) + ylab("open interest")
		return(p)
	
}

getStrikes <- function(chain) {
	if (doDebug) 
		print("getStrikes")
	#20% of range 1/5
	strPerc <- 5
	min <- chain[1, ]$strike
	max <- chain[nrow(chain), ]$strike
	mid <- nrow(chain)/2
	interval <- chain[mid + 1, ]$strike - chain[mid, ]$strike
	lower <- min + min%/%strPerc
	upper <- max - max%/%strPerc
	strikes <- data.frame(min = min, max = max, upper = upper, lower = lower, 
		interval = interval)
	return(strikes)
}
shinyServer(function(input, output, clientData, session) {
	if (doDebug) 
		print("update")

	#chain <- reactive({getGoodChain(input$ticker)})
	strikes <- reactive(getStrikes(chain()))
	lower <- reactive({
		input$slider[1]
	})
	upper <- reactive({
		input$slider[2]
	})
	#	output$slider <- renderUI({		
	#		sliderInput("slider", "Strikes", value=c(strikes()$lower,strikes()#$upper), min=strikes()$min, max=strikes()$max)	})

	googChains <- reactive({
		getOptionChainGoogle(input$ticker)
	})
	chains <- reactive({
		stripGoogleChain(googChains())
	})
	chain <- reactive({
		getOneExpiration(chains())
	})

	#chain <- chains()[(chains()$expiry==input$expiry),]
	
	#	updateTextInput(session, "expiry",label="default expiration", value=options()[1,]$expiry)
	
	output$OIplot <- renderPlot({
		if (doDebug) 
			print("oiPlot")
		doPlot(chain(),lower(),upper())
	})
})


