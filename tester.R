
library(shiny)

library(ggplot2)
library(RCurl)
library(jsonlite)
library(plyr)
source("./newOI/source/googleInput.R")
doDebug <<- T
theN <- 30
theSize <- 12

stripGoogleChain <- function(googChains) {
	if (doDebug) 
		print("stripGoogleChain")
	print(names(googChains))
	optionMin <- subset(googChains, select = c(expiry, type, strike, open.interest))
	puts <- subset(optionMin, type == "Put")
	calls <- subset(optionMin, type == "Call")
	putCall <- merge(puts, calls, by = c("expiry", "strike"))
	putCall <- subset(putCall, select = -c(type.x, type.y))
	names(putCall) <- c("expiry", "strike", "putOI", "callOI")
	return(putCall)
}
getOneExpiration <- function(chain, exp = NULL) {
	if (doDebug) 
		cat("getOneExpiration")
	if (is.null(exp)) 
		exp <- chain[1, ]$expiry
	return(chain[(chain$expiry == exp), ])
}
sym <- "AAPL"
#shinyServer(function(input, output, session){
#	observeEvent(update,  {
if (doDebug) 
	cat("update")
googChains <- getOptionChainGoogle(sym)
chains <- stripGoogleChain(googChains)
chain <- getOneExpiration(chains)
strikes <- getStrikes(chain, 50)
#expiry <- "2015-08-21"

minStrike <- reactive(chain[1, ]$strike)
# maxStrike <- reactive(chain[nrow(chain),]$strike)
# mid <- reactive(nrow(chain)/2)
# interval <- reactive(chain[mid+1,]$strike - chain[mid,]$strike)

#updateSliderInput(session, "strikeRange", value = minStrike,
#min=minStrike, max=maxStrike, step= interval)

p <- ggplot(chain, aes(x = strike))
p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", stat = "bin"), alpha = 0.5) + 
	geom_area(aes(y = callOI, fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue") + geom_point(aes(y = putOI), 
	size = 1.5, alpha = 0.5, color = "red")
p <- p + scale_x_continuous(limits = c(lower, upper), breaks = pretty(lower:upper, n = 30))
p <- p + theme(legend.title = element_blank) + theme(axis.text.x = element_text(angle = 90, 
	size = theSize)) + theme(panel.grid.minor.y = element_blank, panel.grid.major.y = element_blank) + 
	theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ylab("open interest")
p


#      })   
#})