library(shiny)
library(ggplot2)
library(jsonlite)
source("./openinterest/source/googleInput.R")
ticker <- "GOOG"
sym <- ticker
inputstrikes <- 20
doDebug <<- T
theSize <- 12

googChains <- getOptionChainGoogle(ticker)
	chains <- mergePutsCalls(googChains)
	chain <- getOneExpiration(chains)
	strikes <- getStrikes(chain,inputstrikes)
	expiry <- googChains[1,"expiry"]
	selectInputChoices <- levels(as.factor(googChains[,"expiry"]))
	chain <- getOneExpiration(googChains,expiry)
		p <- ggplot(chain, aes(x = strike))

		
	p <- p + geom_area(aes(y = putOI, fill = "1 put", colour = "1 put", stat = "bin"), alpha = 0.5) + geom_area(aes(y = callOI, fill = "2 call", colour = "2 call", stat = "bin"), alpha = 0.5)
	       
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue") + geom_point(aes(y=putOI), size = 1.5, alpha = 0.5, color = "red")
	       
	p <- p + scale_x_continuous(limits = c(strikeData$lower, strikeData$upper, breaks = strikeData$range))
	
	#pretty(strikes$lower:strikes$upper, n = strikes$strikesToPlot)))
	p <- p + theme(legend.title = element_blank()) + theme(axis.text.x = element_text(angle = 90, size = theSize)) 
	p  <- p +theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) + theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ylab("open interest")
p