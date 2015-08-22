#local test 
library(shiny)
library(quantmod)
library(ggplot2)
library(RCurl)

#use command f3

setwd("~/ShinyApps/openinterest")
source("source/yahooInput.R")
source("source/googleInput.R")
source("source/openIntFunctions.R")
source("source/plotFunctions.R")
source("source/getOptionChainPatch.R")

doDebug <<- T
stock <- "AAPL"
sym <- stock
yymmdd <- "150814"
strikes <- 16
smoothOn <- F
pinByStrikes <- F

lastQuote <- getQuote(stock)$Last
#chain <- getYahooDataReformatted(stock, yymmdd, "yahoo")#exp <- #putInDateFormat(yymmdd)

lower <- 100
upper <- 130
theN <- 30
theSize <- 12
options
optionList <- getOptionChainGoogle(sym)

optionMin <- subset(optionList, select=c(expiry,type,strike,open.interest))
puts <- subset(optionMin, type=="Put")
calls <- subset(optionMin, type=="Call")
putCall <- merge(puts, calls, by=c("expiry","strike"))
putCall <- subset(putCall, select=-c(type.x,type.y))
names(putCall) <- c("expiry", "strike", "putOI", "callOI")

minStrike <- as.numeric(putCall[1,]$strike)
maxStrike <- as.numeric(putCall[nrow(putCall),]$strike)

oneExp <- putCall[(putCall$expiry==putCall[1,]$expiry),]
oneExp <- oneExp[(oneExp$strike>=lower)&(oneExp$strike<=upper),]
p <- ggplot(oneExp, aes(x=strike))
p <- p + geom_area(aes(y = putOI,  fill = "1 put", colour = "1 put",   stat = "bin"), alpha = 0.5) + geom_area(aes(y = callOI, fill = "2 call",colour = "2 call",  stat = "bin"), alpha = 0.5)
	p <- p	 + geom_point(aes(y=callOI), size=1.5, alpha=.5, color="blue") + geom_point(aes(y=putOI),  size=1.5, alpha=.5, color="red")
	p <- p + scale_x_continuous(breaks = pretty(lower:upper, n = theN)) + theme(legend.title = element_blank()) + theme(axis.text.x=element_text(angle=90,size=theSize))+ theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+ theme(legend.justification=c(1,1), legend.position=c(1,1))+ ylab("open interest")
	p








//do it here	
Exp <- putInDateFormat(yymmdd)
Call <- paste("getOptionChain", "google", sep = ".")
do.call(Call, list(Symbols=stock, Exp=Exp))
optionList <- getOptionChain(stock, Exp)
goodChain <- nrow(openInt)>0
	strikePar <- getStrikes(openInt, stock, strikes, lastQuote, smoothOn)
	subChain <- truncChain(openInt, strikePar, smoothOn)
	sp <- strikePar
	
	

strikeParam <- setupStrikeParam(openInt, stock, strikes, lastQuote, smoothOn)
	openInt <- subsetOIforStrikes(openInt, strikeParam, smoothOn)
	openInt <- getRidofNA(openInt)
	openInt <- addCumm(openInt)
	openInt <- invertPutOrder(openInt)
getPin(openInt)
getPlot("prettyPlot",openInt,stock, strikes, lastQuote, smoothOn, pinByStrikes=)

plotDensity(openInt, stock, strikes,  lastQuote,smoothOn )
plotBW(openInt, stock, strikes, lastQuote, smoothOn)
plotVolume(openInt, stock, strikes,  lastQuote, smoothOn )
plotCummDiff(openInt, stock, strikes,  lastQuote, smoothOn )
plotCumm(openInt, stock, strikes,  lastQuote, smoothOn )
minCumDiff <- min(abs(openInt$cumDiff))
pin <- openInt[openInt$cumDiff==minCumDiff,"strike"]
#test update check
updatedYet <<- TRUE
oldOI <- 0
expText <- yymmdd
chain <- openInt

expirationDatesFuture <<- c(
                  #   "Oct 31" = "141031",
                     "Nov 07" = "141107",
                     "Nov 14" = "141114",
                     "Nov 22*" = "141122",
                     "Nov 28" = "141128",
                     "Dec 05" = "141205",
                     "Dec 12" = "141212",
                     "Dec 20*" = "141220",
                     "Dec 26" = "141226",
           
             		 "Jan 15* 2015"="150117",
                     "Jan 16* 2016"="160116"
                     )
                     
isitapple(stock, chain, expText)