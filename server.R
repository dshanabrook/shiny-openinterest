library(shiny)
#library(htmltools) 
library(ggplot2)
library(quantmod)
library(scales)
source("./source/googleInput.R")
source("./source/functions.R")
doDebug <<- T



shinyServer(function(input, output, clientData, session) {
	if (doDebug) print("update")
	
	googChains <- reactive(withProgress(message="Getting data from Google", value=10,
		getOptionChainGoogle(input$ticker)))
			
	quote <- reactive(getQuote(googChains()))
	expirations <- reactive(levels(as.factor(googChains()[,"expiry"])))
	
	chains <- reactive(mergePutsCalls(googChains()))
	chain1 <- reactive(getOneExpiration(chains(),input$expiry,input$allExpiry))
	chain2 <- reactive(naToZero(chain1()))
	strikeData <- reactive(getStrikes(chain2(),input$strikes, quote()))
	chain <- reactive({
		if (input$graphType == "cumulative")
			useCumulative(chain2(),strikeData())
		else
			chain2()
			})
    observe(
   		updateSelectInput(session,"expiry",choices= expirations()))
   		
	output$tickerText <- renderText({paste("Last quote ",input$ticker,": $", quote(), sep="")})
		
output$OIplot <- renderPlot({
withProgress(message="Now Plot the Data", value=10,{
	if (doDebug) print("oiPlot")
		
	p <- ggplot(chain(), aes(x = strike))
	p <- p + geom_vline(xintercept=quote(), linetype=3)

	p <- p + geom_area(aes(y = putOI,  fill = "1 put",  stat = "bin"), alpha = 0.5)
	p <- p + geom_area(aes(y = callOI, fill = "2 call",  stat = "bin"), alpha = 0.5)
	       
	p <- p + geom_point(aes(y = callOI), size = 1.5, alpha = 0.5, color = "blue", na.rm=T)
	p <- p + geom_point(aes(y = putOI),  size = 1.5, alpha = 0.5, color = "red", na.rm=T)
	       
	p <- p + scale_x_continuous(limits = c(strikeData()$lower, strikeData()$upper), breaks = strikeData()$range) + scale_y_continuous(labels=comma) 
	
	p <- p + theme(legend.title = element_blank())
	p <- p + theme(axis.text.x = element_text(angle = 90, size = 12))
	p <- p + theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
	p <- p + theme(legend.justification = c(1, 1), legend.position = c(1, 1))
	p <- p + ylab("open interest")
	return(p)
	})	
	})
})
