#ui
shinyUI(fluidPage(
title="Open Interest Analysis",
fluidRow(
h3("Option Open Interest"),
    br(),br(),
    plotOutput("OIplot"),

	br(),br(),
	column(6,
	textInput("ticker","Ticker", value="AAPL"),
	actionButton("update","update"),br(),
	submitButton("submit"),
	#sliderInput("strikeRange", "Strikes:", 
     #           min=0, max=1000, value=c(20,200)),
    # uiOutput("slider"),
    sliderInput("slider", "Strike Range", 
    				min=30, max=150, value=c(100,150)),
	
	textInput("expiry","expiration yyyy-mm-dd")
	))))
