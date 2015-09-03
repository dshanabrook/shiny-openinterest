#ui
maxStrikes <<- 40
shinyUI(fluidPage(
title="Open Interest Analysis",
fluidRow(
h3("Option Open Interest"),
    plotOutput("OIplot"),
	column(4,
	textInput("ticker","Ticker", value="AAPL")),
	column(4,
	sliderInput("strikes","Strikes:", value=20, min=4, max=maxStrikes)),
	column(4,
	#textInput("expiry","expiration yyyy-mm-dd"))
	 selectInput("expiry", "expiration yyyy-mm-dd",
                    multiple = FALSE,
                    c("")))
	)))
