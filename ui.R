#ui
maxStrikes <<- 40

shinyUI(fluidPage(
title="Open Interest Analysis",
fluidRow(

	h3("Option Open Interest", align="center"),
	h4(textOutput("tickerText")),
    plotOutput("OIplot"),
	column(4,
	textInput("ticker","Ticker", value="AAPL")
	,submitButton(text = "Go", icon = NULL)
	),
	column(4,
	checkboxInput("allStrikes","Use all strikes?", value=F),
#	checkboxInput("allExpiration","Aggregate all expirations?", value=F),
	sliderInput("strikes","Strikes:", value=20, min=4, max=maxStrikes)),
	column(4,
	 selectInput("expiry", "expiration yyyy-mm-dd",
                    multiple = FALSE,
                    c("")))
	)))
