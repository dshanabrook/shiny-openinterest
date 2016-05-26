#ui

shinyUI(fluidPage(
title="Open Interest Analysis",
fluidRow(

	h3("Option Open Interest", align="center"),
	h4(textOutput("tickerText"), align="center"),
	br(),
    plotOutput("OIplot"),
    br(),br(),
	column(4,
	textInput("ticker","Ticker", value="AAPL")
	,submitButton(text = "Go", icon = NULL)
	),
	column(4,
		selectInput("graphType", "Plot type:",list(
			   "Open Interest" = "prettyPlot"
		     , "Cummulative OI"= "cummulative"
		     #,"Volume" = "OIvol"
		     #,"Call-Put OI difference" = "OIDiff"
		     #, "Cummulative Diff"="cummDiff"
		     )),
		numericInput("strikes","Strikes:", value=20, min=2)),
	
	column(4,
	 selectInput("expiry", "expiration yyyy-mm-dd", multiple = FALSE, c("")),
     checkboxInput("allExpiry", "Use all expirations?", value=F),
     #checkboxInput("smoothOn", label = strong("Smooth strike interval?"),value = F),
	br()
	))))
