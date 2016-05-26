openinterest:
===========
Graphing Stock Option Open Interest
===========

This Shiny R code will plot option stock open interest for any valid stock ticker (a stock with options) and for a given expiration date.

* [Graphing choices:](#graphingchoices)
  * Ticker symbol
  * Graph Type
    * open Interest
    * cumulative
  * Expiration (available expiration dates for the ticker.)
  * All expirations - compbines all future expirations

Explanation
===========

Options are different from stocks in that a stock has a float, the number of shares available, where options have open interest.  For any particular expiration date, for any strike, there are a certain number of put and call contracts: the open interest.   Note that this is not the same as the volume.  

Open interest is not calculated during the day.  It is calculated and posted after market close and before next market open.  

Cumulative graph is simply the cumulative sum of put contracts as the strikes increase, and for calls it is as strike prices decrease.

All expirations simply is the simultaneous plotting of all expirations

Executing
=========
Run on my shinyio server: https://bravo.shinyapps.io/openinterest/

To run locally install R, then:
    
    install.packages(c("shiny", "ggplot2", "quantmod", "scales"), repos="http://cran.rstudio.com/")
    library(shiny)
    runGitHub("shiny-openinterest", "dshanabrook")library(shiny) 
    
![alt text](https://cloud.githubusercontent.com/assets/1490174/15562059/d368dfb0-22c9-11e6-854e-0686bbfd4ce9.png)
