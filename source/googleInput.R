#https://github.com/joshuaulrich/quantmod/issues/38
library(RCurl)
library(jsonlite)
library(plyr)

getOptionChainGoogle = function(symbol, Exp="") {
  if (doDebug) print("getOptionChainGoogle")
  fixJSON = function(json){
    gsub('([^,{:]+):', '"\\1":', json)
  }
  URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
  URL2 = paste0(URL1, '&expy=%d&expm=%d&expd=%d')

  url = sprintf(URL1, symbol)
  chain = fromJSON(fixJSON(getURL(url)))
  
  options = tryCatch({
  	mlply(chain$expirations, function(y, m, d) {
	    url = sprintf(URL2, symbol, y, m, d)
	    expiry = fromJSON(fixJSON(getURL(url)))
	    expiry$calls$type = "Call"
	    expiry$puts$type  = "Put"
	
	    prices = rbind(expiry$calls, expiry$puts)
	    prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
	    prices$underlying.price = expiry$underlying_price
	    prices$retrieved = Sys.time()
	    prices
  		})
  	},
  	error= function(cond) {
  		return(NULL)
  		}
  	)
	
	if (is.null(options)) 
		return(NULL)

  options = options[sapply(options, class) == "data.frame"]
  options = cbind(data.frame(symbol), rbind.fill(options))
  options = rename(options, c(p="premium", b="bid", a="ask", oi="open.interest"))
  for (col in c("strike", "premium", "bid", "ask"))
    options[, col] = suppressWarnings(as.numeric(options[, col]))
  options[, "open.interest"] = suppressWarnings(as.integer(options[, "open.interest"]))
  col.order = c("symbol", "type", "expiry", "strike", "premium",
                "bid", "ask", "open.interest", "retrieved")
  options[, col.order]
}