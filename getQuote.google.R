URL1 <- "http://finance.google.com/finance/info?client=ig&q=%s"

getQuote.google <- function(sym) {
  fixJSON = function(json){
    gsub('([^,{:]+):', '"\\1":', json)
  }
  URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
  URL2 = paste0(URL1, '&expy=%d&expm=%d&expd=%d')

  url = sprintf(URL1, symbol)
  chain = fromJSON(fixJSON(getURL(url)))
  return(chain$underlying_price)
}