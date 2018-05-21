if( "Quandl" %in% installed.packages()[,1]){
  library("Quandl")
}  else {
  install.packages("Quandl")
  library("Quandl")
}



if(!'data.table' %in% installed.packages()[,1]) install.packages('data.table')




# Quandl package must be installed
library(Quandl)

# Get your API key from quandl.com
quandl_api = "MYAPIKEY"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

quandl_get <-
  function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8"),  #  Adj. Open
      paste0("WIKI/", sym, ".9"),  # Adj. High
      paste0("WIKI/", sym, ".10"), # Adj. Low
      paste0("WIKI/", sym, ".11"), # Adj. Close
      paste0("WIKI/", sym, ".12")), # Adj. Volume
      start_date = start_date,
      type = "zoo"
    ))
  }