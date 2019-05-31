## TO DO

## This file has 2 parts
### A.  Downloading data from the web and storing it into the machine. 
### B.  Loading data from the machine into the system

##############################################
###  Loading libraries      ##################
##############################################
library(httr)
library(rvest)

##############################################
###  A. Downloading & storing data      ######
##############################################

### Get the market cap of a ticker using Yahoo and plain web scrapping. 
get_market_cap <- function(symbol){
  # We get the Yahoo! stats
  print(symbol)
  url <- paste0('https://finance.yahoo.com/quote/',symbol, "?p=", symbol)
  yo <- url %>% read_html() %>% html_table()
  # Here a few trials and errors are necessary to get the right piece of data (in this case market cap)
  return(yo[[2]][1, 2])
}

### download market data using the tiingo api
get_tiingo_data <- function(ticker, start_date){
  
  data <- GET(paste0("https://api.tiingo.com/tiingo/daily/", ticker, "/prices?startDate=", 
                     start_date, 
                     "&token=2e677f550418d4e5f5e2b73bd24104c92f820cfd&format=csv")) %>% 
    content() %>% as_tibble()
  
  write_csv(data, paste0(thePath, "/10_energy/stock_data_ti/", ticker, ".csv"))
}

### download market data using the alpha-vantage API and quantmod library
get_av_data <- function(ticker, start_date){ 
  
  data = quantmod::getSymbols(Symbols = ticker,
                              src = "av",
                              api.key = "W7SHG93NFG5YWE2K", 
                              output.size = "full", 
                              adjusted = TRUE, 
                              auto.assign = FALSE,
                              index.class="POSIXct")
  colnames(data) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  zoo::write.zoo(data, paste0(thePath, "/10_energy/stock_data_av/", ticker, ".csv"), 
                 sep = ",", row.names = FALSE)
  Sys.sleep(7)
}

### wrap both API into one function
### what matter here is the exchange.  Tiingo being just for US stocks.
get_eod_data <- function(df, start_date = "2005-01-02"){
  df_tick <- df %>% select(api_ticker, ticker_stock_exchange) %>% 
    arrange(api_ticker) %>% na.omit()
  
  for (i in 1:nrow(df_tick)){
    print(df_tick$api_ticker[i]) 
    
    if(df_tick$ticker_stock_exchange[i] == "NYSE" ){
      tryCatch({get_tiingo_data(df_tick$api_ticker[i], start_date)}, 
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
    
    else if(df_tick$ticker_stock_exchange[i] == "Nasdaq"){
      get_tiingo_data(df_tick$api_ticker[i], start_date)}
    
    else{
      tryCatch({
        get_av_data(df_tick$api_ticker[i], start_date)}, 
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
    
  }
}


### wrap both API into one function
### what matter here is the exchange.  Tiingo being just for US stocks.
get_eod_data_II <- function(tick, stock_exch, start_date = "2005-01-02"){
  print(paste0("Fetching fin data for ", tick))
    
    if(stock_exch == "NYSE" ){
      tryCatch({get_tiingo_data(tick, start_date)}, 
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
    
    else if(stock_exch == "Nasdaq"){
      tryCatch({get_tiingo_data(tick, start_date)}, 
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
    
    else{
      tryCatch({get_av_data(tick, start_date)}, 
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")})}
  
    
  }






##############################################
###  B. Loading the data                ######
##############################################

load_fin_data <- function(tick, stock_exch) {
  
  tryCatch({
    if(stock_exch == "NYSE"){
      df <- read_csv(paste0(thePath, "/10_energy/stock_data_ti/", tick, ".csv"))
      df <- df %>% mutate(Adjusted = adjClose) %>% 
        select(Index = date, Open = adjOpen, High = adjHigh, Low = adjLow, Close = adjClose, Adjusted)}
    
    else if(stock_exch == "Nasdaq"){
      df <- read_csv(paste0(thePath, "/10_energy/stock_data_ti/", tick, ".csv"))
      df <- df %>% mutate(Adjusted = adjClose) %>% 
        select(Index = date, Open = adjOpen, High = adjHigh, Low = adjLow, Close = adjClose, Adjusted)}
    
    else {
      df <- read_csv(paste0(thePath, "/10_energy/stock_data_av/", tick, ".csv"))}
    
    df$Index <- ymd(df$Index)
    return(df)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


