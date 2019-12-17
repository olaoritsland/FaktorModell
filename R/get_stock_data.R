get_stock_data <- function(tickers) {
  
  if (length(tickers) > 1) {
  
  tickers %>% 
    tq_get() %>% 
    select(symbol, date, close) %>% 
    group_by(symbol) %>% 
    mutate(returns = close - lag(close),
           returns_log = log(close) - log(lag(close)))
    
  } else {
    
    tickers %>% 
      tq_get() %>% 
      mutate(symbol = tickers) %>% 
      select(symbol, date, close) %>%
      mutate(returns = close - lag(close),
             returns_log = log(close) - log(lag(close)))
  }
  
}
