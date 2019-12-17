get_factor_data <- function() {
  
  url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_5_Factors_Daily_CSV.zip"
  csv_file <- "Global_5_Factors_Daily.csv"
  
  temp <- tempfile()
  download.file(url, temp, quiet = TRUE)
  
  factors <- 
    read_csv(unz(temp, csv_file), skip = 6) %>% 
    rename(date = X1, mkt = `Mkt-RF`) %>%
    rename_at(c("SMB", "HML", "RMW", "CMA", "RF"), .funs = tolower) %>% 
    mutate(date = lubridate::ymd(date)) %>%
    mutate_if(is.numeric, funs(. / 100)) 
  
  return(factors)
}
