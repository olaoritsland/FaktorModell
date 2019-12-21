plot_roll <- function(.data, ticker, width = 200) {
  
  .data <- .data %>% 
    filter(symbol == {{ticker}}) %>% 
    ungroup() %>% 
    select(date, close)
  
  roll_mean_name <- paste0("roll_mean_", width)
  roll_sd_name <- paste0("roll_sd_", width)
  
    plot_data <- .data %>%
      tq_mutate(select = close, 
                mutate_fun = rollapply, 
                width = width, 
                FUN = mean,
                na.rm = TRUE, 
                col_rename = roll_mean_name) %>% 
      tq_mutate(select = close, 
                mutate_fun = rollapply, 
                width = width, 
                FUN = sd,
                na.rm = TRUE, 
                col_rename = roll_sd_name)
    
  
  plot_data %>% 
    pivot_longer(-date) %>% 
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    scale_color_ptol()
}
