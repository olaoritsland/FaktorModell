plot_factor_returns_cum <- function(.data) {
  
  .data %>% 
    mutate_if(is.numeric, cumsum) %>% 
    pivot_longer(-date) %>%  
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line()
  
}

  
