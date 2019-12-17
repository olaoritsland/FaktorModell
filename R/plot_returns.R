plot_returns <- function(.data, ticker, return_var) {
  
  mean <- returns %>% 
    filter(symbol == {{ticker}}) %>% 
    summarise(mean = mean({{return_var}}, na.rm = T)) %>% 
    pull(mean)
  
  .data %>% 
    filter(symbol == {{ticker}}) %>% 
    ggplot(aes({{return_var}})) + 
    geom_density(fill = 'forestgreen', alpha = .2) + 
    geom_vline(xintercept = mean, linetype = "dashed") +
    #geom_vline(xintercept = sd(returns$returns, na.rm = T)) +
    theme_light()
}