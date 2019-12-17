plot_vol <- function(.data, ticker, theme = theme_bw()) {

.data %>% 
  filter(symbol == {{ticker}}) %>% 
  ggplot(aes(date, returns)) +
  geom_bar(stat = 'identity') +
  #facet_grid(symbol ~ ., scales = "free")
  theme
  
}
