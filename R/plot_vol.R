plot_vol <- function(.data, ticker) {

.data %>% 
  filter(symbol == {{ticker}}) %>% 
  ggplot(aes(date, returns)) +
  geom_bar(stat = 'identity')
  #facet_grid(symbol ~ ., scales = "free")
  
}
