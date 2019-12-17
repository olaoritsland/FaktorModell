plot_pred_truth_dist <- function(.data = prediction, theme = theme_bw()) {
  
  .data %>% 
    select(truth, estimate, date) %>% 
    pivot_longer(-date) %>% 
    ggplot(aes(value, fill = name)) +
    geom_density(alpha = .3) +
    theme
  
}
