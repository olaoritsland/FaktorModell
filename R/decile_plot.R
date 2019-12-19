
decile_plot <- function(model, test_data_prepped, test_data_raw, response_var, ci) {
  
  # predict and create df
  prediction <- predict(model, test_data_prepped) %>%
    bind_cols(test_data_raw) %>%
    rename(estimate     = .pred,
           truth        = {{response_var}}) %>%
    mutate(
      dev = truth - estimate,
      abs_dev = abs(truth - estimate),
      abs_dev_perc = abs_dev / truth
    )
  
  # Create measures and structure date
  decile_data <- prediction %>% 
    mutate(decile = ntile(estimate, 10)) %>%
    group_by(decile, add = TRUE) %>%
    summarise(mean_estimate = mean(estimate),
              mean_truth = mean(truth),
              sd_estimate = sd(estimate),
              n_estimate = n()) %>%
    mutate(se_estimate = sd_estimate / sqrt(n_estimate),
           lower.ci_estimate = mean_estimate - qt(1 - (ci / 2), n_estimate - 1) * se_estimate,
           upper.ci_estimate = mean_estimate + qt(1 - (ci / 2), n_estimate - 1) * se_estimate) %>%
    select(-sd_estimate,-se_estimate) %>% 
    pivot_longer(-c(decile, n_estimate))
  
  # Plot data
  decile_data %>%
    ggplot(aes(decile, value, color = name)) +
    geom_line(size = 1)
}
