# packages---------------------------------------------------------------------------------------
library(tidyquant)
library(tidymodels)

# tickers ---------------------------------------------------------------------------------------
tickers = "NHY"

# get stock data --------------------------------------------------------------------------------
returns <- get_stock_data(tickers)

# get factor data -------------------------------------------------------------------------------
factors <- get_factor_data()


# join data -------------------------------------------------------------------------------------
df <- returns %>%
  left_join(factors, by = "date") %>% 
  na.omit()

# Plot returns ----------------------------------------------------------------------------------
# plot_returns <- function(.data, ticker, return_var) {
  
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
# plot_returns(returns, ticker = tickers[1], return_var = returns_log)
# plot_returns(returns, ticker = tickers[1], return_var = returns)
map(.x = tickers, .f = ~plot_returns(ticker = ., .data = returns, return_var = returns_log))
map(.x = tickers, .f = ~plot_returns(ticker = ., .data = returns, return_var = returns))

# Plot vol --------------------------------------------------------------------------------------
returns %>% 
  filter(symbol == tickers) %>% 
  ggplot(aes(date, returns)) +
  geom_bar(stat = 'identity') +
  #facet_grid(symbol ~ ., scales = "free")
  theme_light()


# Model -----------------------------------------------------------------------------------------

# Select stock
df_filtered <- df %>% 
  filter(symbol == "EQNR")

# Split into train and test
df_split     <- initial_split(df_filtered)
df_train_raw <- training(df_split)
df_test_raw  <- testing(df_split)

# Create recipe
recipe <- 
  recipe(returns_log ~ mkt + smb + hml + rmw + cma, data = df_train_raw) %>% 
  prep()

# Bake
df_train <- bake(recipe, df_train_raw)
df_test <- bake(recipe, df_test_raw)

# Train model
model <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    returns_log ~ 
      mkt
    + smb
    + hml
    + rmw
    + cma,
    data = df_train
  )

# Test model
summary(model$fit)

prediction <- predict(model, df_test) %>%
  bind_cols(df_test_raw) %>%
  rename(estimate     = .pred,
         truth        = returns_log) %>%
  mutate(
    dev = truth - estimate,
    abs_dev = abs(truth - estimate),
    abs_dev_perc = abs_dev / truth
  )

# Evaluate model
multi_metric <- yardstick::metric_set(mape, rmse, mae, rsq)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)

# Pdp plot
model$fit %>%
  pdp::partial(pred.var = "cma", train = df_train) %>%
  autoplot() +
  theme_light()

# Plot predictions vs. truth
prediction %>% 
  select(truth, estimate, date) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .3) +
  theme_light()

# Create decile plot
decile_plot(model = model, test_data_prepped = df_test, test_data_raw = df_test_raw, response_var = returns_log, ci = 0.05)
  






