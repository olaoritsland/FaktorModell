# packages---------------------------------------------------------------------------------------
library(tidyquant)
library(tidymodels)
library(ggthemes)
theme_set(theme_clean())

# functions -------------------------------------------------------------------------------------
source("scripts/load_functions.R")

# tickers (AMEX, NYSE or NASDAQ)c----------------------------------------------------------------
tickers = c("MS", "JPM")

# get stock data --------------------------------------------------------------------------------
returns <- get_stock_data(tickers)

# get factor data -------------------------------------------------------------------------------
factors <- get_factor_data()

# plot factor data ------------------------------------------------------------------------------
plot_factor_returns_cum(.data = factors)

# plot close, rolling sd and mean----------------------------------------------------------------
map(.x = tickers, .f = ~plot_roll(ticker = ., .data = returns))

# join data -------------------------------------------------------------------------------------
df <- returns %>%
  left_join(factors, by = "date") %>% 
  na.omit()

# Plot distribution of returns ------------------------------------------------------------------
map(.x = tickers, .f = ~plot_returns(ticker = ., .data = returns, return_var = returns_log))
map(.x = tickers, .f = ~plot_returns(ticker = ., .data = returns, return_var = returns))

# Plot vol --------------------------------------------------------------------------------------
#plot_vol(returns, ticker = tickers[1])
map(.x = tickers, .f = ~plot_vol(ticker = ., .data = returns))


# Model -----------------------------------------------------------------------------------------

# Select stock
df_filtered <- df %>% 
  filter(symbol == tickers[1])

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
  pdp::partial(pred.var = "mkt", train = df_train) %>%
  autoplot()

# Plot predictions vs. truth
plot_pred_truth_dist(.data = prediction)

# Create decile plot
decile_plot(model = model, test_data_prepped = df_test, test_data_raw = df_test_raw, response_var = returns_log, ci = 0.05)
  





