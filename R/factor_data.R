# packages---------------------------------------------------------------------------------------
library(tidyquant)
library(tidymodels)

# get stock data --------------------------------------------------------------------------------

tickers <- c("EQNR", "NHY")

returns <- tickers %>% 
  tq_get() %>% 
  select(symbol, date, close) %>% 
  group_by(symbol) %>% 
  mutate(returns = close - lag(close),
         returns_log = log(close) - log(lag(close)))

# get factor data -------------------------------------------------------------------------------
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


# join data -------------------------------------------------------------------------------------
df <- returns %>%
  left_join(factors, by = "date") %>% 
  na.omit()

# Plot returns ----------------------------------------------------------------------------------
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
plot_returns(returns, ticker = tickers[1], return_var = returns_log)
plot_returns(returns, ticker = tickers[1], return_var = returns)
map(.x = tickers, .f = ~plot_returns(ticker = ., .data = returns, return_var = returns_log))

# Plot vol --------------------------------------------------------------------------------------
returns %>% 
  filter(symbol == "EQNR") %>% 
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
ci = 0.05
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


decile_data %>%
  ggplot(aes(decile, value, color = name)) +
  geom_line(size = 1.1) +
  theme_light()
  






