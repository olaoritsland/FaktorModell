plot_risk_return <- function(index, sharpe = FALSE, sortino = FALSE, MAR = 0.0008) {
  
  tickers <- 
    tq_index(x = index, use_fallback = TRUE) %>% 
    pull(symbol)
  
  returns <- get_stock_data(tickers) %>% 
    group_by(symbol) %>% 
    summarise(mean_log_returns = mean(returns_log, na.rm = TRUE),
              sd_log_returns = sd(returns_log, na.rm = TRUE),
              sharpe = mean_log_returns / sd_log_returns,
              sortino = mean(returns - MAR, na.rm = TRUE)/
                sqrt(sum(pmin(returns - MAR, 0, na.rm = TRUE)^2)/
                       nrow(.)))

    
  if (sharpe) {
    
    lab <- index
    back_col <- '#2C3E50'
    font_col <- '#FFFFFF'
    line_col <- "#FFFFFF"
    grid_col <- 'rgb(255, 255, 255)'
    col_brew_pal <- 'BrBG'
    
    # Plotly
    plot_ly(data   = returns,
            type   = "scatter",
            mode   = "markers",
            x      = ~ sd_log_returns,
            y      = ~ mean_log_returns,
            color  = ~ sharpe,
            colors = col_brew_pal,
            size   = ~ sharpe,
            text   = ~ str_c("<em>", symbol, "</em><br>",
                             "Reward to Risk: ", round(sharpe, 1)),
            marker = list(opacity = 0.8,
                          symbol = 'circle',
                          sizemode = 'diameter',
                          sizeref = 4.0,
                          line = list(width = 2, color = line_col))
    ) %>%
      layout(title   = str_c(lab, 'Analysis: Stock Risk vs Reward', sep = " "),
             xaxis   = list(title = 'Risk: StDev of Daily Log Returns (SDDLR)',
                            gridcolor = grid_col,
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
             yaxis   = list(title = 'Reward: Mean Daily Log Returns (MDLR)',
                            gridcolor = grid_col,
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
             margin = list(l = 100,
                           t = 100,
                           b = 100),
             font   = list(color = font_col),
             paper_bgcolor = back_col,
             plot_bgcolor = back_col)
  }

if (sortino) {
  
  lab <- index
  back_col <- '#2C3E50'
  font_col <- '#FFFFFF'
  line_col <- "#FFFFFF"
  grid_col <- 'rgb(255, 255, 255)'
  col_brew_pal <- 'BrBG'
  
  # Plotly
  plot_ly(data   = returns,
          type   = "scatter",
          mode   = "markers",
          x      = ~ sd_log_returns,
          y      = ~ mean_log_returns,
          color  = ~ sortino,
          colors = col_brew_pal,
          size   = ~ sortino,
          text   = ~ str_c("<em>", symbol, "</em><br>",
                           "Reward to Risk: ", round(sortino)),
          marker = list(opacity = 0.8,
                        symbol = 'circle',
                        sizemode = 'diameter',
                        sizeref = 4.0,
                        line = list(width = 2, color = line_col))
  ) %>%
    layout(title   = str_c(lab, 'Analysis: Stock Risk vs Reward', sep = " "),
           xaxis   = list(title = 'Risk: StDev of Daily Log Returns (SDDLR)',
                          gridcolor = grid_col,
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
           yaxis   = list(title = 'Reward: Mean Daily Log Returns (MDLR)',
                          gridcolor = grid_col,
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font   = list(color = font_col),
           paper_bgcolor = back_col,
           plot_bgcolor = back_col)
}
}
  