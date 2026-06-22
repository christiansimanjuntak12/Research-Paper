
################################################### Gas ####################################################
# 1) Prepare your data
df_gas <- return_gas %>%
  filter(!is.na(log_daily_ret))
head(df_gas)

# 2) Detect breaks
breaks_gas     <- icss(df_gas$log_daily_ret, alpha = 0.05)$tb
break_dates_gas<- df_gas$date[breaks_gas]

# 3) Build regime intervals
regimes_gas <- tibble(
  start_gas = c(min(df_gas$date),  break_dates_gas),
  end_gas   = c(break_dates_gas,   max(df_gas$date))
)

# 4) Compute 3*SD per regime
regime_stats_gas <- regimes_gas %>%
  rowwise() %>%
  mutate(
    sd3_gas   = 3 * sd(df_gas$log_daily_ret[df_gas$date >= start_gas & df_gas$date < end_gas], na.rm = TRUE),
    upper_gas =  sd3_gas,
    lower_gas = -sd3_gas
  ) %>%
  ungroup()

# 5) Plot
b_gas<-ggplot(df_gas, aes(x = date, y = log_daily_ret)) +
  # return series
  geom_line(color = "steelblue", size = 0.3) +
  
  # horizontal ±3 SD in each regime
  geom_segment(
    data    = regime_stats_gas,
    aes(x      = start_gas, xend = end_gas,
        y      = upper_gas, yend = upper_gas),
    color   = "firebrick",
    linetype= "dashed",
    size    = 0.9,
    lineend = "butt"
  ) +
  geom_segment(
    data    = regime_stats_gas,
    aes(x      = start_gas, xend = end_gas,
        y      = lower_gas, yend = lower_gas),
    color   = "firebrick",
    linetype= "dashed",
    size    = 0.9,
    lineend = "butt"
  ) +
  scale_x_date(
    limits = c(min(df_gas$date), max(df_gas$date)),
    expand = c(0, 0),
    date_breaks = "2 years", date_labels = "%Y")+
  labs(
    title = "Gas",
    x     = NULL,
    y     = "Log-Daily Return"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

