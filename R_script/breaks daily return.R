
################################################### wheat ####################################################
# 1) Prepare your data
df_wheat <- return_wheat %>%
  filter(!is.na(log_daily_ret))
head(df_wheat)

# 2) Detect breaks
breaks_wheat     <- icss(df_wheat$log_daily_ret, alpha = 0.05)$tb
break_dates_wheat<- df_wheat$date[breaks_wheat]

# 3) Build regime intervals
regimes_wheat <- tibble(
  start_wheat = c(min(df_wheat$date),  break_dates_wheat),
  end_wheat   = c(break_dates_wheat,   max(df_wheat$date))
)

# 4) Compute 3*SD per regime
regime_stats_wheat <- regimes_wheat %>%
  rowwise() %>%
  mutate(
    sd3_wheat   = 3 * sd(df_wheat$log_daily_ret[df_wheat$date >= start_wheat & df_wheat$date < end_wheat], na.rm = TRUE),
    upper_wheat =  sd3_wheat,
    lower_wheat = -sd3_wheat
  ) %>%
  ungroup()

# 5) Plot
b_wheat<-ggplot(df_wheat, aes(x = date, y = log_daily_ret)) +
  # return series
  geom_line(color = "steelblue", size = 0.3) +
  
  # horizontal ±3 SD in each regime
  geom_segment(
    data    = regime_stats_wheat,
    aes(x      = start_wheat, xend = end_wheat,
        y      = upper_wheat, yend = upper_wheat),
    color   = "firebrick",
    linetype= "dashed",
    size    = 0.9,
    lineend = "butt"
  ) +
  geom_segment(
    data    = regime_stats_wheat,
    aes(x      = start_wheat, xend = end_wheat,
        y      = lower_wheat, yend = lower_wheat),
    color   = "firebrick",
    linetype= "dashed",
    size    = 0.9,
    lineend = "butt"
  ) +
  scale_x_date(
    limits = c(min(df_wheat$date), max(df_wheat$date)),
    expand = c(0, 0),
    date_breaks = "2 years", date_labels = "%Y")+
  labs(
    title = "Wheat",
    x     = NULL,
    y     = "Log-Daily Return"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )
