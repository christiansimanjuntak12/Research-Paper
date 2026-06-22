#install.packages(c("rugarch", "rmgarch"))  # ← run once
library(rugarch)   # univariate GARCH
library(rmgarch)   # multivariate DCC / GO-GARCH / Copula-GARCH



head(aligned_returns)


#Univariate GARCH spec for each column 
u_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std" # Student's t-distribution for heavy tails
)

m_spec <- multispec(replicate(ncol(aligned_returns), u_spec, simplify = FALSE))


# DCC spec, fit, and extraction 
dcc_spec <- dccspec(uspec = m_spec, dccOrder = c(1,1), distribution = "mvt") # Multivariate t-distribution
dcc_fit  <- dccfit(dcc_spec, data = aligned_returns)

# Extract dynamic correlations
Rho <- rcor(dcc_fit)                 # 3-D array: [asset, asset, time]
rho_DAX_Crude <- Rho["DAX", "Crude", ]
rho_DAX_Gas   <- Rho["DAX", "Gas",   ]
rho_DAX_Wheat <- Rho["DAX", "Wheat", ]

# Get time index from aligned_returns (assuming it's a zoo/xts object)
time_index <- index(aligned_returns)

# Set up a 3x1 plotting layout
par(mfrow = c(3, 1), mar = c(4, 2, 2, 1))

# Plot 1: DAX vs Crude
plot(time_index, rho_DAX_Crude, type = "l", col = "blue", 
     main = "Dynamic Correlation: DAX vs Crude", 
     xlab = "Time", ylab = "Correlation (ρ)", 
     ylim = c(-1, 1), lwd = 2)
abline(h = 0, lty = 2, col = "gray")

# Plot 2: DAX vs Gas
plot(time_index, rho_DAX_Gas, type = "l", col = "red", 
     main = "Dynamic Correlation: DAX vs Gas", 
     xlab = "Time", ylab = "Correlation (ρ)", 
     ylim = c(-1, 1), lwd = 2)
abline(h = 0, lty = 2, col = "gray")

# Plot 3: DAX vs Wheat
plot(time_index, rho_DAX_Wheat, type = "l", col = "green", 
     main = "Dynamic Correlation: DAX vs Wheat", 
     xlab = "Time", ylab = "Correlation (ρ)", 
     ylim = c(-1, 1), lwd = 2)
abline(h = 0, lty = 2, col = "gray")

# Reset plotting parameters
par(mfrow = c(1, 1))


#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(zoo) # For handling time series index

# Get time index from aligned_returns
time_index <- index(aligned_returns)

# Create a data frame for plotting
data <- data.frame(
  Time = time_index,
  DAX_Crude = as.numeric(rho_DAX_Crude),
  DAX_Gas = as.numeric(rho_DAX_Gas),
  DAX_Wheat = as.numeric(rho_DAX_Wheat)
)
head(data)
tail(data)

# Define highlight period of Stagnation crisis
highlight_start <- as.Date("2000-08-30")
highlight_end <- as.Date("2002-12-31")

# Define highlight period of Global financial crisis
highlight_start_g <- as.Date("2008-01-01")
highlight_end_g <- as.Date("2009-12-31")

# Define highlight period of European crisis
highlight_start_e <- as.Date("2010-01-01")
highlight_end_e <- as.Date("2012-12-31")

# Define highlight period of pandemic crisis
highlight_start_p <- as.Date("2020-01-01")
highlight_end_p <- as.Date("2020-12-29")


# Plot using ggplot2
dax_crude<-ggplot(data, aes(x = Time, y = DAX_Crude)) +
  geom_line(color = "black", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",size=0.9, color = "firebrick")+
  labs(title = "DAX vs Crude",
       x="", y = "Correlation (ρ)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-08-30", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1))+
  annotate("rect",
           xmin = highlight_start,
           xmax = highlight_end,
           ymin = -Inf,
           ymax = Inf,
           fill = "yellow",
           alpha = 0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

dax_gas<-ggplot(data, aes(x = Time, y = DAX_Gas)) +
  geom_line(color = "blue", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=0.9, color = "firebrick")+
  labs(title = "DAX vs Gas",
       x="", y = "Correlation (ρ)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-08-30", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1))+
  annotate("rect",
           xmin = highlight_start,
           xmax = highlight_end,
           ymin = -Inf,
           ymax = Inf,
           fill = "yellow",
           alpha = 0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

dax_wheat<-ggplot(data, aes(x = Time, y = DAX_Wheat)) +
  geom_line(color = "darkgreen", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size=0.9, color = "firebrick")+
  labs(title = "DAX vs Wheat",
       x="Time", y = "Correlation (ρ)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-08-30", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.2, 0.2, by = 0.1))+
  annotate("rect",
           xmin = highlight_start,
           xmax = highlight_end,
           ymin = -Inf,
           ymax = Inf,
           fill = "yellow",
           alpha = 0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )


plot_grid(dax_crude, dax_gas, dax_wheat,
          labels = "AUTO",
          ncol = 1, nrow = 3,
          align       = "v",          # align vertically
          axis        = "l",          # align left axes so widths line up
          rel_widths  = c(1, 1),      # both columns same size
          rel_heights = c(1, 1, 1))    # three rows same height

