#####################################DCC GARCH model###################################################

head(return_dax)
head(return_crude)
head(return_gas)
head(return_wheat)


# Convert to xts
dax_xts <- xts(return_dax$log_daily_ret, order.by = as.Date(return_dax$date))
crude_xts <- xts(return_crude$log_daily_ret, order.by = as.Date(return_crude$date))
gas_xts <- xts(return_gas$log_daily_ret, order.by = as.Date(return_gas$date))
wheat_xts <- xts(return_wheat$log_daily_ret, order.by = as.Date(return_wheat$date))

# Assign column names
colnames(dax_xts) <- "DAX"
colnames(crude_xts) <- "Crude"
colnames(gas_xts) <- "Gas"
colnames(wheat_xts) <- "Wheat"


# Merge all series, aligned them, keeping only common dates
aligned_returns <- merge(dax_xts, crude_xts, gas_xts, wheat_xts)
head(aligned_returns)

# Remove any rows with NA values
aligned_returns <- na.omit(aligned_returns)
nrow(aligned_returns)
head(aligned_returns)


# Check date range
range(index(aligned_returns))

# Check for missing values
sum(is.na(aligned_returns)) # Should be 0

# Plot to inspect
plot(aligned_returns, main = "Aligned Log Returns", multi.panel = TRUE)

# Calculate mean for each series
means <- colMeans(aligned_returns, na.rm = TRUE)
print(format(means, scientific = FALSE))


# Calculate variance for each series
variances <- apply(aligned_returns, 2, var, na.rm = TRUE)
print(format(variances, scientific = FALSE))



library(moments)
#convert xts to numeric
returns_vector_dax <- as.numeric(aligned_returns[, 1])
returns_vector_crude <- as.numeric(aligned_returns[, 2])
returns_vector_gas <- as.numeric(aligned_returns[, 3])
returns_vector_wheat <- as.numeric(aligned_returns[, 4])
head(returns_vector_dax)
head(returns_vector_crude)
head(returns_vector_gas)
head(returns_vector_wheat)



# Compute skewness
skewness_test <- skewness(aligned_returns, na.rm = TRUE)
kurtosis_test <- kurtosis(aligned_returns, na.rm = TRUE)

skewness_agostino_dax<-agostino.test(returns_vector_dax)
skewness_agostino_crude<-agostino.test(returns_vector_crude)
skewness_agostino_gas<-agostino.test(returns_vector_gas)
skewness_agostino_wheat<-agostino.test(returns_vector_wheat)

#Extract p-value
p_value_dax<- skewness_agostino_dax$p.value
p_value_crude<- skewness_agostino_crude$p.value
p_value_gas<- skewness_agostino_gas$p.value
p_value_wheat<- skewness_agostino_wheat$p.value


# Print results
# Print results for each dataset at 1% significance level
cat("D'Agostino Skewness Test Results at alpha = 0.01\n")
cat("---------------------------------------------\n")

cat("DAX:\n")
print(skewness_agostino_dax)
cat("P-value:", p_value_dax, "\n")
cat("Significant at 1% (alpha = 0.01):", p_value_dax < 0.01, "\n\n")

cat("Crude:\n")
print(skewness_agostino_crude)
cat("P-value:", p_value_crude, "\n")
cat("Significant at 1% (alpha = 0.01):", p_value_crude < 0.01, "\n\n")

cat("Gas:\n")
print(skewness_agostino_gas)
cat("P-value:", p_value_gas, "\n")
cat("Significant at 1% (alpha = 0.01):", p_value_gas < 0.01, "\n\n")

cat("Wheat:\n")
print(skewness_agostino_wheat)
cat("P-value:", p_value_wheat, "\n")
cat("Significant at 1% (alpha = 0.01):", p_value_wheat < 0.01, "\n")




#Compute Kurtosis
kurt_test_dax <- anscombe.test(returns_vector_dax)
kurt_test_crude <- anscombe.test(returns_vector_crude)
kurt_test_gas <- anscombe.test(returns_vector_gas)
kurt_test_wheat <- anscombe.test(returns_vector_wheat)

print(kurt_test_dax)
print(kurt_test_crude)
print(kurt_test_gas)
print(kurt_test_wheat)


#min and max
# Print min and max for each variable
cat("DAX Min:", min(returns_vector_dax, na.rm = TRUE), "Max:", max(returns_vector_dax, na.rm = TRUE), "\n")
cat("Crude Min:", min(returns_vector_crude, na.rm = TRUE), "Max:", max(returns_vector_crude, na.rm = TRUE), "\n")
cat("Gas Min:", min(returns_vector_gas, na.rm = TRUE), "Max:", max(returns_vector_gas, na.rm = TRUE), "\n")
cat("Wheat Min:", min(returns_vector_wheat, na.rm = TRUE), "Max:", max(returns_vector_wheat, na.rm = TRUE), "\n")


#Stationary test
#install.packages("tseries")  # Only once
#install.packages("rugarch")
#install.packages("FinTS")
library(tseries)
library(rugarch)
library(FinTS)


#############################Autocorrelation test and heteroskedasticity test###############################
# Function to fit ARCH(1) model and perform diagnostic tests
perform_arch_diagnostics <- function(returns, series_name, lags_lb = 30, lags_arch = 30) {
  # Define ARCH(1) model specification
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # ARCH(1) = GARCH(1,0)
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),  # Constant mean
    distribution.model = "norm"  # Normal distribution
  )
  
  # Fit the model
  fit <- ugarchfit(spec = spec, data = returns, solver = "hybrid")
  
  # Extract standardized residuals
  std_residuals <- residuals(fit, standardize = TRUE)
  
  # Ljung-Box Test on standardized residuals
  lb_test <- Box.test(std_residuals, lag = lags_lb, type = "Ljung-Box")
  cat("\nLjung-Box Test on Standardized Residuals for", series_name, ":\n")
  print(lb_test)
  
  # ARCH-LM Test on squared standardized residuals
  arch_test <- ArchTest(std_residuals^2, lags = lags_arch)
  cat("\nARCH-LM Test on Squared Standardized Residuals for", series_name, ":\n")
  print(arch_test)
  
  # Return fit for further inspection if needed
  return(fit)
}


# Perform diagnostics for each series
fit_dax <- perform_arch_diagnostics(returns_vector_dax, "DAX")
fit_crude <- perform_arch_diagnostics(returns_vector_crude, "Crude Oil")
fit_gas <- perform_arch_diagnostics(returns_vector_gas, "Gas")
fit_wheat <- perform_arch_diagnostics(returns_vector_wheat, "Wheat")






# Stationarity tests for each series
# install once (if needed)
#install.packages("tseries")
library(tseries)

adf.test(aligned_returns$DAX)
adf.test(aligned_returns$Crude)
adf.test(aligned_returns$Gas)
adf.test(aligned_returns$Wheat)


kpss.test(aligned_returns$DAX)
kpss.test(aligned_returns$Crude)
kpss.test(aligned_returns$Gas)
kpss.test(aligned_returns$Wheat)

