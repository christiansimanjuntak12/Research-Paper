#install.packages(c("quantmod", "tidyquant", "ggplot2", "dplyr","micss","aTSA"))
library(quantmod)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(micss)
library(aTSA)

#install.packages(c("imputeTS", "zoo"))
library(imputeTS)   # statsNA(), ggplot_na_distribution()
library(zoo)        # core xts/zoo NA helpers



####################daily exchange rate########################################
# Get USD per EUR rate (DEXUSEU)
exchange<-getSymbols("DEXUSEU", src = "FRED",
                     from = "2000-01-01", to ="2020-12-30",auto.assign = FALSE)
head(exchange)
exchange_df <- data.frame(Date = index(exchange), Exchange_Rate = coredata(exchange)[,1])
summary(exchange_df)
ggplot(data = exchange_df, aes(x = Date, y = Exchange_Rate)) +
  geom_line(color = "purple", size=0.6) +
  labs(title = "US Dollar to Euro Exchange Rate",
       x = "Time",
       y = "Exchange Rate (USD/EUR)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-01-03", NA)),
                                                    date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 2, by = 0.2))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )



############### Get DAX index, ^GDAXI is the Yahoo Finance symbol for DAX ######################
dax_index <- getSymbols("^GDAXI",src = "yahoo", from = "2000-01-01", to ="2020-12-30", auto.assign = FALSE)
#the number of missing value
sum(is.na(dax_index$GDAXI.Close))
#linear interpolation
#dax_index_inter <- na.approx(dax_index)
#sum(is.na(dax_index_inter))
#extract the closing price in time index
dax_index <- Cl(dax_index)
head(dax_index)
sum(is.na(dax_index))
#Remove any rows where *any* column is NA and create data frame
dax_index_clean <- na.omit(dax_index)
sum(is.na(dax_index_clean))
dax_index_df <- data.frame(date = index(dax_index_clean),close = coredata(dax_index_clean))
head(dax_index_df)
#daily log return
return_dax <- transform(dax_index_df, log_daily_ret = c(NA, diff(log(GDAXI.Close))))
return_dax <- return_dax[-1, ]
head(return_dax)
tail(return_dax)




#visualisation
f_dax<-ggplot(dax_index_df, aes(x = date, y = GDAXI.Close)) +
  geom_line(color = "red", size = 0.5) +
  labs(title = "DAX",
       x="", y = "Cl (EUR)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-01-03", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(2000, 18000, by = 2000))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

#ICSS structural break
dax_break <- icss(return_dax$log_daily_ret, alpha = 0.05)  # default 95% confidence level
str(dax_break)
#NO structural break

#Compute the global SD (excluding the NA)
sd_ret <- sd(return_dax$log_daily_ret, na.rm = TRUE)

# Plot daily return
b_dax<-ggplot(return_dax, aes(x = date, y = log_daily_ret)) +
  geom_line(color = "steelblue", size = 0.3) +
  # ±3 SD band
  geom_hline(yintercept =  3 * sd_ret, linetype = "dashed", size    = 0.9, color = "firebrick") +
  geom_hline(yintercept = -3 * sd_ret, linetype = "dashed", size    = 0.9, color = "firebrick") +
  labs(
    title = "DAX",
    x     = NULL,
    y     = "Log-Daily Return"
  ) +
  scale_x_date(
    limits = as.Date(c("2000-01-03", NA)),
    expand = c(0, 0), date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )







##################Get Crude Oil####################
crude <- getSymbols("CL=F", src = "yahoo", from = "2000-01-01", to ="2020-12-30", auto.assign = FALSE)
#the number of missing value
sum(is.na(crude$`CL=F.Close`))
#linear interpolation
#crude_inter <- na.approx(crude)
#sum(is.na(crude_inter))
# Extract just the 'Close' column
crude_xts <- Cl(crude)
crude_xts["2020-04-13/2020-04-27"]

#Remove na and specific row
crude_clean <- na.omit(crude_xts)
sum(is.na(crude_clean))
crude_clean["2020-04-13/2020-04-27"]




#Convert to EUR based on daily exchange rate
head(exchange)
head(crude_clean)
crude_exchange<-merge(crude_clean, exchange, join = "inner") #keep Inner‐join, keeps only common dates:
crude_exchange<-na.omit(crude_exchange) #drops any row with at least one NA
crude_eur <- crude_exchange$CL.F.Close/crude_exchange$DEXUSEU #convert dollar to euro
colnames(crude_eur) <- "crude.EUR"
head(crude_eur)
#drop 1 negative value
crude_eur["2020-04-13/2020-04-27"]
crude_eur <- crude_eur[ index(crude_eur) != as.Date("2020-04-20")]
#create data frame
crude_df <- data.frame(date = index(crude_eur),close = coredata(crude_eur))
head(crude_df)
#daily log return
return_crude <- transform(crude_df, log_daily_ret = c(NA, diff(log(crude.EUR))))
return_crude <- return_crude[-1, ]
head(return_crude)
tail(return_crude)




#visualisation
#summary(crude_df$CL.F.Close)

f_crude<-ggplot(crude_df, aes(x = date, y = crude.EUR)) +
  geom_line(color = "black", size = 0.5) +
  labs(title = "Crude Oil",
       x="", y = "Cl (EUR per barrel)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-08-23", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 150, by = 20))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )


#ICSS structural break
crude_break <- icss(return_crude$log_daily_ret, alpha = 0.5)  # default 95% confidence level
str(crude_break)
#No structural break


#Compute the global SD (excluding the NA)
sd_crude <- sd(return_crude$log_daily_ret, na.rm = TRUE)

# Plot daily return
b_crude<-ggplot(return_crude, aes(x = date, y = log_daily_ret)) +
  geom_line(color = "steelblue", size = 0.3) +
  # ±3 SD band
  geom_hline(yintercept =  3 * sd_crude, linetype = "dashed", size    = 0.9, color = "firebrick") +
  geom_hline(yintercept = -3 * sd_crude, linetype = "dashed",size    = 0.9, color = "firebrick") +
  labs(
    title = "Crude Oil",
    x     = NULL,
    y     = "Log-Daily Return"
  ) +
  scale_x_date(
    limits = as.Date(c("2000-08-23", NA)),
    expand = c(0, 0),
    date_breaks = "2 years", date_labels = "%Y")+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )





########## Get Natural gas##################
gas <- getSymbols("NG=F",src = "yahoo", from = "2000-01-01", to ="2020-12-30", auto.assign = FALSE)
#the number of missing value
sum(is.na(gas$`NG=F.Close`))
#linear interpolation
#gas_inter <- na.approx(gas)
#sum(is.na(gas_inter))
gas_xts <- Cl(gas)  # Extract just the 'Close' column
#Remove any rows where *any* column is NA
gas_clean <- na.omit(gas_xts)
sum(is.na(gas_clean))



#Convert to EUR and create data frame
head(exchange)
head(gas_clean)
gas_exchange<-merge(gas_clean, exchange, join = "inner") #keep Inner‐join, keeps only common dates:
head(gas_exchange)
gas_exchange<-na.omit(gas_exchange) #drops any row with at least one NA
gas_eur <- gas_exchange$NG.F.Close/gas_exchange$DEXUSEU #convert dollar to euro
head(gas_eur)
colnames(gas_eur) <- "gas.EUR"
head(gas_eur)
gas_df <- data.frame(date = index(gas_eur),close = coredata(gas_eur))
head(gas_df)
#daily log return
return_gas <- transform(gas_df, log_daily_ret = c(NA, diff(log(gas.EUR))))
return_gas <- return_gas[-1, ]
head(return_gas)
tail(return_gas)


#visualisagas_df#visualisation
summary(gas_df$NG.F.Close)

f_gas<-ggplot(gas_df, aes(x = date, y = gas.EUR)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "Natural Gas",
       x="Time", y = "Cl (EUR per MMBtu)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-08-30", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 20, by = 2))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )





######################################################### Wheat ################################################
wheat <- getSymbols("ZW=F",src = "yahoo", from = "2000-01-01", to ="2020-12-30", auto.assign = FALSE)
#the number of missing value
sum(is.na(wheat$`ZW=F.Close`))
#linear interpolation
#wheat_inter <- na.approx(wheat)
#sum(is.na(wheat_inter))
wheat_xts <- Cl(wheat)  # Extract just the 'Close' column
#Remove any rows where *any* column is NA
wheat_clean <- na.omit(wheat_xts)
sum(is.na(wheat_clean))


#Convert to EUR
head(exchange)
head(wheat_clean)
wheat_exchange<-merge(wheat_clean, exchange, join = "inner") #keep Inner‐join, keeps only common dates:
head(wheat_exchange)
wheat_exchange<-na.omit(wheat_exchange) #drops any row with at least one NA
wheat_eur <- wheat_exchange$ZW.F.Close/gas_exchange$DEXUSEU #convert dollar to euro
head(wheat_eur)
colnames(wheat_eur) <- "wheat.EUR"
head(wheat_eur)
wheat_df <- data.frame(date = index(wheat_eur),close = coredata(wheat_eur))
#daily log return
return_wheat <- transform(wheat_df, log_daily_ret = c(NA, diff(log(wheat.EUR))))
return_wheat <- return_wheat[-1, ]
head(return_wheat)
tail(return_wheat)


#visualisation
head(wheat_df)
summary(wheat_df$wheat.EUR)

f_wheat<-ggplot(wheat_df, aes(x = date, y = wheat.EUR)) +
  geom_line(color = "darkgreen", size = 0.5) +
  labs(title = "Wheat",
       x = "Time", y = "Cl (EUR per bushel)") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2000-07-16", NA)),
               date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1400, by = 200))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12)
  )





# install.packages("cowplot")
library(cowplot)

# arrange into a 2×3 grid (last cell empty)
plot_grid(f_dax, f_crude, f_gas, f_wheat,
          labels = "AUTO",
          ncol = 2, nrow = 2,
          align       = "v",          # align vertically
          axis        = "l",          # align left axes so widths line up
          rel_widths  = c(1, 1),      # both columns same size
          rel_heights = c(1, 1, 1))    # three rows same height



plot_grid(b_dax, b_crude, b_gas, b_wheat,
          labels = "AUTO",
          ncol = 2, nrow = 2,
          align       = "v",          # align vertically
          axis        = "l",          # align left axes so widths line up
          rel_widths  = c(1, 1),      # both columns same size
          rel_heights = c(1, 1, 1))    # three rows same height
