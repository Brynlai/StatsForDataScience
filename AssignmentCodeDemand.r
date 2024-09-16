# Choose a test data type first !!
testDataOn <- "daily" # 'monthly' or "daily"
# Choose a test data type first !!


# Steps:
# !---------------------------------------------------!
# ---------- 0. Import Dataset, filter etc -----------
# !---------------------------------------------------!
# Load necessary packages
library(readr)
library(dplyr)
library(ggplot2)
library(forecast) # Load the forecast package
library(lubridate) # For date manipulation
library(tseries) # For the adf.test function
library()

# Load data from CSV file
data <- read_csv("C:/Users/wbrya/OneDrive/Documents/australia_energy_complete_dataset.csv")

# Drop columns containing "rrp"
noRrp <- data %>%
  select(-contains("rrp"))

# Select date and demand columns
data <- data %>%
  select(matches("date|demand"))

# Ensure date column is in date format
data$date <- as.Date(data$date)

# Create a data frame with dates and demand for the daily time series
daily_df <- data.frame(date = data$date, demand = data$demand)

# Create monthly aggregated dataset
monthly_df <- data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(demand = sum(demand)) # SUM or MEAN

# Exclude the last month if necessary
monthly_df <- monthly_df %>% 
  filter(month < max(month) - months(1))

# Print the head of the monthly and daily dataset
print(head(daily_df))
print(head(monthly_df))
print(tail(daily_df))
print(tail(monthly_df))







# !---------------------------------------------------!
# ---------- 1. Time Series plot ---------------------
# !---------------------------------------------------!

# Plot the time series
ggplot(daily_df, aes(x = date, y = demand)) +
  geom_line() +
  ggtitle("Daily Demand Over Time") +
  labs(x = "Date", y = "Energy Demand")

# Plot the monthly aggregated time series
ggplot(monthly_df, aes(x = month, y = demand)) +
  geom_line() +
  ggtitle("Monthly Demand Over Time") +
  labs(x = "Date", y = "Energy Demand")







# !---------------------------------------------------!
# ---------- 2. Convert to time series object --------
# !---------------------------------------------------!
if (testDataOn == "daily") {
  ts_data <- ts(daily_df$demand, start = c(year(min(daily_df$date)), 1), frequency = 365)
  frequency <- 365
  time_series_label <- "Daily"
} else if (testDataOn == "monthly") {
  ts_data <- ts(monthly_df$demand, start = c(year(min(monthly_df$month)), month(min(monthly_df$month))), frequency = 12)
  frequency <- 12
  time_series_label <- "Monthly"
} else {
  stop("Invalid testDataOn. Please use 'daily' or 'monthly'.")
}


# Decompose the time series
components_dfts <- decompose(ts_data)

# Plot the decomposed components
plot(components_dfts)

# Seasonal Plots:
ggseasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Energy Demand") +
  ggtitle("Seasonal plot: Australia Energy Demand by Month")

# Plot the Polar seasonal plot using ggseasonplot
ggseasonplot(ts_data, polar = TRUE) +
  ylab("Energy Demand") +
  ggtitle("Polar seasonal plot: Australia Energy Demand by Month")

# Plot the seasonal subseries plot using ggsubseriesplot
ggsubseriesplot(ts_data) +
  ylab("Energy Demand") +
  ggtitle("Seasonal subseries plot: Australia Energy Demand by Month")







# !---------------------------------------------------!
# ---------- 3. Stationary test: ADF test ------------
# !---------------------------------------------------!
# Perform the ADF test
adf_result <- adf.test(ts_data, alternative = "stationary")
print(adf_result)


#kpss_result <- kpss.text(ts_data)
#kpss_pval <- kpss_result$p.value
#print(kpss_pval)

# Extract the p-value
p_value <- adf_result$p.value

# Choose your significance level (alpha)
alpha <- 0.05

# Compare the p-value to the chosen significance level
if (p_value < alpha) {
  print("Reject the null hypothesis: The time series is likely stationary.")
} else {
  print("Fail to reject the null hypothesis: The time series may have a unit root and is non-stationary.")
}







# !---------------------------------------------------!
# ---------- 4. Correlogram and Pacf -----------------
# !---------------------------------------------------!
# Autocorrelation: measures the linear relationship between a time series and its lagged versions.
acf_data <- window(ts_data, start=2015)
ggAcf(acf_data)
ggPacf(acf_data)





# !!!!!
# Do AR and MA
# 




# !---------------------------------------------------!
# ---------- 5. Data Split ---------------------------
# !---------------------------------------------------!
# ** Data Split **
# Set training data from 2015 to 2019
train_data <- window(ts_data,start=2015,end=c(2019)) # if use monthly mape is 1.6
test_data <- window(ts_data, start = 2019)
test_length <- length(test_data)
print(test_length) # Monthly:20, Daily: 646

if (testDataOn == "daily") {
  nIntoFutures <- 365
} else if (testDataOn == "monthly") {
  nIntoFutures <- 12
}







# !---------------------------------------------------!
# ---------- 6. Model fit ----------------------------
# !---------------------------------------------------!
# 1. Fit ETS model using stlf for seasonal data
stl_ets_model <- stlf(train_data)
stl_ets_forecast <- forecast(stl_ets_model, h = test_length)

# 2. Fit ARIMA model
arima_model <- auto.arima(train_data)#, ic = "aicc") # based on the AICc criterion.
arima_forecast <- forecast(arima_model, h = test_length)

summary(arima_model)
library(lmtest)
coeftest(arima_model)

# 3. Fit NNAR model (Neural Network Autoregression)
nnar_model <- nnetar(train_data)
nnar_forecast <- forecast(nnar_model, h = test_length)

# 4. Fit TBATS model
tbats_model <- tbats(train_data)
tbats_forecast <- forecast(tbats_model, h = test_length)

# 5. Fit Holt-Winters model
holt_winters_model <- HoltWinters(train_data, seasonal = "additive")
holt_winters_forecast <- forecast(holt_winters_model, h = test_length)

if (testDataOn == "monthly") {
# 6. Fit ETS model if monthly
  ets_model <- ets(train_data)
  ets_forecast <- forecast(ets_model, h = test_length)
}






 
# !---------------------------------------------------!
# ---------- 7. Forecast with forecast() -------------
# !---------------------------------------------------!
# Simple Models Examples:
autoplot(train_data) +
  # Add layers
  autolayer(naive(train_data, h = nIntoFutures), series = "Naïve", PI = FALSE) +
  autolayer(snaive(train_data, h = nIntoFutures, linetype = 3), series = "Seasonal naïve", PI = FALSE) +
  autolayer(rwf(train_data, drift = FALSE, h = nIntoFutures), series = "Drift Method", PI = FALSE) +
  autolayer(test_data, series = "Actual", color = "pink", linetype = 1) + # Actual data
  ggtitle("Forecasts for Energy Demand") +
  xlab("Year") + ylab("Demand") +
  guides(color = guide_legend(title = "Forecast"))








# !---------------------------------------------------!
# ---------- 8. Model Evaluation, RMSE, AICc, ACF Residual ----------
# !---------------------------------------------------! 
# Use summary(model instead)!!!!! 
# Calculate MAPE for each model
if (testDataOn == "monthly") {
  mape_ets <- mean(abs((test_data - ets_forecast$mean) / test_data)) * 100
}
mape_stl_ets <- mean(abs((test_data - stl_ets_forecast$mean) / test_data)) * 100
mape_arima <- mean(abs((test_data - arima_forecast$mean) / test_data)) * 100
mape_nnar <- mean(abs((test_data - nnar_forecast$mean) / test_data)) * 100
mape_tbats <- mean(abs((test_data - tbats_forecast$mean) / test_data)) * 100
mape_holt_winters <- mean(abs((test_data - holt_winters_forecast$mean) / test_data)) * 100

# Print MAPE values
if (testDataOn == "monthly") {
  print(paste("MAPE ETS:", mape_ets))
}
print(paste("MAPE STL-ETS:", mape_stl_ets))
print(paste("MAPE NNAR:", mape_nnar))
print(paste("MAPE TBATS:", mape_tbats))
print(paste("MAPE ARIMA:", mape_arima))
print(paste("MAPE Holt-Winters:", mape_holt_winters))


# Best Model: ARIMA(1,0,0)(0,1,1)[12] with drift
print(paste("MAPE ARIMA:", mape_arima))
print(arima_model)
Box.test(residuals(arima_model), lag = 12, type = "Ljung-Box")

# 2nd Best: Holts-Winters
print(paste("MAPE Holt-Winters:", mape_holt_winters))
# Extract the residuals
residuals_hw <- residuals(holt_winters_model)
# Plot the ACF of the residuals
acf(residuals_hw, lag.max = 12, na.action = na.pass)
# Perform Ljung-Box test for autocorrelation
Box.test(residuals_hw, lag = 12, type = "Ljung-Box")
  # P-Value > 0.05 suggest model has captured underlying patterns well.
  # Residuals do not exhibit significant autocorrelation.
  # Autocorrelation: how correlated t is related to t-1 








# !---------------------------------------------------!
# ---------- 9. Plot model forecasts ------------------
# !---------------------------------------------------! 
# Plot the forecasts
if (testDataOn == "monthly") {
  autoplot(train_data) +
    #autolayer(ets_forecast, series = "ETS") +
    #autolayer(stl_ets_forecast, series = "STL-ETS") +
    autolayer(arima_forecast, series = "ARIMA") +
    #autolayer(nnar_forecast, series = "NNAR") +
    #autolayer(tbats_forecast, series = "TBATS") +
    autolayer(holt_winters_forecast, series = "Holt-Winters") +
    autolayer(test_data, series = "Actual") +
    ggtitle(paste("Forecasts for", time_series_label, "Energy Demand")) +
    xlab("Year") + ylab("Demand") +
    guides(colour = guide_legend(title = "Forecast")) +
    theme(legend.position = "bottom")
} else {
  autoplot(train_data) +
    autolayer(stl_ets_forecast, series = "STL-ETS") +
    autolayer(arima_forecast, series = "ARIMA") +
    autolayer(nnar_forecast, series = "NNAR") +
    autolayer(tbats_forecast, series = "TBATS") +
    autolayer(holt_winters_forecast, series = "Holt-Winters") +
    autolayer(test_data, series = "Actual") +
    ggtitle(paste("Forecasts for", time_series_label, "Energy Demand")) +
    xlab("Year") + ylab("Demand") +
    guides(colour = guide_legend(title = "Forecast")) +
    theme(legend.position = "bottom")
}
