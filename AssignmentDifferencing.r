# Choose a test data type first !!
testDataOn <- "monthly" # 'monthly' ONLY
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
library(uroot)

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
ts_data <- ts(monthly_df$demand, start = c(year(min(monthly_df$month)), month(min(monthly_df$month))), frequency = 12)
frequency <- 12
time_series_label <- "Monthly"


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
# ---------- 5. Data Split ---------------------------
# !---------------------------------------------------!
# ** Data Split **
# Set training data from 2015 to 2019
train_data <- window(ts_data,start=2015,end=c(2019)) # if use monthly mape is 1.6
test_data <- window(ts_data, start = 2019)
test_length <- length(test_data)
print(test_length) # Monthly:20, Daily: 646
nIntoFutures <- 12




# !---------------------------------------------------!
# ---------- 3. Stationary test: ADF test ------------
# !---------------------------------------------------!
# Perform the ADF test
adf_result <- adf.test(train_data, alternative = "stationary")
print(adf_result)

# Extract the p-value
p_value <- adf_result$p.value

# Choose your significance level (alpha)
alpha <- 0.05

# Compare the p-value to the chosen significance level
if (p_value < alpha) {
  print("Reject the null hypothesis: The time series is likely stationary.")
} else {
  print("Fail to reject the null hypothesis: The time series may have a unit root and is non-stationary.")
} # for monthly, not statioanry

# KPSS test for additional confirmation
kpss_result <- kpss.test(train_data)
print(kpss_result)


# Check if the differenced data is now stationary
if (kpss_result$p.value > alpha) {
  print("Before differencing: The time series is now likely stationary (KPSS Test).")
} else {
  print("Before differencing: The time series may still have stationarity issues (KPSS Test).")
} # for monthly, statioanry


# !---------------------------------------------------!
# ---------- 4. Correlogram and Pacf -----------------
# !---------------------------------------------------!
# Autocorrelation: measures the linear relationship between a time series and its lagged versions.
acf_data <- window(train_data, start=2015, end=2020)
ggAcf(acf_data)
ggPacf(acf_data)

# Manual Arima Fitting:
# 0. Manual Arima Fit:
# Plot ACF and PACF to visually check stationarity
acf(train_data, main = "ACF of Differenced Data")
# Other version is
# ggAcf(acf_data)
pacf(train_data, main = "PACF of Differenced Data")
# Non-seasonal: Below 12.
p <- 1 # AR: Autoregressive. 1 non seasonal AR term, include lagged val of series in model.
d <- 0 # Differencing.
q <- 0 # MA: Moving Average
# Seasonal: Multiples of 12.
P <- 0 # SAR
D <- 0 # Seasonal Differencing. Apply 1 season differencing so stationary. Removes seasonality.
Q <- 1 # SMA. include lagged forecast errors from seasonal component in the model.
freq <- 12
manArima_model <- arima(train_data, order=c(p, d, q), seasonal=list(order=c(P, D, Q), period=freq))
manualArima_forecast <- forecast(manArima_model, h=test_length)

plot(manualArima_forecast)
lines(test_data, col="red")
accuracy(manualArima_forecast, test_data)


# Another option: -=========================================================
# Chekck if differencing is Needed.
# Check for the number of differences needed
d <- ndiffs(train_data)
print(paste("Number of differences required:", d)) # 0

D <- nsdiffs(train_data) # For seasonal data
print(paste("Seasonal differences required:", D)) # 1
# Because nsdiffs returns 1, we will test another model with D = 1

diff_data <- diff(train_data, lag=frequency)
#diff_data <- diff(diff_data, lag=frequency) # Monthly data only run this.

# Perform the ADF test again on the differenced data
adf_result_diff <- adf.test(diff_data, alternative = "stationary")
print(adf_result_diff)
p_value_diff <- adf_result_diff$p.value
if (p_value_diff < alpha) {
  print("After differencing: The time series is now likely stationary (ADF Test).")
} else {
  print("After differencing: The time series may still be non-stationary (ADF Test).")
} # still not stationary but very close.
# ADF test is not always able to detect non-stationarity in the presence of strong seasonality.
# THe time series has strong seasonal fcomponent with 12 months period.
# ADF designed to detecht non stationarity, may not be able to detect non-stationarity caused
# by seasonality.

# KPSS test for additional confirmation
# KPS more sensitive to non-stationarity caused by seasonality.
kpss_result_diff <- kpss.test(diff_data)
print(kpss_result_diff)
if (kpss_result_diff$p.value > alpha) {
  print("After differencing: The time series is now likely stationary (KPSS Test).")
} else {
  print("After differencing: The time series may still have stationarity issues (KPSS Test).")
} # Result is 0.1 > 0.05, time series is likely stationary.


D <- 1 # Seasonal Differencing. Apply 1 season differencing so stationary. Removes seasonality.
freq <- 12 
manArima_model_diff1 <- arima(train_data, order=c(p, d, q), seasonal=list(order=c(P, D, Q), period=freq))
manualArimaDiff1_forecast <- forecast(manArima_model_diff1, h=test_length)
# Need to add AICc
plot(manualArimaDiff1_forecast)
lines(test_data, col="red")
accuracy(manualArimaDiff1_forecast, test_data)



# !---------------------------------------------------!
# ---------- 6. Other Model fit ----------------------
# !---------------------------------------------------!
# 1. Fit ETS model using stlf for seasonal data
stl_ets_model <- stlf(train_data)
stl_ets_forecast <- forecast(stl_ets_model, h = test_length)

# 2. Fit ARIMA model
arima_model <- auto.arima(train_data, trace = TRUE, ic = "aicc") # based on the AICc criterion.
arima_forecast <- forecast(arima_model, h = test_length)

# 3. Fit NNAR model (Neural Network Autoregression)
nnar_model <- nnetar(train_data)
nnar_forecast <- forecast(nnar_model, h = test_length)

# 4. Fit TBATS model
tbats_model <- tbats(train_data)
tbats_forecast <- forecast(tbats_model, h = test_length)

# 5. Fit Holt-Winters model
holt_winters_model <- HoltWinters(train_data, seasonal = "additive")
holt_winters_forecast <- forecast(holt_winters_model, h = test_length)

# 6. Fit ETS model
ets_model <- ets(train_data)
ets_forecast <- forecast(ets_model, h = test_length)

# 7. Seasonal Naive
sea_naive_model <- snaive(train_data)
sea_naive_forecast <- forecast(sea_naive_model, h=test_length)


# !---------------------------------------------------!
# ---------- 7. Simple models Forecasts ---------------
# !---------------------------------------------------!
# Simple Models Examples:
autoplot(train_data) +
  autolayer(naive(train_data, h = nIntoFutures), series = "Naïve", PI = FALSE) +
  autolayer(snaive(train_data, h = nIntoFutures, linetype = 3), series = "Seasonal naïve", PI = FALSE) +
  autolayer(rwf(train_data, drift = TRUE, h = nIntoFutures), series = "Drift Method", PI = FALSE) +
  autolayer(test_data, series = "Actual", color = "green", linetype = 1) + # Actual data
  ggtitle("Forecasts for Energy Demand") +
  xlab("Year") + ylab("Demand") +
  guides(color = guide_legend(title = "Forecast"))



# !---------------------------------------------------!
# --- 8. Model Evaluation, RMSE, AICc, ACF Residual ---
# !---------------------------------------------------! 
# Print Values
stl_ets_accuracy <- accuracy(stl_ets_forecast, test_data)
print(stl_ets_accuracy)

nnar_accuracy <- accuracy(nnar_forecast, test_data)
print(nnar_accuracy)

tbats_accuracy <- accuracy(tbats_forecast, test_data)
print(tbats_accuracy)

ets_accuracy <- accuracy(ets_forecast, test_data)
print(ets_accuracy)

manArima_accuracy <- accuracy(manualArima_forecast, test_data)
print(manArima_accuracy)

arima_accuracy <- accuracy(arima_forecast, test_data)
print(arima_accuracy)

holtsWinter_accuracy <- accuracy(holt_winters_forecast, test_data)
print(holtsWinter_accuracy)

sea_naive_accuracy <- accuracy(sea_naive_forecast, test_data)
print(sea_naive_accuracy)


# STL ETS Plot
autoplot(stl_ets_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("STL ETS Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# NNAR Plot
autoplot(nnar_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("NNAR Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# TBATS Plot
autoplot(tbats_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("TBATS Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# ETS Plot
autoplot(ets_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("ETS Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# Manual ARIMA Plot
autoplot(manualArima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Manual ARIMA Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# ARIMA Plot
autoplot(arima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("ARIMA Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# Holt's Winter Plot
autoplot(holt_winters_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Holt's Winter Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +

# Seasonal Naive Plot
autoplot(sea_naive_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Seasonal Naive Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 




# !---------------------------------------------------!
# ---------- 9. Best model summary-- -----------------
# !---------------------------------------------------! 
# !!! 1. Best Model: ARIMA(1,0,0)(0,1,1)[12] with drift
print(arima_accuracy)
summary(arima_model)
Box.test(residuals(arima_model), lag = 12, type = "Ljung-Box")
checkresiduals(arima_model, plot = TRUE)
# Plot
autoplot(arima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("ARIMA Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +
  guides(colour = guide_legend(title = "Legend"))
