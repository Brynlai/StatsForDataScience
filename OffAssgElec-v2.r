# Steps: (Using MONTHLY DATA for models)
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

# Check for missing values in the entire data frame
print("Total missing values in the data frame:")
print(sum(is.na(data)))

# Check for missing values by column
print("Missing values by column:")
print(colSums(is.na(data)))
# Missing values in columns rainfall and solar_exposure, we will not be using these anyways.

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
# ---------- 3. Data Split ---------------------------
# !---------------------------------------------------!
# ** Data Split **
# Set training data from 2015 to 2019
train_data <- window(ts_data,start=2015,end=c(2019)) # if use monthly mape is 1.6
test_data <- window(ts_data, start = 2019)
test_length <- length(test_data)
print(test_length) # Monthly:20, Daily: 646
nIntoFutures <- 12







# !---------------------------------------------------!
# ---------- 4. Stationary test: ADF test ------------
# !----------# ADF and KPSS Only for non seasonal
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
} # for monthly, not stationary

# KPSS test for additional confirmation
kpss_result <- kpss.test(train_data)
print(kpss_result)

# Double Check:
# Check if the differenced data is now stationary
if (kpss_result$p.value > alpha) {
  print("Before differencing: The time series is now likely stationary (KPSS Test).")
} else {
  print("Before differencing: The time series may still have stationarity issues (KPSS Test).")
} # for monthly, stationary







# !---------------------------------------------------!
# ---------- 5. Correlogram and Pacf -----------------
# !---------------------------------------------------!
# Autocorrelation: measures the linear relationship between a time series and its lagged versions.

ggAcf(train_data, lag.max = 24)
ggPacf(train_data, lag.max = 24)

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

train_data %>% diff() %>% ggtsdisplay(main="")







# !---------------------------------------------------!
# ---------- 6. Manual Arima Fitting -----------------
# !---------------------------------------------------!
# Plot ACF and PACF to visually check stationarity
#acf(train_data, main = "ACF of Training Data")
#pacf(train_data, main = "PACF of Training Data")
ggAcf(train_data, lag.max = 36)
ggPacf(train_data, lag.max = 36)

# DK if need, Test if seasonal Pattern is stable
ch.test(train_data)

d <- ndiffs(train_data)
print(paste("Number of differences required:", d)) # 0
D <- nsdiffs(train_data) # For seasonal data
print(paste("Seasonal differences required:", D)) # 1

# Based on ...

# Non-seasonal: Below 12.
p <- 1 # AR: Autoregressive. 1 non seasonal AR term, include lagged val of series in model.

d <- 1 
# Despite ADF and KPSS results, 1 to capture pattern more effectively.
# When d = 1, shows smaller difference between train and test sets,


q <- 0 # MA: Moving Average
# Seasonal: Multiples of 12.
P <- 0 # SAR
D <- 1 # Clearly seasonal data. Seasonal Differencing. Apply 1 season differencing so stationary. Removes seasonality.
Q <- 1 # SMA. include lagged forecast errors from seasonal component in the model.
freq <- 12
manArima_model <- arima(train_data, order=c(p, d, q), seasonal=list(order=c(P, D, Q), period=freq))
manualArima_forecast <- forecast(manArima_model, h=test_length)


evaluate_arima_model <- function(manArima_model, manArima_forecast, test_data) {
  plot(manualArima_forecast)
  lines(test_data, col="red")
  accuracy(manualArima_forecast, test_data)
  checkresiduals(manArima_model, plot = TRUE)
  #AIC(manArima_model)
  
  npar <- length(manArima_model$coef) + 1
  nstar <- length(manArima_model$residuals) - manArima_model$arma[6] - manArima_model$arma[7] * manArima_model$arma[5]
  
  bic <- manArima_model$aic + npar * (log(nstar) - 2)
  aicc <- manArima_model$aic + 2 * npar * (nstar/(nstar - npar - 1) - 1)
  print("AICc value:")
  print(aicc)
}
evaluate_arima_model(manArima_model, manArima_forecast, test_data)

#d <- 1
#manArimad1_model <- arima(train_data, order=c(p, d, q), seasonal=list(order=c(P, D, Q), period=freq))
#manualArimad1_forecast <- forecast(manArima_model, h=test_length)
#evaluate_arima_model(manArimad1_model, manualArimad1_forecast, test_data)




# !---------------------------------------------------!
# ---------- 7. Other Model fit ----------------------
# !---------------------------------------------------!
# 1. Seasonal Naive
sea_naive_model <- snaive(train_data)
sea_naive_forecast <- forecast(sea_naive_model, h=test_length)

# 2. Fit ETS model using stlf for seasonal data
stl_ets_model <- stlf(train_data)
stl_ets_forecast <- forecast(stl_ets_model, h = test_length)

# 3. Fit ARIMA model
arima_model <- auto.arima(train_data, trace = TRUE, ic = "aicc") # based on the AICc criterion.
arima_forecast <- forecast(arima_model, h = test_length)

# 4. Fit Holt-Winters model
holt_winters_model <- HoltWinters(train_data, seasonal = "additive")
holt_winters_forecast <- forecast(holt_winters_model, h = test_length)

# 5. Fit ETS model
ets_model <- ets(train_data)
ets_forecast <- forecast(ets_model, h = test_length)







# !---------------------------------------------------!
# --- 8. Model Evaluation, RMSE, AICc, ACF Residual ---
# !---------------------------------------------------! 
# Print Values
sea_naive_accuracy <- accuracy(sea_naive_forecast, test_data)
print(sea_naive_accuracy)

stl_ets_accuracy <- accuracy(stl_ets_forecast, test_data)
print(stl_ets_accuracy)

manArima_accuracy <- accuracy(manualArima_forecast, test_data)
print(manArima_accuracy)
summary(manualArima_forecast)

arima_accuracy <- accuracy(arima_forecast, test_data)
print(arima_accuracy)

holtsWinter_accuracy <- accuracy(holt_winters_forecast, test_data)
print(holtsWinter_accuracy)

# Seasonal Naive Plot
autoplot(sea_naive_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Seasonal Naive Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 

# STL ETS Plot
autoplot(stl_ets_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("STL ETS Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 

# Manual ARIMA Plot
autoplot(manualArima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Manual ARIMA D=1 Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 

# ARIMA Plot
autoplot(arima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("ARIMA Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 

# Holt's Winter Plot
autoplot(holt_winters_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("Holt's Winter Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") 







# !---------------------------------------------------!
# ---------- 9. Best model summary-- -----------------
# !---------------------------------------------------! 
# !!! 1. Best Model: ARIMA(1,0,0)(0,1,1)[12] with drift
print(arima_accuracy)
summary(arima_model)
# Extract residuals from the chosen model (e.g., arima_model)
residuals_arima <- residuals(arima_model)
# Plot ACF of residuals
acf(residuals_arima, main = "ACF of Residuals")
Box.test(residuals(arima_model), lag = 12, type = "Ljung-Box")
checkresiduals(arima_model, plot = TRUE)
# Plot
autoplot(arima_forecast) +
  autolayer(test_data, series = "Actual Data", color = "red") +
  ggtitle("ARIMA Forecast vs Actual Data") +
  xlab("Time") +
  ylab("Demand") +
  guides(colour = guide_legend(title = "Legend"))

