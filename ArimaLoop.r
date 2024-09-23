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
} # for monthly, not statioanry

# KPSS test for additional confirmation
kpss_result <- kpss.test(train_data)
print(kpss_result)

# Double Check:
# Check if the differenced data is now stationary
if (kpss_result$p.value > alpha) {
  print("Before differencing: The time series is now likely stationary (KPSS Test).")
} else {
  print("Before differencing: The time series may still have stationarity issues (KPSS Test).")
} # for monthly, statioanry







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
ggAcf(train_data, lag.max = 36)
ggPacf(train_data, lag.max = 36)

# Define the range of possible parameters
p_max <- 2
d_max <- 1
q_max <- 2
P_max <- 2
D_max <- 1
Q_max <- 2
s_max <- 12  # Seasonal period (e.g. 12 for monthly data)

# Initialize the best model and its AICc
best_models <- list()
best_aicc <- rep(Inf, 10)

# Loop over all possible combinations of parameters
for (p in 0:p_max) {
  for (d in 0:d_max) {
    for (q in 0:q_max) {
      for (P in 0:P_max) {
        for (D in 0:D_max) {
          for (Q in 0:Q_max) {
            # Fit the seasonal ARIMA model
            model <- tryCatch(
              expr = arima(train_data, order = c(p, d, q), seasonal = c(P, D, Q, s_max)),
              error = function(e) NULL
            )
            
            # Check if the model was successfully fit
            if (!is.null(model)) {
              # Calculate the AICc
              npar <- length(model$coef) + 1
              nstar <- length(model$residuals) - model$arma[6] - model$arma[7] * model$arma[5]
              
              bic <- model$aic + npar * (log(nstar) - 2)
              aicc <- model$aic + 2 * npar * (nstar/(nstar - npar - 1) - 1)
              
              # Check if this model is among the top 10 models
              if (aicc < max(best_aicc)) {
                # Find the index of the worst model
                idx <- which.max(best_aicc)
                
                # Replace the worst model with the current model
                best_models[[idx]] <- list(
                  model = model,
                  summary = summary(model),
                  aicc = aicc,
                  parameters = c(p, d, q, P, D, Q)
                )
                best_aicc[idx] <- aicc
              }
            }
          }
        }
      }
    }
  }
}

# Print the top 10 models
for (i in 1:10) {
  cat("Model", i, ":\n")
  cat("Parameters: p =", best_models[[i]]$parameters[1], ", d =", best_models[[i]]$parameters[2], ", q =", best_models[[i]]$parameters[3], ", P =", best_models[[i]]$parameters[4], ", D =", best_models[[i]]$parameters[5], ", Q =", best_models[[i]]$parameters[6], "\n")
  print(best_models[[i]]$summary)
  cat("AICc:", best_models[[i]]$aicc, "\n\n")
}

# Make a forecast using the best model
best_model <- best_models[[which.min(best_aicc)]]$model
arima_forecast <- forecast(best_model, h = test_length)
