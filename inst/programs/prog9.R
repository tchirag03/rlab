
# Load required libraries for time series analysis and modeling
#install.packages("forecast")
#install.packages("TSA")
library(forecast)
library(ggplot2)
library(TSA)
library(tseries)

# Function to perform Exploratory Data Analysis (EDA) on the time series data
perform_eda <- function(ts_data, dataset_name) {
  cat("Exploratory Data Analysis for", dataset_name, "\n")
  print(summary(ts_data)) # Print summary of the dataset
  plot(ts_data, main = paste(dataset_name, "Time Series"), ylab = "Values", xlab = "Time") # Plot the time series data
  cat("ACF and PACF plots:\n")
  acf(ts_data, main = paste("ACF of", dataset_name)) # Autocorrelation plot
  pacf(ts_data, main = paste("PACF of", dataset_name)) # Partial autocorrelation plot
}

# Function to decompose the time series into trend, seasonal, and residual components
decompose_ts <- function(ts_data, dataset_name) {
  cat("Decomposing the time series for", dataset_name, "\n")
  decomposition <- decompose(ts_data) # Decompose the time series
  plot(decomposition) # Plot the decomposition
  return(decomposition) # Return the decomposition result
}

# Function to fit an ARIMA model to the time series data
fit_arima <- function(ts_data, dataset_name) {
  cat("Fitting ARIMA model for", dataset_name, "\n")
  adf_test <- adf.test(ts_data, alternative = "stationary") # ADF test for stationarity
  cat("ADF Test p-value:", adf_test$p.value, "\n")
  # If p-value > 0.05, data is non-stationary, so we difference the data
  if (adf_test$p.value > 0.05) {
    ts_data <- diff(ts_data) # Difference the data to make it stationary
    plot(ts_data, main = paste(dataset_name, "Differenced Time Series")) # Plot the differenced data
  }
  auto_model <- auto.arima(ts_data, seasonal = FALSE) # Fit ARIMA model (non-seasonal)
  print(summary(auto_model)) # Print ARIMA model summary
  forecast_result <- forecast(auto_model, h = 12) # Forecast next 12 periods
  plot(forecast_result, main = paste(dataset_name, "ARIMA Forecast")) # Plot ARIMA forecast
  return(auto_model) # Return the fitted ARIMA model
}

# Function to fit a Seasonal ARIMA (SARIMA) model to the time series data
fit_sarima <- function(ts_data, dataset_name) {
  cat("Fitting SARIMA model for", dataset_name, "\n")
  auto_sarima <- auto.arima(ts_data, seasonal = TRUE) # Fit SARIMA model (seasonal)
  print(summary(auto_sarima)) # Print SARIMA model summary
  sarima_forecast <- forecast(auto_sarima, h = 12) # Forecast next 12 periods
  plot(sarima_forecast, main = paste(dataset_name, "SARIMA Forecast")) # Plot SARIMA forecast
  return(auto_sarima) # Return the fitted SARIMA model
}

# Function to compare ARIMA and SARIMA models by evaluating forecast accuracy
compare_models <- function(arima_model, sarima_model, ts_data) {
  cat("Comparing ARIMA and SARIMA models:\n")
  h <- min(12, length(ts_data)) # Forecast horizon of 12 or adjusted based on dataset length
  arima_forecast <- forecast(arima_model, h = h) # ARIMA forecast
  sarima_forecast <- forecast(sarima_model, h = h) # SARIMA forecast
  actual_values <- ts_data[(length(ts_data) - h + 1):length(ts_data)] # Actual values for comparison
  # Calculate accuracy of both models
  arima_accuracy <- accuracy(arima_forecast$mean, actual_values)
  sarima_accuracy <- accuracy(sarima_forecast$mean, actual_values)
  cat("ARIMA Forecast Accuracy:\n", arima_accuracy) # Print ARIMA accuracy
  cat("SARIMA Forecast Accuracy:\n", sarima_accuracy) # Print SARIMA accuracy
}

# Function to visualize the comparison of ARIMA and SARIMA forecast performance
plot_forecast_comparison <- function(actual_values, arima_forecast, sarima_forecast, time_points) {
  arima_rmse <- sqrt(mean((arima_forecast - actual_values)^2)) # Calculate RMSE for ARIMA
  sarima_rmse <- sqrt(mean((sarima_forecast - actual_values)^2)) # Calculate RMSE for SARIMA
  # Color coding for better and worse RMSE
  better_color <- ifelse(arima_rmse < sarima_rmse, "green", "red")
  worse_color <- ifelse(arima_rmse < sarima_rmse, "red", "green")

  # Plot actual values and forecasts
  plot(time_points, actual_values, type = "o", col = "blue", pch = 16, lty = 1,
       xlab = "Time", ylab = "Values", main = "Forecast Comparison")
  lines(time_points, arima_forecast, col = better_color, lty = 2, lwd = 2) # ARIMA forecast line
  lines(time_points, sarima_forecast, col = worse_color, lty = 3, lwd = 2) # SARIMA forecast line

  # Add a legend to the plot
  legend("topright",
         legend = c("Actual Values",
                    paste("ARIMA (RMSE =", round(arima_rmse, 2), ")"),
                    paste("SARIMA (RMSE =", round(sarima_rmse, 2), ")")),
         col = c("blue", better_color, worse_color),
         lty = c(1, 2, 3), lwd = c(1, 2, 2), pch = c(16, NA, NA))
}

# Monthly Milk Production Dataset Analysis
data(milk) # Load the Monthly Milk Production dataset
milk_data <- milk # Assign the dataset to a variable
cat("\n--- Monthly Milk Production Dataset ---\n")
perform_eda(milk_data, "Monthly Milk Production")
decompose_ts(milk_data, "Monthly Milk Production")
arima_milk <- fit_arima(milk_data, "Monthly Milk Production")
sarima_milk <- fit_sarima(milk_data, "Monthly Milk Production")
compare_models(arima_milk, sarima_milk, milk_data)

# Forecasting and plot comparison for Milk Production dataset
h_milk <- 12 # Define forecast horizon for Milk Production dataset (12 months ahead)
milk_actual_values <- milk_data[(length(milk_data) - h_milk + 1):length(milk_data)]
arima_milk_forecast <- forecast(arima_milk, h = h_milk)$mean
sarima_milk_forecast <- forecast(sarima_milk, h = h_milk)$mean
time_points_milk <- time(milk_data)[(length(milk_data) - h_milk + 1):length(milk_data)]
plot_forecast_comparison(milk_actual_values, arima_milk_forecast, sarima_milk_forecast, time_points_milk)
