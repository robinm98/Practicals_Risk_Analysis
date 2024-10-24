# Libraries
library(readr)
library(dplyr)
library(lubridate)
library(extRemes)

# Load data using a relative paths
gv_temp <- read_csv("Practical 2/Data/Geneva_temperature.csv")
ls_rain <- read_csv("Practical 2/Data/Precipitation_lausanne_full.csv")

#### Plotting Rainfall Data ####

# Plot histogram of daily precipitation frequency
hist(ls_rain$Precipitation, main="Histogram of Daily Precipitation", xlab="Daily Precipitation (mm)")

# Convert the Date column from character to Date format (adjusting for the format)
ls_rain <- ls_rain |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))  # Format is MM/DD/YYYY in your data

# Extract yearly maxima
yearly_max <- ls_rain |> 
  group_by(Year = year(Date)) |> 
  summarise(Precipitation = max(Precipitation, na.rm = TRUE))  # Handle NA values in precipitation

# Plot the histogram of yearly maxima
hist(yearly_max$Precipitation, main="Histogram of Yearly Maxima", xlab="Yearly Maxima (mm)", breaks=10)

#### LM ####

# Fit a linear model for yearly maximum precipitation
lm_model <- lm(Precipitation ~ Year, data=yearly_max)

# Generate a data frame for the next 10 years
future_years <- data.frame(Year = seq(max(yearly_max$Year) + 1, max(yearly_max$Year) + 10))

# Predict for the next 10 years with confidence intervals
predictions <- predict(lm_model, newdata=future_years, interval="confidence")

# Combine actual data and predicted values for plotting
combined_years <- c(yearly_max$Year, future_years$Year)
combined_precipitation <- c(yearly_max$Precipitation, predictions[,1])

# Plot the actual data
plot(yearly_max$Year, yearly_max$Precipitation, main="Predictions for Next 10 Years",
     xlab="Year", ylab="Precipitation (mm)", pch=16)

# Add the fitted line for actual + predicted values
lines(combined_years, combined_precipitation, col="blue")

# Add confidence intervals
lines(future_years$Year, predictions[,2], col="red", lty=2)  # Lower bound
lines(future_years$Year, predictions[,3], col="red", lty=2)  # Upper bound

# Optionally, add legend
legend("topleft", legend=c("Fitted Line", "Confidence Interval"), col=c("blue", "red"), lty=c(1, 2))

#### GEV ####

##################### Current problem with GEV time varying model #####################

# Fit a GEV model with constant parameters
gev_model_const <- fevd(yearly_max$Precipitation, type = "GEV")

# Fit a GEV model with time-varying location parameter (linear trend with Year)
gev_model_time_var <- fevd(Precipitation ~ Year, data = yearly_max, type = "GEV")

# Manually calculate AIC for the constant model
n_params_const <- length(gev_model_const$results$par)  # Number of estimated parameters
loglik_const <- gev_model_const$results$value           # Negative log-likelihood
aic_const <- 2 * n_params_const + 2 * loglik_const      # AIC formula
bic_const <- 2 * loglik_const + log(nrow(yearly_max)) * n_params_const  # BIC formula

# Manually calculate AIC for the time-varying model
n_params_time_var <- length(gev_model_time_var$results$par)
loglik_time_var <- gev_model_time_var$results$value
aic_time_var <- 2 * n_params_time_var + 2 * loglik_time_var
bic_time_var <- 2 * loglik_time_var + log(nrow(yearly_max)) * n_params_time_var

# Print the AIC and BIC values
cat("AIC (Constant Parameters):", aic_const, "\n")
cat("AIC (Time-Varying Location):", aic_time_var, "\n")
cat("BIC (Constant Parameters):", bic_const, "\n")
cat("BIC (Time-Varying Location):", bic_time_var, "\n")
