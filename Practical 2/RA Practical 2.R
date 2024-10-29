# Libraries
library(readr)
library(dplyr)
library(lubridate)
library(extRemes)
library(evd)

# (a) Load data using a relative paths and plotting
gv_temp <- read_csv("Practical 2/Data/Geneva_temperature.csv")
ls_rain <- read_csv("Practical 2/Data/Precipitation_lausanne_full.csv")

# Plot histogram of daily precipitation frequency
hist(ls_rain$Precipitation, main="Histogram of Daily Precipitation", xlab="Daily Precipitation (mm)")

# (b) Extract yearly maxima and plot histogram

# Convert the Date column from character to Date format (adjusting for the format)
ls_rain <- ls_rain |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Extract yearly maxima
yearly_max <- ls_rain |> 
  group_by(Year = year(Date)) |> 
  summarise(Precipitation = max(Precipitation, na.rm = TRUE))

# Plot the histogram of yearly maxima
hist(yearly_max$Precipitation, main="Histogram of Yearly Maxima", xlab="Yearly Maxima (mm)", breaks=10)

#### (c) Linear Model for Yearly Max Precipitation ####

# Fit a linear model for yearly maximum precipitation
lm_model <- lm(Precipitation ~ Year, data=yearly_max)

# Generate a data frame for the next 10 years
future_years <- data.frame(Year = seq(max(yearly_max$Year) + 1, max(yearly_max$Year) + 10))

# Predict for the next 10 years with confidence intervals
predictions <- predict(lm_model, newdata=future_years, interval="confidence")

# Combine actual data and predicted values for plotting
combined_years <- c(yearly_max$Year, future_years$Year)
combined_precipitation <- c(yearly_max$Precipitation, predictions[,1])

# Plot the actual data and predictions
plot(yearly_max$Year, yearly_max$Precipitation, main="Predictions for Next 10 Years",
     xlab="Year", ylab="Precipitation (mm)", pch=16)
# Add the fitted line and confidence intervals
lines(combined_years, combined_precipitation, col="blue")
lines(future_years$Year, predictions[,2], col="red", lty=2)  # Lower bound
lines(future_years$Year, predictions[,3], col="red", lty=2)  # Upper bound
legend("topleft", legend=c("Fitted Line", "Confidence Interval"), col=c("blue", "red"), lty=c(1, 2))

#### (d) GEV Model with Constant and Time-Varying Parameters ####

# Fit a GEV model with constant parameters
gev_model_const <- fevd(Precipitation ~ Year, data = yearly_max, type = "GEV")

# Fit a GEV model with time-varying location parameter (linear trend with Year)
gev_model_time_var <- fevd(Precipitation ~ Year, data = yearly_max, location.fun = ~ Year, type = "GEV")

# Manually calculate AIC for the constant model
n_params_const <- length(gev_model_const$results$par)  # Number of estimated parameters
loglik_const <- gev_model_const$results$value           # Negative log-likelihood
aic_const <- 2 * n_params_const + 2 * loglik_const      # AIC formula
bic_const <- 2 * loglik_const + log(nrow(yearly_max)) * n_params_const

# Manually calculate AIC + BIC for the time-varying model
n_params_time_var <- length(gev_model_time_var$results$par)
loglik_time_var <- gev_model_time_var$results$value
aic_time_var <- 2 * n_params_time_var + 2 * loglik_time_var
bic_time_var <- 2 * loglik_time_var + log(nrow(yearly_max)) * n_params_time_var

# Print the AIC and BIC values
cat("AIC (Constant Parameters):", aic_const, "\n")
cat("AIC (Time-Varying Location):", aic_time_var, "\n")
cat("BIC (Constant Parameters):", bic_const, "\n")
cat("BIC (Time-Varying Location):", bic_time_var, "\n")

# Plot the GEV model with constant parameters
plot(gev_model_const, main="GEV Model with Constant Parameters")

#### (e) Diagnostic Plots ####

library(ismev)
ismevconst <- gev.fit(yearly_max$Precipitation)
gev.diag(ismevconst)

#### (f) Predict 10-Year Return Level and Plot ####

# Define return period
T <- 10  # 10-year return period

# Extract parameters from ismevconst
mu <- ismevconst$mle[1]   # Location parameter
sigma <- ismevconst$mle[2] # Scale parameter
xi <- ismevconst$mle[3]    # Shape parameter

# Calculate 10-year return level using the GEV formula
z_T <- mu + (sigma / xi) * ((-log(1 - 1 / T))^(-xi) - 1)
cat("10-Year Return Level:", z_T, "\n")

# Plot historical data and 10-year return level
plot(yearly_max$Year, yearly_max$Precipitation, main="10-Year Return Level Prediction",
     xlab="Year", ylab="Precipitation (mm)", pch=16, col="blue")
abline(h = z_T, col = "red", lty = 2)
legend("topright", legend=c("Yearly Maxima", "10-Year Return Level"), col=c("blue", "red"), pch=c(16, NA), lty=c(NA, 2))

#### (g) Historical Values Above Specified Return Levels ####

# Define return periods
periods <- c(10, 20, 50, 85)

# Calculate return levels for each period using the GEV model
return_levels <- sapply(periods, function(T) {
  mu + (sigma / xi) * ((-log(1 - 1 / T))^(-xi) - 1)
})
names(return_levels) <- periods

# Count historical values above each return level
counts_above <- sapply(return_levels, function(level) {
  sum(yearly_max$Precipitation > level)
})

# Display the results
return_levels
counts_above

#### (h) Return Period of 100 mm of Precipitation ####

# Define threshold
threshold <- 100

# Calculate return period
return_period_100mm <- 1 / (1 - pgev(threshold, loc = mu, scale = sigma, shape = xi))
cat("Return period for 100 mm precipitation:", return_period_100mm, "years\n")

#### (i) Probability of Exceeding 150 mm in a Given Year ####

# Define threshold for daily event
threshold_daily <- 150

# Probability of exceeding 150 mm in one day
prob_exceed_daily <- 1 - pgev(threshold_daily, loc = mu, scale = sigma, shape = xi)

# Assuming 365 days of independence, probability of at least one day > 150 mm in a year
prob_exceed_annual <- 1 - (1 - prob_exceed_daily)^365
cat("Probability of exceeding 150 mm in a day at least once in a year:", prob_exceed_annual, "\n")
