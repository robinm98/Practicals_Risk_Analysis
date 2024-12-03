# Libraries
library(readr)
library(dplyr)
library(lubridate)
library(extRemes)
library(evd)
library(ggplot2)
library(POT)

################# Part.1 ######################

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


################# Part.2 ######################

#### (a) Time series plot of the daily precipitation ####

# Create a time series plot of daily precipitation
ggplot(data = ls_rain, aes(x = Date, y = Precipitation)) +
  geom_line(color = "blue") +
  labs(title = "Time Series Plot of Daily Precipitation",
       x = "Date",
       y = "Daily Precipitation (mm)") +
  theme_minimal()

#### (b) Mean Residual Life Plot and Threshold Selection ####

# Create a Mean Residual Life Plot to determine a reasonable threshold
mrlplot(ls_rain$Precipitation, main="Mean Residual Life Plot for Daily Precipitation", xlab="Threshold (mm)", ylab="Mean Excess")

# Choose a threshold based on the MRL plot
threshold <- 30

# Highlight data that exceeds the threshold in the time series plot
ggplot(data = ls_rain, aes(x = Date, y = Precipitation)) +
  geom_line(color = "blue") +
  geom_point(data = subset(ls_rain, Precipitation > threshold), aes(x = Date, y = Precipitation), color = "red") +
  labs(title = "Time Series Plot of Daily Precipitation with Highlighted Exceedances",
       x = "Date",
       y = "Daily Precipitation (mm)") +
  theme_minimal()

#### (c) Fit a GPD for Data Exceeding Threshold ####

# Fit a Generalized Pareto Distribution (GPD) for data exceeding the threshold
gpd_fit <- fpot(ls_rain$Precipitation, threshold = threshold, method = "Nelder-Mead")

# Diagnostic plot for GPD fit
par(mfrow = c(2, 2))
plot(gpd_fit)

# Based on the diagnostic plots, the GPD model appears to be a reasonable fit. The QQ plot shows that most points lie along the 45-degree line, indicating a good fit. The return level plot also shows consistency between the model and empirical data. If significant deviations were present, we would reconsider the choice of the threshold.


#### (d) Compute Return Levels for Different Periods ####

# Define return periods
return_periods <- c(10, 20, 50, 85)

# Extract GPD parameters
threshold_gpd <- gpd_fit$threshold
scale <- gpd_fit$param["scale"]
shape <- gpd_fit$param["shape"]

# Calculate lambda (rate of exceedance)
n_exceedances <- sum(ls_rain$Precipitation > threshold_gpd)
lambda <- n_exceedances / nrow(ls_rain)

# Calculate return levels for specified return periods using the fitted GPD model
return_levels_gpd <- sapply(return_periods, function(T) {
  if (shape != 0) {
    threshold_gpd + (scale / shape) * (((T / lambda)^shape) - 1)
  } else {
    threshold_gpd + scale * log(T / lambda)
  }
})

# Print return levels
names(return_levels_gpd) <- return_periods
cat("Return levels for specified return periods:
")
print(return_levels_gpd)


#### (e) Compute Return Period for 100 mm of Precipitation ####


# Calculate the return period for 100 mm of precipitation using the GPD model
if (shape != 0) {
  return_period_100 <- (1 / lambda) * (1 + (shape / scale) * (threshold - threshold_gpd))^(1 / shape)
} else {
  return_period_100 <- (1 / lambda) * exp((threshold - threshold_gpd) / scale)
}

# Print the return period for 100 mm of precipitation
cat("Return period for 100 mm of precipitation:", return_period_100, "years
")


#### (f) Probability of Exceeding 150 mm in a Given Year ####

# Define threshold for daily event
threshold_daily <- 150

# Probability of exceeding 150 mm in one day
prob_exceed_daily <- 1 - pgev(threshold_daily, loc = mu, scale = sigma, shape = xi)

# Assuming 365 days of independence, probability of at least one day > 150 mm in a year
prob_exceed_annual <- 1 - (1 - prob_exceed_daily)^365
cat("Probability of exceeding 150 mm in a day at least once in a year:", prob_exceed_annual, "\n")


#### (g) Compare POT Approach with Block Maxima Method ####

# Comparison Explanation
cat("Comparison of POT and Block Maxima Methods:\n")

# Advantages of POT (Peaks Over Threshold) Approach
cat("Advantages of POT Approach:\n")
cat("- More data points: The POT method uses all exceedances over a threshold, leading to more data points and potentially more accurate parameter estimation.\n")
cat("- Flexibility: The choice of threshold allows flexibility in modeling extremes, capturing the tail of the distribution more effectively.\n")

# Drawbacks of POT Approach
cat("Drawbacks of POT Approach:\n")
cat("- Threshold selection: Choosing an appropriate threshold can be challenging and subjective, which affects the model fit.\n")
cat("- Sensitivity: POT is sensitive to the threshold value, and an inappropriate choice can lead to biased estimates.\n")

# Advantages of Block Maxima Method
cat("Advantages of Block Maxima Method:\n")
cat("- Simplicity: The method is conceptually simple, as it only takes the maximum value from each block of data (e.g., annual maxima).\n")
cat("- Widely used: It has been traditionally used for extreme value analysis and is often easier to communicate in practice.\n")

# Drawbacks of Block Maxima Method
cat("Drawbacks of Block Maxima Method:\n")
cat("- Loss of information: By only using one maximum value per block, the method discards a lot of data, which can lead to less accurate parameter estimates.\n")
cat("- Larger variance: Due to fewer data points, the estimates can have larger variance, especially when the number of blocks is small.\n")

# Preference
cat("Preference:\n")
cat("In most cases, the POT approach is preferred when the goal is to make the best use of available extreme data and the threshold can be selected appropriately. However, if simplicity is desired and the data is naturally segmented into clear blocks, the Block Maxima method may be more suitable.\n")


################# Part.3 ######################
#### (a) Upload the Geneva temperature data. Plot the data. Subset the data for the summer months (June to September) ####

# Load the Geneva temperature data
data <- read.csv("Practical 2/Data/Geneva_temperature.csv")

# Combine "Year" and "Month" into a single "Date" column
data$Date <- as.Date(paste(data$Year, data$Month, "01", sep = "-"))

# Plot the temperature data over time
ggplot(data, aes(x = Date, y = AvgTemperature)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Geneva Temperature Data",
    x = "Date",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()

# Subset the data for summer months (June to September)
summer_data <- subset(data, Month >= 6 & Month <= 9)

# Plot the summer temperature data
ggplot(summer_data, aes(x = Date, y = AvgTemperature)) +
  geom_line(color = "orange", linewidth = 1) +
  labs(
    title = "Geneva Summer Temperature Data (June to September)",
    x = "Date",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()

#### (b) Compute the extremal index of the subsetted series with appropriatelly chosen threshold. Do the extremes occur in clusters? ####
#### What is the probability that if the temperature today is extreme (above the chosen threshold) then tomorrow will be also extreme? ####

# Define the threshold for extreme temperatures
threshold <- quantile(summer_data$AvgTemperature, 0.95, na.rm = TRUE) # 95th percentile

# Compute the extremal index
extremal_index <- extremalindex(summer_data$AvgTemperature, threshold = threshold)

# Print the results
cat("Extremal Index:", extremal_index, "\n")

# Check if extremes occur in clusters
cat("Do extremes occur in clusters? ", ifelse(extremal_index < 1, "Yes", "No"), "\n")

# Compute the probability that if today's temperature is extreme, tomorrow's is also extreme
prob_extreme_tomorrow <- extremal_index
cat("Probability that if today's temperature is extreme, tomorrow's will be also extreme:", prob_extreme_tomorrow, "\n")

#### (c)  Decluster the data using a suitable threshold. Plot the resulting declustered data ####

# Define the threshold for extreme temperatures
threshold <- quantile(summer_data$AvgTemperature, 0.95, na.rm = TRUE) # 95th percentile

# Decluster the data using the decluster function
declustered_data <- decluster(summer_data$AvgTemperature, threshold = threshold)

# Create a data frame for plotting (to align with declustered indices)
declustered_series <- data.frame(
  Index = seq_along(declustered_data),
  AvgTemperature = declustered_data
)

# Plot the original and declustered data

ggplot() +
  geom_line(data = summer_data, aes(x = 1:nrow(summer_data), y = AvgTemperature),
            alpha = 0.5, color = "blue", linewidth = 1) +
  geom_point(data = declustered_series, aes(x = Index, y = AvgTemperature),
             color = "orange", size = 1.5) +
  labs(
    title = "Declustered Geneva Summer Temperature Data",
    x = "Index",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()


#### (d) Fit a Generalized Pareto Distribution (GPD) to the data, both raw and declustered. Compare the models and compute 10-year return level ####

# Define the threshold for extreme temperatures
threshold <- quantile(summer_data$AvgTemperature, 0.95, na.rm = TRUE) # 95th percentile

# Fit a GPD to the raw data
fit_gpd_raw <- fevd(summer_data$AvgTemperature, threshold = threshold, type = "GP", method = "MLE")

# Fit a GPD to the declustered data
fit_gpd_decl <- fevd(declustered_data, threshold = threshold, type = "GP", method = "MLE")

# Summary of fitted models
summary(fit_gpd_raw)
summary(fit_gpd_decl)

# Compute the 10-year return level for both models
return_period <- 10
return_level_raw <- return.level(fit_gpd_raw, return.period = return_period)
return_level_decl <- return.level(fit_gpd_decl, return.period = return_period)

# Print return levels
cat("10-year Return Level (Raw Data):", return_level_raw, "\n")
cat("10-year Return Level (Declustered Data):", return_level_decl, "\n")
