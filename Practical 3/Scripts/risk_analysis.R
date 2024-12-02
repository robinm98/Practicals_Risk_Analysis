### Risk Analysis - Practical 3 ###
###################################

# Load the required libraries
library(tseries)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(lubridate)
library(extRemes)
library(evd)
library(ismev)

# Read the data
suicide_attacks <- read.csv("Practical 3/Data/suicide_attacks_cleaned.csv")
attach(suicide_attacks)
View(suicide_attacks)

### Check for stationarity ###
##############################

### Create a ts with 0 when we have a date gap
# Ensure the date column is in Date format
suicide_attacks <- suicide_attacks %>%
  mutate(date = as.Date(date))

# Remove rows with NA dates
suicide_attacks_clean <- suicide_attacks %>%
  filter(!is.na(date))

# Generate a complete sequence of dates and fill gaps with 0
suicide_attacks_filled <- suicide_attacks_clean %>%
  group_by(date) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  complete(date = seq(min(date), max(date), by = "day"), fill = list(total_casualties = 0))

### Test if our ts is stationnary
# Perform the ADF test on the filled data
adf_test_result <- adf.test(suicide_attacks_filled$total_casualties)

# Print the ADF test result
print(adf_test_result) # --> p-value < 0.05 so we reject H0 and we can assume that this time serie is stationnary (H0: non stationnary)

### Plot ts 
ggplot(suicide_attacks_filled, aes(x = date, y = total_casualties)) +
  geom_line() +
  labs(
    title = "Time Series Plot of Total Casualties",
    x = "Date",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Plot ts without extreme values
# Filter out values greater than 1000
filtered_data <- suicide_attacks_filled %>%
  filter(total_casualties <= 1000)

# Plot the time series
ggplot(filtered_data, aes(x = date, y = total_casualties)) +
  geom_line(color = "blue") +
  labs(
    title = "Time Series of Total Casualties (Values ≤ 1000)",
    x = "Date",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Check normality ###
#######################

### Visual check
# Plot histogram with density curve
ggplot(filtered_data, aes(x = total_casualties)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Histogram of Total Casualties with Density Curve",
    x = "Total Casualties",
    y = "Density"
  ) +
  theme_minimal()

# Q-Q plot
qqnorm(suicide_attacks_filled$total_casualties)
qqline(suicide_attacks_filled$total_casualties, col = "red")

### Test check
# Randomly sample 5000 rows if data exceeds 5000
set.seed(123)  # For reproducibility
sample_data <- sample(suicide_attacks_filled$total_casualties, size = 5000)

# Perform Shapiro-Wilk test on the sample
shapiro_test_result <- shapiro.test(sample_data)

# Print the result
print(shapiro_test_result) # --> p < 0.001 we reject H0 and our data is not not normal (H0: data follow normal distribution)

### Compare normal vs t-distribution ###
########################################

# Compares how well the total casualties fit a t-distribution versus a normal distribution

# fit the t-distribution
t_fit <- fitdistr(total_casualties, "t")

# Extract the degrees of freedom (df) from the fitted distribution
df_value <- t_fit$estimate["df"]

# Generate QQ-plot for t-distribution
qqplot(
  qt(ppoints(length(suicide_attacks_filled$total_casualties)), df = df_value),
  suicide_attacks_filled$total_casualties,
  main = "QQ-Plot of Total Casualties vs t-Distribution",
  xlab = "Theoretical Quantiles (t-distribution)",
  ylab = "Sample Quantiles"
)
# Add a 45-degree reference line
qqline(suicide_attacks_filled$total_casualties, distribution = function(p) qt(p, df = df_value), col = "red")

# Generate QQ-plot for normal distribution
qqnorm(suicide_attacks_filled$total_casualties, main = "QQ-Plot of Total Casualties vs Normal Distribution")
qqline(suicide_attacks_filled$total_casualties, col = "red")

#### Block Maxima ####
######################

#remove failed attacks (no casualties)
suicide_attacks_casualties <- suicide_attacks |>
  filter(!is.na(total_casualties) & total_casualties > 0)

# Plot histogram of daily casualties
hist(suicide_attacks_casualties$total_casualties, main = "Histogram of Daily Casualties",
     xlab = "Casualties", breaks = 500)

# With bins to better show the extremes

suicide_attacks_casualties_bins <- suicide_attacks_casualties %>%
  mutate(casualty_bins = cut(total_casualties, 
                             breaks = c(0, 10, 20, 30, 40, 50, 100, 500, 1000, Inf), 
                             labels = c("1-10", "11-20", "21-30", "31-40", "41-50", 
                                        "51-100", "101-500", "501-1000", ">1000")))

table(suicide_attacks_casualties_bins$casualty_bins)
barplot(table(suicide_attacks_casualties_bins$casualty_bins),
        main = "Casualties Binned into Intervals",
        xlab = "Casualty Intervals", ylab = "Frequency")

# Extract yearly maxima
yearly_max <- suicide_attacks_filled %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  summarise(Max_Casualties = max(total_casualties, na.rm = TRUE))

# Plot histogram of yearly maxima
hist(yearly_max$Max_Casualties, main = "Histogram of Yearly Maximum Casualties",
     xlab = "Yearly Maximum Casualties", breaks = 50)

# Fit a linear model for yearly maximum casualties
lm_model <- lm(Max_Casualties ~ Year, data = yearly_max)

# Generate a data frame for the next 10 years
future_years <- data.frame(Year = seq(max(yearly_max$Year) + 1, max(yearly_max$Year) + 10))

# Predict for the next 10 years with confidence intervals
predictions <- predict(lm_model, newdata = future_years, interval = "confidence")

# Combine actual data and predicted values for plotting
combined_years <- c(yearly_max$Year, future_years$Year)
combined_casualties <- c(yearly_max$Max_Casualties, predictions[,1])

# Plot the actual data and predictions
plot(yearly_max$Year, yearly_max$Max_Casualties, main = "Predictions for Next 10 Years",
     xlab = "Year", ylab = "Maximum Daily Casualties", pch = 16)
lines(combined_years, combined_casualties, col = "blue")
lines(future_years$Year, predictions[,2], col = "red", lty = 2)
lines(future_years$Year, predictions[,3], col = "red", lty = 2)
legend("topleft", legend = c("Fitted Line", "Confidence Interval"),
       col = c("blue", "red"), lty = c(1, 2))

# Fit a GEV model with constant parameters
gev_model_const <- fevd(yearly_max$Max_Casualties, type = "GEV")

# Fit a GEV model with time-varying location parameter
gev_model_time_var <- fevd(Max_Casualties ~ Year, data = yearly_max, location.fun = ~Year, type = "GEV")

# Calculate AIC and BIC for model comparison
n_params_const <- length(gev_model_const$results$par)
loglik_const <- gev_model_const$results$value
aic_const <- 2 * n_params_const + 2 * loglik_const
bic_const <- log(nrow(yearly_max)) * n_params_const + 2 * loglik_const

n_params_time_var <- length(gev_model_time_var$results$par)
loglik_time_var <- gev_model_time_var$results$value
aic_time_var <- 2 * n_params_time_var + 2 * loglik_time_var
bic_time_var <- log(nrow(yearly_max)) * n_params_time_var + 2 * loglik_time_var

cat("AIC (Constant Parameters):", aic_const, "\n")
cat("AIC (Time-Varying Location):", aic_time_var, "\n")
cat("BIC (Constant Parameters):", bic_const, "\n")
cat("BIC (Time-Varying Location):", bic_time_var, "\n")

# Plot the GEV model with constant parameters
plot(gev_model_const, main = "GEV Model with Constant Parameters")

# Diagnostic plots
ismev_const <- gev.fit(yearly_max$Max_Casualties)
gev.diag(ismev_const)

# Define return period
T <- 10

# Extract parameters
mu <- ismev_const$mle[1]
sigma <- ismev_const$mle[2]
xi <- ismev_const$mle[3]

# Calculate 10-year return level
z_T <- mu + (sigma / xi) * ((-log(1 - 1 / T))^(-xi) - 1)
cat("10-Year Return Level:", z_T, "\n")

# Plot return level
plot(yearly_max$Year, yearly_max$Max_Casualties, main = "10-Year Return Level Prediction",
     xlab = "Year", ylab = "Maximum Daily Casualties", pch = 16, col = "blue")
abline(h = z_T, col = "red", lty = 2)
legend("topright", legend = c("Yearly Maxima", "10-Year Return Level"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 2))

# Calculate return levels for specified periods
periods <- c(10, 20, 50, 85)
return_levels <- sapply(periods, function(T) {
  mu + (sigma / xi) * ((-log(1 - 1 / T))^(-xi) - 1)
})
names(return_levels) <- periods

# Count historical exceedances
counts_above <- sapply(return_levels, function(level) {
  sum(yearly_max$Max_Casualties > level)
})

print(return_levels)
print(counts_above)

# Return period for a specific threshold
threshold <- 100
return_period_threshold <- 1 / (1 - pgev(threshold, loc = mu, scale = sigma, shape = xi))
cat("Return period for", threshold, "casualties:", return_period_threshold, "years\n")

# Probability of exceeding a threshold in the next year
threshold_daily <- 150
prob_exceed_daily <- 1 - pgev(threshold_daily, loc = mu, scale = sigma, shape = xi)
prob_exceed_annual <- 1 - (1 - prob_exceed_daily)^365
cat("Probability of exceeding", threshold_daily, "casualties in a day at least once in a year:", prob_exceed_annual, "\n")