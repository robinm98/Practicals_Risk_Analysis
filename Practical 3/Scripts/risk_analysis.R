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

#### POT - Threshold minimum to define it is an extreme event ####`

# We take the positive values
data_extremes <- suicide_attacks_casualties$total_casualties

# We trace the Mean Residual Life Plot
thresholds <- seq(quantile(data_extremes, 0.85), quantile(data_extremes, 0.99), length.out = 50)
mean_excess <- sapply(thresholds, function(u) {
  excesses <- data_extremes[data_extremes > u] - u
  mean(excesses, na.rm = TRUE)
})

mrl_plot <- data.frame(thresholds, mean_excess)
ggplot(mrl_plot, aes(x = thresholds, y = mean_excess)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Mean Residual Life Plot",
    x = "Threshold",
    y = "Mean Excess"
  ) +
  theme_minimal()
## A linearity area seems to be reached between 100-150, which is a candidate. If the slope is too weak, we could lose some info.
## It is possible to modelize the events above the thresholds with GPD (Generalized Pareto Distribution).

# Adjust a model GPD for different thresholds.
thresholds_gpd <- seq(quantile(data_extremes, 0.9), quantile(data_extremes, 0.99), length.out = 10)
gpd_params <- lapply(thresholds_gpd, function(thresh) {
  excesses <- data_extremes[data_extremes > thresh] - thresh
  if (length(excesses) > 10) {
    gpd_fit <- fpot(excesses, threshold = 0)
    return(gpd_fit$estimate)
  } else {
    return(NULL)
  }
})

# Extract the parameters Shape et Scale for each threshold
shape_params <- sapply(gpd_params, function(params) if (!is.null(params)) params["shape"] else NA)
scale_params <- sapply(gpd_params, function(params) if (!is.null(params)) params["scale"] else NA)

# Trace the stability of the parameters
param_data <- data.frame(
  Threshold = thresholds_gpd,
  Shape = shape_params,
  Scale = scale_params
)

ggplot(param_data, aes(x = Threshold)) +
  geom_line(aes(y = Shape), color = "blue") +
  geom_point(aes(y = Shape), color = "red") +
  labs(
    title = "Stability of Shape Parameter",
    x = "Threshold",
    y = "Shape Parameter"
  ) +
  theme_minimal()

ggplot(param_data, aes(x = Threshold)) +
  geom_line(aes(y = Scale), color = "green") +
  geom_point(aes(y = Scale), color = "red") +
  labs(
    title = "Stability of Scale Parameter",
    x = "Threshold",
    y = "Scale Parameter"
  ) +
  theme_minimal()

# Determine the optimal threshold
## For the shape, the parameter seems to be stable between 100-150. Above 200, it becomes unstable.
## This parameter determine the probability of exterme events. It is predictable in this area of 100-150 casualties.
## For the scale, same between 100-150, above 200, it becomes unstable.
## Mean Amplitude of excesses above the threshold. Between 100-150, possible to predict this amplitude. Above, not enough data to be precise.

# Extract the excesses above the threshold
threshold <- 120
data_extremes <- suicide_attacks$total_casualties
excesses <- data_extremes[data_extremes > threshold] - threshold  # Excès au-dessus du seuil

# Aajust a GPD distribution to the excesses.
gpd_fit <- fpot(excesses, threshold = 0)  # Ajustement du modèle GPD
summary(gpd_fit)  # Résumé des paramètres ajustés

# Extract the estimated parameters of GPD
shape_param <- gpd_fit$estimate["shape"]  # ξ (shape parameter)
scale_param <- gpd_fit$estimate["scale"]  # 𝜎 (scale parameter)

# Estimating extremes quantiles
# Probabilities for quantiles
probabilities <- c(0.95, 0.99)

# Function to calculate the quantiles based on GPD
gpd_quantiles <- function(p, threshold, shape, scale, n_excess, total_n) {
  lambda <- n_excess / total_n  # Proportion of events above the threshold
  quantile <- threshold + (scale / shape) * (((1 - p) / lambda)^(-shape) - 1)
  return(quantile)
}

# Calcul of quantiles for the specified probabilities
n_excess <- length(excesses)  # Number of excesses above the threshold
total_n <- length(data_extremes)  # Total number of events
quantiles <- sapply(probabilities, function(p) {
  gpd_quantiles(p, threshold, shape_param, scale_param, n_excess, total_n)
})
names(quantiles) <- paste0(probabilities * 100, "%")
print(quantiles)

# Analysis of the frequency & evaluation of total risk
# Estimate the occurence of excesses
frequency <- n_excess / total_n
print(paste("Frequence of occurence of the excesses (threshold =", threshold, "):", frequency))

# Combiner the frequence and the excesses to evaluate total risk
expected_excess <- mean(excesses)  # Mean of the excesses
total_risk <- frequency * (threshold + expected_excess)  # Total risk estimated
print(paste("Risque total estimé :", total_risk))

# Visualisation of excesses with the adjusted GPD
ggplot(data.frame(excesses), aes(x = excesses)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  stat_function(fun = function(x) {
    dgpd(x, loc = 0, scale = scale_param, shape = shape_param)
  }, color = "red", size = 1) +
  labs(
    title = "Ajustement of the GPD to excesses",
    x = "Excesses (above threshold)",
    y = "Density"
  ) +
  theme_minimal()
## Quantiles extraction : 95% : 226.83 (120+106.83), 99% : 366.34 as a probability of losses
## Frequency of occurence for exterme events at 120 : 4.1%
## Freq * (Threshold + Mean excess) = 10.49 additional casualties for events > 120.


-----

# Definition of a threshold for extreme values (95th percentile)
threshold <- quantile(suicide_attacks_filled$total_casualties, 0.95, na.rm = TRUE)

# Computation of the extremal index for the entire time series
extremal_index <- extremalindex(suicide_attacks_filled$total_casualties, threshold = threshold)
cat("Extremal Index:", extremal_index, "\n")
cat("Do extremes occur in clusters? ", ifelse(extremal_index < 1, "Yes", "No"), "\n")

## Extremal Index: 0.06736063 41 31 

## Do extremes occur in clusters?  Yes No No 

# Probability of extreme event tomorrow given today's is extreme
prob_extreme_tomorrow <- extremal_index
cat("Probability that if today's total casualties are extreme, tomorrow's will also be extreme:", prob_extreme_tomorrow, "\n")

## Probability that if today's total casualties are extreme, tomorrow's will also be extreme: 0.06736063 41 31 

# Declusterization of the data using the defined threshold
declustered_data <- decluster(suicide_attacks_filled$total_casualties, threshold = threshold)

# Plotting the original and declustered data
ggplot() +
  geom_line(data = suicide_attacks, aes(x = 1:nrow(suicide_attacks), y = total_casualties),
            alpha = 0.5, color = "blue", linewidth = 1) +
  geom_point(data = data.frame(Index = seq_along(declustered_data), Declustered = declustered_data), 
             aes(x = Index, y = Declustered), color = "orange", size = 1.5) +
  labs(
    title = "Declustered Suicide Attacks Data",
    x = "Index",
    y = "Total Casualties"
  ) +
  theme_minimal()

# Fittig a Generalized Pareto Distribution (GPD) to the raw and declustered data
fit_gpd_raw <- fevd(suicide_attacks_filled$total_casualties, threshold = threshold, type = "GP", method = "MLE")
fit_gpd_decl <- fevd(declustered_data, threshold = threshold, type = "GP", method = "MLE")

## Summarizing the fitted models
summary(fit_gpd_raw)

## Negative Log-Likelihood Value:  4154.435 
##  AIC = 8312.871 BIC = 8321.956 

summary(fit_gpd_decl)

# Negative Log-Likelihood Value:  2449.936 
#  AIC = 4903.871 ; BIC = 4911.819 

## We can clearly see that the model with the lowest AIC and BIC values is the one with declustered data
## and among competing models is preferred.

# Compute 10-year return level
return_period <- 10
return_level_raw <- return.level(fit_gpd_raw, return.period = return_period)
return_level_decl <- return.level(fit_gpd_decl, return.period = return_period)

cat("10-year Return Level (Raw Data):", return_level_raw, "\n")

## 10-year Return Level (Raw Data): 1236.513 
## An event with 1236 casualties or more is expected to occur, on average, once every 10 years

cat("10-year Return Level (Declustered Data):", return_level_decl, "\n")

## 10-year Return Level (Declustered Data): 1348.169 
## An event with 1348 casualties or more is expected to occur, on average, once every 10 years

###########################################################
###########################################################
###########################################################