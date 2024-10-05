### Risk Analytics - Practical 1 ###
####################################

# Load libraries
library(readr)
library(tseries)
library(nortest)
library(MASS)


# Set wd
setwd("/Users/robin/Library/Mobile Documents/com~apple~CloudDocs/UNIL/HEC cours/Master/MA3/Risk Analytics/Practicals/Practical 1")

# Load data
Crypto_data <- read_csv("Data/Crypto_data.csv")
View(Crypto_data)

################## PART 1 ##################

### a

# Extract Bitcoin prices 
bitcoin_prices <- Crypto_data[[1]] # or Crypto_data$Bitcoin

# Plot the Bitcoin prices to visually inspect stationarity
plot(bitcoin_prices, type = "l", main = "Bitcoin Prices", xlab = "Time", ylab = "Price")

# Perform the ADF test
adf_test_result <- adf.test(bitcoin_prices)

# Print the ADF test result
print(adf_test_result) # p-value = 0.3885 > 0.05, so it's non-stationary as we don't reject H0 (H0: Data are non-stationnary)

### b

# Create a function to compute negative log returns
negative_log_returns <- function(prices) {
  # Calculate log returns
  log_returns <- diff(log(prices))
  # Return the negative of the log returns
  return(-log_returns)
}

# Apply the function to the Bitcoin prices
bitcoin_negative_log_returns <- negative_log_returns(bitcoin_prices)

# Plot the negative log returns
plot(bitcoin_negative_log_returns, type = "l", main = "Negative Log Returns of Bitcoin", xlab = "Time", ylab = "Negative Log Returns")

# Perform the ADF test on negative log returns
adf_test_neg_log <- adf.test(bitcoin_negative_log_returns)

# Print the result
print(adf_test_neg_log) # p-value = 0.01 < 0.05, so it's stationary as we reject H0 (H0: Data are non-stationnary)


### c

# Draw a histogram of the negative log returns
hist(bitcoin_negative_log_returns, breaks = 30, main = "Histogram of Negative Log Returns", xlab = "Negative Log Returns", col = "lightblue", border = "black")
# --> seem normally distributed

# Create a QQ-plot to check normality
qqnorm(bitcoin_negative_log_returns, main = "QQ-Plot of Negative Log Returns")
qqline(bitcoin_negative_log_returns, col = "red") # --> normally distributed only for non-extreme values

# Perform the Anderson-Darling test
ad_test_result <- ad.test(bitcoin_negative_log_returns)

# Print the test result
print(ad_test_result) # p-value < 0.05, so data are not normally distributed as we reject H0 (H0: Data are normally distributed)


### d

# Scale the negative log returns
bitcoin_negative_log_returns_scaled <- scale(bitcoin_negative_log_returns)

# Fit a t-distribution to the negative log returns
t_fit <- fitdistr(bitcoin_negative_log_returns_scaled, "t")

# Extract the degrees of freedom (df) from the fitted distribution
df_value <- t_fit$estimate["df"]

### QQ-plot for t-distribution : seem to follow it quite well
# Generate a QQ-plot for the t-distribution fit
qqplot(qt(ppoints(length(bitcoin_negative_log_returns)), df = df_value), 
       bitcoin_negative_log_returns, main = "QQ-Plot of Negative Log Returns t-Distribution",
       xlab = "Theoretical Quantiles (t-distribution)", ylab = "Sample Quantiles")
# Add a 45-degree line to the QQ-plot
qqline(bitcoin_negative_log_returns, distribution = function(p) qt(p, df = df_value), col = "red")


### QQ-plot for normal distribution : not so well
# For comparison, create a QQ-plot for the normal distribution fit
qqnorm(bitcoin_negative_log_returns, main = "QQ-Plot of Negative Normal Distribution")
qqline(bitcoin_negative_log_returns, col = "red")

# --> fit seem better with t-distribution than normal distribution

################## Compare data distribution with normal and t-distribution #############################

# Extract the fitted parameters from t_fit
m <- t_fit$estimate["m"]      # Mean (location parameter)
s <- t_fit$estimate["s"]      # Scale (related to standard deviation)
df <- t_fit$estimate["df"]    # Degrees of freedom

# Calculate the mean and standard deviation for the normal distribution
mean_normal <- mean(bitcoin_negative_log_returns_scaled)
sd_normal <- sd(bitcoin_negative_log_returns_scaled)

# Plot the histogram of your data
hist(bitcoin_negative_log_returns_scaled, breaks = 30, probability = TRUE, 
     main = "Histogram of Bitcoin Negative Log Returns with Fitted t-Distribution and Normal Distribution",
     xlab = "Negative Log Returns", col = "lightblue", border = "black")

# Overlay the density of the fitted t-distribution
x_vals <- seq(min(bitcoin_negative_log_returns_scaled), max(bitcoin_negative_log_returns_scaled), length.out = 1000)
t_density <- dt((x_vals - m) / s, df) / s  # Density of the t-distribution with fitted parameters

# Add the t-distribution curve to the plot
lines(x_vals, t_density, col = "red", lwd = 2)

# Overlay the density of the normal distribution
normal_density <- dnorm(x_vals, mean = mean_normal, sd = sd_normal)

# Add the normal distribution curve to the plot
lines(x_vals, normal_density, col = "blue", lwd = 2)

# Add a legend to distinguish between t-distribution and normal distribution
legend("topright", legend = c("Fitted t-Distribution", "Normal Distribution"), 
       col = c("red", "blue"), lwd = 2)

############################################################################################################

### e

# Calculate mean and standard deviation of the negative log returns
mean_neg_log_returns <- mean(bitcoin_negative_log_returns)
sd_neg_log_returns <- sd(bitcoin_negative_log_returns)

# Create a sequence of values for plotting the densities
x_vals <- seq(min(bitcoin_negative_log_returns), max(bitcoin_negative_log_returns), length.out = 1000)

# Calculate density for the normal distribution with the same mean and standard deviation
normal_density <- dnorm(x_vals, mean = mean_neg_log_returns, sd = sd_neg_log_returns)

# Calculate density for the t-distribution with the fitted degrees of freedom
t_density <- dt((x_vals - mean_neg_log_returns) / sd_neg_log_returns, df) / sd_neg_log_returns

# Plot the densities to compare tails
plot(x_vals, normal_density, type = "l", col = "blue", lwd = 2, 
     main = "Density Comparison: Normal vs t-Distribution",
     ylab = "Density", xlab = "Negative Log Returns")
lines(x_vals, t_density, col = "red", lwd = 2)
legend("topright", legend = c("Normal", "t-Distribution"), col = c("blue", "red"), lwd = 2)

# --> t-distribution has heavier tails than normal distribution, so it accounts for extreme values better

### Conslusion of part 1 

"Since the Bitcoin data follows the t-distribution more closely, and the t-distribution has fatter tails than the 
normal distribution, it means that extreme values (large deviations from the mean) are more likely in the Bitcoin data 
than if it were normally distributed."

############################################################################################################################################

### Part 2 ###

# Plot the ACF of the raw Bitcoin series
ggAcf(bitcoin_prices, main = "ACF of Negative Log Returns")

# Plot the ACF of the negative log returns
ggAcf(bitcoin_negative_log_returns, main = "ACF of Negative Log Returns")


