### Risk Analytics - Practical 1 ###
####################################

# Load libraries
library(readr)
library(tseries)
library(nortest)
library(MASS)
library(fpp2)
library(fGarch)
library(lmtest)


# Load data using a relative path
Crypto_data <- read_csv("Practical 1/Data/Crypto_data.csv")

# View the data
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

################## PART 2 ##################

### a

# Plot the ACF of the raw Bitcoin series
ggAcf(bitcoin_prices, main = "ACF of Negative Log Returns") # significant autocorrelation, non-stationary, difficult to model

# Plot the ACF of the negative log returns
ggAcf(bitcoin_negative_log_returns, main = "ACF of Negative Log Returns") # no significant autocorrelation, stationary, easier to model

"Non-stationary time series are more difficult to model because the key statistical properties like mean, variance, 
and autocovariance change over time. This variability makes it challenging to build predictive models since the underlying 
structure of the data is constantly shifting." 

"Stationary time series are easier to model because their statistical properties are constant over time. This stability 
allows models (like ARMA, ARIMA, or GARCH) to assume that past behavior can be used to predict future behavior, given that 
the underlying dynamics of the series don't change."

### b

# Ljung-Box test : H0 = no autocorrelation (white noise) / H1 = autocorrelation in the data
# --> a ts with some autocorrelation can still be stationary

# Perform Ljung-Box test for the raw Bitcoin series
ljung_box_raw <- Box.test(bitcoin_prices, lag = 20, type = "Ljung-Box") # lag = 20 because rule of thumb n/10

# Print the results for the raw series
print(ljung_box_raw) # --> p-value < 0.05, so we reject H0, there is autocorrelation in the data

# Perform Ljung-Box test for the negative log returns
ljung_box_neg_log <- Box.test(bitcoin_negative_log_returns, lag = 20, type = "Ljung-Box")

# Print the results for the negative log returns
print(ljung_box_neg_log) # --> p-value < 0.05, so we reject H0, there is autocorrelation in the data BUT doesn't mean non-stationary

"Even though the negative log returns are stationary, they still exhibit some autocorrelation but the autocorellation 
is much lower for the negative log returns compared to the raw Bitcoin series."

### c

# Plot the ACF of the negative log returns
ggAcf(bitcoin_negative_log_returns, main = "ACF of Negative Log Returns") # Spike at lag 2 so p=2

# Plot the PACF of the negative log returns
ggPacf(bitcoin_negative_log_returns, main = "PACF of Negative Log Returns") # Spike at lag 2 so p=2

# Fit an ARIMA(2, 0,2) model to the negative log returns (d=0 because non-stationarity form the ACF and PACF)
arima_fit <- arima(bitcoin_negative_log_returns, order = c(2,0,2))
print(arima_fit)

# Fit an auto.arima model to the negative log returns
auto_arima_fit <- auto.arima(bitcoin_negative_log_returns)
print(auto_arima_fit) # Auto.arima() suggests also an ARIMA(2,0,2) model, so it confirms the choice of the ARIMA model.

### Asses residuals of the ARIMA model

# Extract the residuals
residuals_arima <- residuals(arima_fit)

# ACF of residuals (to check for autocorrelation)
acf(residuals_arima, main = "ACF of ARIMA Model Residuals")

# Ljung-Box test on residuals (to formally test for autocorrelation)
Box.test(residuals_arima, lag = 20, type = "Ljung-Box") # There is no significant autocorrelation in the residuals of the ARIMA model, which means the model has fully captured the patterns in the data (good sign)

# QQ-plot of residuals (to check for normality)
qqnorm(residuals_arima)
qqline(residuals_arima, col = "red") # residuals don't seem to follow a normal distribution

# Shapiro-Wilk test (to test for normality)
shapiro.test(residuals_arima) # p-value < 0.05, so we reject H0, residuals are not normally distributed (not good sign)

# Plot residuals over time (to check for changing variance)
plot(residuals_arima, main = "Residuals of ARIMA Model", ylab = "Residuals", xlab = "Time") # Volatility clustering, so variance is not constant over time (not good sign)

"The residuals of the ARIMA model show no significant autocorrelation, which is a good sign that the model has captured 
the data. However the residuals do not follow a normal distribution, and the variance is not constant over time. This 
suggests that the ARIMA model may not be the best fit for the data. Maybe using a GARCH model could be more appropriate"

### d

### Fitting a GARCH(1,1) model with a normal distribution
garch_normal_fit <- garchFit(~ garch(1, 1), data = bitcoin_negative_log_returns, cond.dist = "norm", trace = FALSE)

# Print the summary of the model
summary(garch_normal_fit)

# Extract residuals from the fitted GARCH model with normal distribution
garch_normal_residuals <- residuals(garch_normal_fit)

# Plot ACF of residuals to check for autocorrelation
acf(garch_normal_residuals, main = "ACF of Residuals (GARCH Normal)")

### It captures most of the autocorrelation, but the spike in lag 1 can suggests that there is some remaining of corr, 
### the model does not capture full of the dependencies.

# Perform Ljung-Box test on residuals
Box.test(garch_normal_residuals, lag = 20, type = "Ljung-Box")
### We reject the hypothesis of no autocorrelation. There is so a bit of autocorr (we see it by the ACF plot with spike in lag 1)

# QQ-Plot to check for normality of residuals
qqnorm(garch_normal_residuals, main = "QQ-Plot of Residuals (GARCH Normal)")
qqline(garch_normal_residuals, col = "red")
### There are deviations in the tails, there are so extreme values. In principle, t-distr will correct it!

shapiro.test(garch_normal_residuals)
### It rejects the hypothesis of the normality for the residuals. So the residuals are not normally distributed.

### Fitting a GARCH(1,1) model with a standardized t-distribution ###
garch_t_fit <- garchFit(~ garch(1, 1), data = bitcoin_negative_log_returns, cond.dist = "std", trace = FALSE)

# Print the summary of the model
summary(garch_t_fit)

# Extract residuals from the fitted GARCH model with t-distribution
garch_t_residuals <- residuals(garch_t_fit)

# Plot ACF of residuals to check for autocorrelation
acf(garch_t_residuals, main = "ACF of Residuals (GARCH t-Distribution)")

# Perform Ljung-Box test on residuals
Box.test(garch_t_residuals, lag = 20, type = "Ljung-Box")

# QQ-Plot to check for normality of residuals
qqnorm(garch_t_residuals, main = "QQ-Plot of Residuals (GARCH t-Distribution)")
qqline(garch_t_residuals, col = "red")

# Shapiro-Wilk test for normality of residuals
shapiro.test(garch_t_residuals)

### The conclusions are the same for all the indicators, because the results are very close.

# Compare the models based on AIC, BIC, or log-likelihood (lower AIC/BIC or higher log-likelihood is better)
cat("AIC (Normal):", garch_normal_fit@fit$ics[1], "\n")
cat("AIC (t-Distribution):", garch_t_fit@fit$ics[1], "\n")

cat("BIC (Normal):", garch_normal_fit@fit$ics[2], "\n")
cat("BIC (t-Distribution):", garch_t_fit@fit$ics[2], "\n")

### The values are really close, but the normal model appears to be better than the t-distr (AIC: -10.60 vs -10.75)

### e

# Assuming `neg_log_returns` is the negative log returns data

# Fit an ARIMA(2,0,2) model on the negative log returns
arima_fit <- arima(bitcoin_negative_log_returns, order = c(2, 0, 2))

# Extract the residuals from the ARIMA model
arima_residuals <- residuals(arima_fit)

# Fit a GARCH(1,1) model on the ARIMA residuals
garch_fit_arima_resid <- garchFit(~ garch(1, 1), data = arima_residuals, trace = FALSE)

# Summary of the GARCH fit
summary(garch_fit_arima_resid)

# Assess the quality of the GARCH(1,1) fit

# Plot ACF of residuals from GARCH fit
garch_residuals <- residuals(garch_fit_arima_resid)
acf(garch_residuals, main = "ACF of Residuals (GARCH on ARIMA Residuals)")
### There is fast no values out of the bounds, which shows the GARCH has captured fast all the volatility in the residuals.

# QQ-plot of residuals
qqnorm(garch_residuals)
qqline(garch_residuals, col = "red", main = "QQ-Plot of Residuals (GARCH on ARIMA Residuals)")
### There are deviations in the extreme, which is a sign of heavy tails.

# Box-Ljung test on GARCH residuals
box_ljung_test <- Box.test(garch_residuals, lag = 20, type = "Ljung-Box")
print(box_ljung_test)
### High p-val, which indicates there is no significant autocorr in the residuals.


# Shapiro-Wilk test for normality of residuals
shapiro_test <- shapiro.test(garch_residuals)
print(shapiro_test)
### The p-val is lower than 0.05, so we reject the null hypothesis of normality. So the residuals are not normally distributed.

### f


"1. ARIMA(2, 0, 2) Model:

    Autocorrelation: The ACF of the residuals showed no significant autocorrelation, which is a good sign that the ARIMA model has captured most of the structure in the data.
    Normality: The QQ-plot and Shapiro-Wilk test indicated that the residuals do not follow a normal distribution. This suggests that the ARIMA model is not capturing the full distributional characteristics of the data, especially the heavy tails or extreme values common in financial data.
    Variance: The residual plot showed signs of volatility clustering, indicating that the variance is not constant (heteroscedasticity), which violates the ARIMA model's assumption of homoscedasticity.
    Conclusion: Although the ARIMA model captured the serial correlation (autocorrelation), it does not handle the non-constant variance well, suggesting that this model is not fully suitable for this data.

2. GARCH(1, 1) with Normal Distribution:

    Autocorrelation: The ACF of the residuals showed that the GARCH(1, 1) model captured most of the autocorrelation, but there was a small spike at lag 1, indicating some remaining serial dependence that was not fully captured.
    Normality: The QQ-plot and Shapiro-Wilk test indicated that the residuals are not normally distributed. The deviations in the tails suggest that the normal distribution is not capturing the heavy tails of the negative log returns, which is common in financial data.
    Variance: The GARCH model allows for heteroscedasticity, meaning that it models the changing variance over time, capturing the volatility clustering in the data.
    Conclusion: The GARCH(1, 1) model with a normal distribution improves over ARIMA in terms of handling volatility, but it still does not adequately account for the heavy tails in the data.

3. GARCH(1, 1) with t-Distribution:

    Autocorrelation: The ACF of the residuals showed that the GARCH(1, 1) model with a t-distribution performed similarly to the GARCH with normal distribution. The Ljung-Box test indicated that some autocorrelation might still be present, but overall, it captures the structure well.
    Normality: The QQ-plot and Shapiro-Wilk test for the t-distribution residuals suggested that while the fit was not perfect, the t-distribution better captures the heavy tails than the normal distribution does.
    Variance: Like the GARCH model with normal distribution, this model captures volatility clustering and accommodates heteroscedasticity well.
    Conclusion: The GARCH(1, 1) model with a t-distribution provides a better fit for the data because it captures the heavy tails more effectively than the normal distribution. This model is generally more suitable for financial data, which often exhibits extreme returns."


"Conclusion on Model Comparison:

    Most Suitable Model: The GARCH(1, 1) model with t-distribution is likely the most suitable for the negative log returns of Bitcoin. It handles both the changing variance (heteroscedasticity) and the heavy tails present in financial returns, making it a more appropriate model for this type of data.

    Homoscedasticity Assumption:
        The ARIMA model clearly violates the homoscedasticity assumption, as it does not model the volatility clustering seen in the data.
        Both GARCH models (with normal and t-distributions) allow for heteroscedasticity and are specifically designed to model the changing variance over time. Hence, in these models, the homoscedasticity assumption is not violated because the models account for the non-constant variance."

################## PART 3 ##################

# 1st: Negative log-return of ETH
# We take the concerned column.
eth_prices <- Crypto_data$Ethereum

# Function to compute negative log returns:
negative_log_returns <- function(prices) {
  log_returns <- diff(log(prices))  # Calculate log returns
  return(-log_returns)  # Return the negative log returns
}

# Apply the function to the ETH prices
eth_negative_log_returns <- negative_log_returns(eth_prices)

# Plot the negative log returns of ETH
plot(eth_negative_log_returns, type = "l", main = "Negative Log Returns of ETH", xlab = "Time", ylab = "Negative Log Returns")

# Perform the ADF test on the negative log returns of ETH
adf_test_eth <- adf.test(eth_negative_log_returns)
print(adf_test_eth)

### a
# Ensure the length of both series is the same by trimming if necessary
min_length <- min(length(bitcoin_negative_log_returns), length(eth_negative_log_returns))
bitcoin_negative_log_returns <- bitcoin_negative_log_returns[1:min_length]
eth_negative_log_returns <- eth_negative_log_returns[1:min_length]

# Perform the correlation test between Bitcoin and ETH negative log returns
correlation_test <- cor.test(bitcoin_negative_log_returns, eth_negative_log_returns)

# Print the result of the correlation test
print(correlation_test)
# The p-value is greater than 0.05(= 0.905), you cannot reject the null hypothesis, meaning the series might be independent.
# So the Bitcoin & Ethereum are apparently not correlated, so these 2 series are independent.

### b
# Calculate the Cross-Correlation Function (CCF)
ccf_result <- ccf(bitcoin_negative_log_returns, eth_negative_log_returns, plot=TRUE)

# Print the CCF result (if needed)
print(ccf_result)
# No corr at lag 0, so the 2 series are independent in the beginning.
# There is a notable spike at lag -5, so change in ETH log return precedes change in BTC from around 5 periods. 
# Largely above the IC, so statistically significant.
# This pattern indicates some degree of dependency between the 2 series, ETH potentially driving BTC at certain points.

### c
# Granger causality test for Bitcoin predicting ETH
grangertest(eth_negative_log_returns ~ bitcoin_negative_log_returns, order = 10)
# The first test gives a p-val very small (0.001<<), so we reject the null hypothesis.
# There is predictive power in Bitcoin's returns for forecasting Ethereum's returns.

# Granger causality test for ETH predicting Bitcoin
grangertest(bitcoin_negative_log_returns ~ eth_negative_log_returns, order = 10)
# The p-val is large (0.81), so we cannot reject the null hypothesis.
# So Ethereum's past returns do not have predictive power for Bitcoin's future returns.

### d
### 1:
# We can reasonably expect that Ethereum will also experience a negative impact shortly after. 
# This is because the causality test suggests that Bitcoin's price movements tend to influence Ethereum's price movements.
# one should expect Ethereum prices to also experience a downturn in the near future based on the observed predictive relationship between their returns.

### 2: 
# We cannot conclude that Bitcoin will be similarly affected.
# The lack of Granger causality from Ethereum to Bitcoin suggests that Ethereum's price movements do not have a significant predictive power over Bitcoin's movements. 
# Therefore, Bitcoin might not experience a similar drop just because Ethereum does.
# Bitcoin may remain unaffected, or its movement could depend on other market forces, not directly on Ethereum's price action.
