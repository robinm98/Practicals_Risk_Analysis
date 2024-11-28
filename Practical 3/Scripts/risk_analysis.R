### Risk Analysis - Practical 3 ###
###################################

# Load the required libraries
library(tseries)
library(dplyr)
library(tidyr)
library(MASS)

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

