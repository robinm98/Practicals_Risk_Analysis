### EDA - Practical 3 ###
#########################

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
suicide_attacks <- read.csv("Practical 3/Data/suicide_attacks_cleaned.csv")

# Ensure the "date" column is properly formatted as a Date object
suicide_attacks <- suicide_attacks %>%
  mutate(date = as.Date(paste(date.year, date.month, date.day, sep = "-")))


##### Plot fatalities over time
ggplot(data = suicide_attacks, aes(x = date, y = statistics...killed_high)) +
  geom_line(color = "blue") +
  labs(title = "Fatalities Over Time",
       x = "Date",
       y = "Fatalities (High Estimate)") +
  theme_minimal()

#### Plot fatalities over time without 9/11
# Filter out the data for 09.11.2001
suicide_attacks_filtered <- suicide_attacks %>%
  filter(date != as.Date("2001-09-02"))

# Plot fatalities over time
ggplot(data = suicide_attacks_filtered, aes(x = date, y = statistics...killed_high)) +
  geom_line(color = "blue") +
  labs(title = "Fatalities Over Time (Excluding 09.11.2001)",
       x = "Date",
       y = "Fatalities (High Estimate)") +
  theme_minimal()
