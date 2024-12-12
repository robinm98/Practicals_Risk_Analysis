### EDA - Practical 3 ###
#########################

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
suicide_attacks <- read.csv("Practical 3/Data/suicide_attacks_cleaned.csv")
attach(suicide_attacks)
View(suicide_attacks)

# Ensure the "date" column is properly formatted as a Date object
suicide_attacks <- suicide_attacks %>%
  mutate(date = as.Date(paste(date.year, date.month, date.day, sep = "-")))

#####################################################################################

### Summarise variable of interest ###
######################################

### Wounded
summary(wounded)

### Killed
summary(killed)

### Total Causalities
summary(total_casualties)

#####################################################################################

### Histogram plots ###
#######################

### Group by groups

# Top 10 groups with the highest total casualties
top_groups <- suicide_attacks %>%
  group_by(group) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Histogram for top 10 groups
ggplot(top_groups, aes(x = reorder(group, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Groups by Total Casualties",
    x = "Groups",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by country

# Top 10 countries with the highest total casualties
top_countries <- suicide_attacks %>%
  group_by(target.country) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Histogram for top 10 countries
ggplot(top_countries, aes(x = reorder(target.country, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Total Casualties",
    x = "Countries",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by target nationality

# Top 10 target nationalities with the highest total casualties
top_nationality <- suicide_attacks %>%
  group_by(target.nationality) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Histogram for top 10 nationalities
ggplot(top_nationality, aes(x = reorder(target.nationality, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Target Nationality by Total Casualties",
    x = "Nationality",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by region

# Regions total casualties
top_region <- suicide_attacks %>%
  group_by(target.region) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) 

# Histogram for regions
ggplot(top_region, aes(x = reorder(target.region, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Regions by Total Casualties",
    x = "Region",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by target city

# Top 10 cities with the highest total casualties
top_city <- suicide_attacks %>%
  group_by(target.city) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Histogram for top 10 cities
ggplot(top_city, aes(x = reorder(target.city, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Target Cities by Total Casualties",
    x = "City",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by target type

# Top type with the highest total casualties
top_type <- suicide_attacks %>%
  group_by(target.type) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Histogram for target type
ggplot(top_type, aes(x = reorder(target.type, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Target Types by Total Casualties",
    x = "Types",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Group by target weapon

## Casualties
# Summarise total casualties for each weapon type
weapon_casualties <- suicide_attacks %>%
  group_by(target.weapon) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties))

# Bar chart for weapon types by total casualties
ggplot(weapon_casualties, aes(x = reorder(target.weapon, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total Casualties by Weapon Type",
    x = "Weapon Types",
    y = "Total Casualties"
  ) +
  theme_minimal()

## Distribution
# Count occurrences of each weapon type in target.weapon
weapon_distribution <- suicide_attacks %>%
  group_by(target.weapon) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Bar chart for weapon types
ggplot(weapon_distribution, aes(x = reorder(target.weapon, -count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Distribution of Weapon Types",
    x = "Weapon Types",
    y = "Count"
  ) +
  theme_minimal()

### Group by number of attackers

## Casualties
# Summarise total casualties for each number of attackers
attackers_casualties <- suicide_attacks %>%
  group_by(attackers) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Bar chart for attacker numbers by total casualties
ggplot(attackers_casualties, aes(x = reorder(attackers, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total Casualties by Number of Attackers",
    x = "Number of Attackers",
    y = "Total Casualties"
  ) +
  theme_minimal()

## Distribution
# Count occurrences of number of attackers
attackers_distribution <- suicide_attacks %>%
  group_by(attackers) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)


# Bar chart for attacker types
ggplot(attackers_distribution, aes(x = reorder(attackers, -count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Distribution of Attackers' Number",
    x = "Attacker Types",
    y = "Count"
  ) +
  theme_minimal()

### Group by date

## Casualties
# Calculate total casualties for each year and select the top 10
top_dates_casualties <- suicide_attacks %>%
  group_by(date.year) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties)) %>%
  slice_head(n = 10)

# Bar chart for the top 10 dates with highest casualties
ggplot(top_dates_casualties, aes(x = reorder(date.year, -total_casualties), y = total_casualties)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Dates with Highest Total Casualties",
    x = "Dates",
    y = "Total Casualties"
  ) +
  theme_minimal()

## Distributions
# Count occurrences for each year
top_dates <- suicide_attacks %>%
  group_by(date.year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Bar chart for the top 10 dates
ggplot(top_dates, aes(x = reorder(date.year, -count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Years with Highest Occurrences",
    x = "Dates",
    y = "Count"
  ) +
  theme_minimal()

### Group by Year

# Summarize total casualties and distribution by year
yearly_data <- suicide_attacks %>%
  group_by(date.year) %>%
  summarise(
    total_casualties = sum(total_casualties, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(total_casualties))

## Casualties
# Bar chart for total casualties by year
ggplot(yearly_data, aes(x = date.year, y = total_casualties)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Casualties by Year",
    x = "Year",
    y = "Total Casualties"
  ) +
  theme_minimal()

## Distribution
# Bar chart for distribution (number of incidents) by year
ggplot(yearly_data, aes(x = date.year, y = count)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Incidents by Year",
    x = "Year",
    y = "Number of Incidents"
  ) +
  theme_minimal()

### Group by months

# Summarize total casualties and distribution by month
monthly_data <- suicide_attacks %>%
  group_by(date.month) %>%
  summarise(
    total_casualties = sum(total_casualties, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(total_casualties))

## Casualties
# Bar chart for total casualties by month
ggplot(monthly_data, aes(x = factor(date.month, levels = 1:12), y = total_casualties)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Casualties by Month",
    x = "Month",
    y = "Total Casualties"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

## Distribution
# Bar chart for distribution (number of incidents) by month
ggplot(monthly_data, aes(x = factor(date.month, levels = 1:12), y = count)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Incidents by Month",
    x = "Month",
    y = "Number of Incidents"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

#####################################################################################

### Line plots ###
##################

### Casualties over time
ggplot(data = suicide_attacks, aes(x = date, y = total_casualties)) +
  geom_line(color = "blue") +
  labs(title = "Casualties Over Time",
       x = "Date",
       y = "Casualties") +
  theme_minimal()

### Casualties over time without 9/11
# Filter out the data for 09.11.2001
suicide_attacks_filtered <- suicide_attacks %>%
  filter(date != as.Date("2001-09-02"))

# Plot fatalities over time
ggplot(data = suicide_attacks_filtered, aes(x = date, y = total_casualties)) +
  geom_line(color = "blue") +
  labs(title = "Fatalities Over Time (Excluding 09.11.2001)",
       x = "Date",
       y = "Fatalities (High Estimate)") +
  theme_minimal()

### Log(causalties) over time
ggplot(data = suicide_attacks, aes(x = date, y = log(total_casualties))) +
  geom_line(color = "blue") +
  labs(title = "Casualties Over Time",
       x = "Date",
       y = "Casualties") +
  theme_minimal()

### Yearly trend
ggplot(yearly_data, aes(x = date.year, y = total_casualties)) +
  geom_line(group = 1, color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Total Casualties Over the Years",
    x = "Year",
    y = "Total Casualties"
  ) +
  theme_minimal()

### Monthly trend
ggplot(monthly_data, aes(x = factor(date.month, levels = 1:12), y = total_casualties)) +
  geom_line(group = 1, color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Total Casualties Over the Months",
    x = "Month",
    y = "Total Casualties"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

### Seasonal Trends
seasonal_trend <- suicide_attacks %>%
  group_by(date.year, date.month) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE), .groups = "drop")

ggplot(seasonal_trend, aes(x = factor(date.month, levels = 1:12), y = total_casualties, color = as.factor(date.year), group = date.year)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Casualties Over the Years",
    x = "Month",
    y = "Total Casualties",
    color = "Year"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

## Top groups over time
top_groups_trend <- suicide_attacks %>%
  filter(group %in% top_groups$group) %>%
  group_by(date.year, group) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE), .groups = "drop")

ggplot(top_groups_trend, aes(x = date.year, y = total_casualties, color = group)) +
  geom_line(size = 1) +
  labs(
    title = "Trends in Casualties for Top Groups Over Time",
    x = "Year",
    y = "Total Casualties",
    color = "Groups"
  ) +
  theme_minimal()

### Top countries over time
top_countries_trend <- suicide_attacks %>%
  filter(target.country %in% top_countries$target.country) %>%
  group_by(date.year, target.country) %>%
  summarise(total_casualties = sum(total_casualties, na.rm = TRUE), .groups = "drop")

ggplot(top_countries_trend, aes(x = date.year, y = total_casualties, color = target.country)) +
  geom_line(size = 1) +
  labs(
    title = "Trends in Casualties for Top Countries Over Time",
    x = "Year",
    y = "Total Casualties",
    color = "Country"
  ) +
  theme_minimal()
