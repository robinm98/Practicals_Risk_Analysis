### Data Cleaning - Practical 3 ###
###################################

# Load the required libraries
library(dplyr)
library(ggplot2)

# Load the data
suicide_attacks <- read.csv("Practical 3/Data/suicide_attacks.csv")

# Remove the "attacker.gender" column
suicide_attacks <- suicide_attacks %>%
  select(-c(attacker.gender, claim, statistics...wounded_low, statistics...killed_low, statistics...killed_low_civilian, statistics...killed_low_political, statistics...killed_low_security, statistics...female_attackers, statistics...male_attackers, statistics...unknown_attackers))

# Remove duplicate rows
suicide_attacks <- unique(suicide_attacks)

# Combine rows with the same values in all columns except "groups"
suicide_attacks <- suicide_attacks %>%
  group_by(across(-groups)) %>%              # Group by all columns except "groups"
  summarise(groups_combined = paste(unique(groups), collapse = ", "), .groups = "drop") %>% # Combine unique "groups"
  mutate(groups_combined = sub(",.*", "", groups_combined)) %>%  # Keep only the first group (before the comma)
  select(groups_combined, everything())      # Move "groups_combined" to the first column

# Create the "date" column by merging "date.year", "date.month", and "date.day"
suicide_attacks <- suicide_attacks %>%
  mutate(date = as.Date(paste(date.year, date.month, date.day, sep = "-")))

# Reorder columns to place "date" before "date.year"
suicide_attacks <- suicide_attacks %>%
  select(1:(which(names(.) == "date.year") - 1), 
         date, 
         date.year:last_col())

View(suicide_attacks)

