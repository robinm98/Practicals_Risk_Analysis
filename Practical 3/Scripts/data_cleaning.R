### Data Cleaning - Practical 3 ###
###################################

# Load the required libraries
library(dplyr)
library(stringr)

# Load the data
suicide_attacks <- read.csv("Practical 3/Data/suicide_attacks.csv")

# Remove unnecessary columns
columns_to_remove <- c(
  "status", "attacker.gender", "statistics.sources", "statistics...weapon_oth",
  "statistics...weapon_unk", "claim", "statistics...wounded_low", 
  "statistics...killed_low", "statistics...killed_high_civilian", 
  "statistics...killed_low_civilian", "statistics...killed_low_political", 
  "statistics...killed_high_political", "statistics...killed_low_security", 
  "statistics...killed_high_security", "statistics...female_attackers", 
  "statistics...male_attackers", "statistics...unknown_attackers",
  "statistics...belt_bomb", "statistics...truck_bomb", "statistics...car_bomb"
)

suicide_attacks <- suicide_attacks %>%
  select(-all_of(columns_to_remove))

# Remove rows with negative values in all numeric columns except target.latitude and target.longitude
columns_to_exclude <- c("target.latitude", "target.longtitude")
suicide_attacks <- suicide_attacks %>%
  filter(across(
    .cols = where(is.numeric) & !all_of(columns_to_exclude), 
    .fns = ~ . >= 0
  ))

# Remove duplicate rows
suicide_attacks <- distinct(suicide_attacks)

# Combine rows with the same values in all columns except "groups"
suicide_attacks <- suicide_attacks %>%
  group_by(across(-groups)) %>%
  summarise(
    groups_combined = paste(unique(groups), collapse = ", "), 
    .groups = "drop"
  ) %>%
  mutate(groups_combined = sub(",.*", "", groups_combined)) %>%
  relocate(groups_combined)

# Create the "date" column by merging "date.year", "date.month", and "date.day"
suicide_attacks <- suicide_attacks %>%
  mutate(date = as.Date(paste(date.year, date.month, date.day, sep = "-"))) %>%
  relocate(date, .before = date.year)

# Rename columns by removing specific prefixes
prefixes_to_remove <- c("^statistics\\.\\.\\.")
colnames(suicide_attacks) <- colnames(suicide_attacks) %>%
  str_remove(paste(prefixes_to_remove, collapse = "|"))

# Rename columns, create a new column, and relocate it
suicide_attacks <- suicide_attacks %>%
  rename(
    killed = killed_high,
    wounded = wounded_high,
    group = groups_combined
  ) %>%
  mutate(
    total_casualties = killed + wounded
  ) %>%
  relocate(total_casualties, .before = wounded)

View(suicide_attacks)

# Save the cleaned dataset
write.csv(suicide_attacks, "Practical 3/Data/suicide_attacks_cleaned.csv", row.names = FALSE)
