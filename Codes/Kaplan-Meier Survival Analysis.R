# Load necessary libraries
library(tidyverse)
library(survival)
library(survminer)

# Add a column to distinguish between contact and fumigant
contact$Mode <- "Contact"
fumigant$Mode <- "Fumigant"

# Combine the datasets
combined_data <- bind_rows(contact, fumigant)

# Reshape data into long format
combined_long <- combined_data %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Time", 
               values_to = "Mortality")

# Convert Time to numeric (extract numbers from time points)
combined_long <- combined_long %>%
  mutate(Time = as.numeric(gsub("[^0-9]", "", Time)))  # Extract numbers (e.g., "X30.MIN" -> 30)

# Define event (1 = mortality, 0 = censored)
combined_long <- combined_long %>%
  mutate(Event = ifelse(Mortality > 0, 1, 0))  # Assume mortality > 0% indicates an event

# Fit survival curves for each mode of application
surv_fit <- survfit(Surv(Time, Event) ~ Mode, data = combined_long)

# Compare survival curves using the log-rank test
surv_diff <- survdiff(Surv(Time, Event) ~ Mode, data = combined_long)
print(surv_diff)  # Log-rank test results

