# Load necessary libraries
library(tidyverse)
library(drda)  # For dose-response analysis

# Load the combined dataset
combined_data <- bind_rows(contact, fumigant)

# Convert Mode to a factor
combined_data$Mode <- as.factor(combined_data$Mode)

# Check the structure of the data
str(combined_data)
glimpse(combined_data)

# Reshape data into long format
combined_long <- combined_data %>%
  pivot_longer(cols = c("X30.MIN", "X1.HR", "X2.HR", "X3.HR", "X6.HR", "X24.HR"), 
               names_to = "Time", 
               values_to = "Mortality")

# Convert Time to numeric (extract numbers from time points)
combined_long <- combined_long %>%
  mutate(Time = as.numeric(gsub("[^0-9]", "", Time)))  # Extract numbers (e.g., "30.MIN" -> 30)

# Define event (1 = mortality, 0 = censored)
combined_long <- combined_long %>%
  mutate(Event = ifelse(Mortality > 0, 1, 0))  # Corrected: Removed extra parenthesis

# View the final reshaped data
head(combined_long)

# Fit dose-response models for each treatment or mode of application
lt50_results <- combined_long %>%
  group_by(Treatements, Mode) %>%
  do(
    model = drda(Mortality ~ Time, data = .)  # Fit dose-response model using drda
  ) %>%
  mutate(
    LT50 = effective_dose(model, 0.5)  # Calculate LT50 (50% mortality)
  )

# View LT50 results
print(lt50_results)

# Fit dose-response models and calculate LT50 with confidence intervals
lt50_results <- combined_long %>%
  group_by(Treatements, Mode) %>%
  do(
    model = drda(Mortality ~ Time, data = .)  # Fit dose-response model using drda
  ) %>%
  mutate(
    LT50 = effective_dose(model, 0.5, interval = "confidence")[[1]],  # LT50 estimate
    LT50_Lower = effective_dose(model, 0.5, interval = "confidence")[[2]],  # Lower CI
    LT50_Upper = effective_dose(model, 0.5, interval = "confidence")[[3]]   # Upper CI
  )

# View LT50 results with confidence intervals
print(lt50_results)

# Define a better color palette
better_colors <- c("Contact" = "#1F77B4", "Fumigant" = "#FF7F0E")

# Plot LT50 with error bars (bar chart)
ggplot(lt50_results, aes(x = Treatements, y = LT50, fill = Mode)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = LT50_Lower, ymax = LT50_Upper), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "LT50 by Treatment and Mode of Application",
       x = "Treatment",
       y = "LT50 (Hours)",
       fill = "Mode of Application") +
  scale_fill_manual(values = better_colors) +  # Use better colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))