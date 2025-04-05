# Load ggplot2 for visualizations
library(ggplot2)
# Reshape data into long format for ggplot
contact_long <- contact %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Time", 
               values_to = "Mortality")
# Calculate mean mortality for each treatment at each time point
contact_summary <- contact_long %>%
  group_by(Treatements, Time) %>%
  summarise(Mean_Mortality = mean(Mortality, na.rm = TRUE))
# Convert Time to an ordered factor
time_order <- c("X30.MIN", "X1.HR", "X2.HR", "X3.HR", "X6.HR", "X24.HR")
contact_summary$Time <- factor(contact_summary$Time, levels = time_order, ordered = TRUE)
# Line plot for trends over time
ggplot(contact_summary, aes(x = Time, y = Mean_Mortality, color = Treatements, group = Treatements)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Mortality Trends Over Time for Contact Application Across Treatments",
       x = "Time",
       y = "Mean Mortality (%)") +
  scale_x_discrete(labels = c("30 Minutes", "1 Hour", "2 Hours", "3 Hours", "6 Hours", "24 Hours")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Reshape data into long format for trendline
fumigant_long <- fumigant %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Time", 
               values_to = "Mortality")
# Calculate mean mortality for each treatment at each time point
fumigant_summary <- fumigant_long %>%
  group_by(Treatements, Time) %>%
  summarise(Mean_Mortality = mean(Mortality, na.rm = TRUE))
# Convert Time to an ordered factor
time_order <- c("X30.MIN", "X1.HR", "X2.HR", "X3.HR", "X6.HR", "X24.HR")
fumigant_summary$Time <- factor(contact_summary$Time, levels = time_order, ordered = TRUE)
# Line plot for trends over time
ggplot(fumigant_summary, aes(x = Time, y = Mean_Mortality, color = Treatements, group = Treatements)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Mortality Trends Over Time for Fumigant Application Across Treatments",
       x = "Time",
       y = "Mean Mortality (%)") +
  scale_x_discrete(labels = c("30 Minutes", "1 Hour", "2 Hours", "3 Hours", "6 Hours", "24 Hours")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot survival curves
ggsurvplot(surv_fit,
           data = combined_long,
           pval = TRUE,  # Add p-value from log-rank test
           conf.int = FALSE,  # Remove confidence intervals
           palette = c("#E7B800", "#2E9FDF"),  # Custom colors
           legend.labs = c("Contact", "Fumigant"),  # Legend labels
           xlab = "Time (Hours)",  # X-axis label
           ylab = "Survival Probability",  # Y-axis label
           title = "Comparative Survival Analysis: Contact vs. Fumigant Treatments")  # Plot title


