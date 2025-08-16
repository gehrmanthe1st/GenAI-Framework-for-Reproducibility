setwd("C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_4")

# Step 1. import_libraries
library(dplyr)     # Data manipulation and summarization
library(tidyr)     # Data reshaping and pivoting
library(afex)      # Mixed ANOVA models
library(emmeans)   # Post-hoc pairwise comparisons
library(car)       # Levene's test for homogeneity of variance
library(ggplot2)   # Plotting for interaction graph (Fig. 2)

# Step 2. read_data
data <- read.csv('dataset_to_use.csv', header = TRUE, stringsAsFactors = FALSE)
# The dataset contains: group, participant, subscale, item, Agreement_scale

# Step 3. clean_and_recode
data$group <- factor(data$group, levels = c('young Spaniard', 'Moroccan'))
data$participant <- factor(data$participant)
data$subscale <- factor(data$subscale, levels = c('PAST', 'FUTURE'))

# Step 4. aggregate_ratings
aggregated_data <- data %>%
  group_by(group, participant, subscale) %>%
  summarize(mean_agreement = mean(Agreement_scale, na.rm = TRUE), .groups = 'drop')

# Step 5. compute_descriptives
desc_stats <- aggregated_data %>%
  group_by(group, subscale) %>%
  summarize(mean_rating = mean(mean_agreement, na.rm = TRUE),
            sd_rating = sd(mean_agreement, na.rm = TRUE),
            n = n(),
            .groups = 'drop')
print("Descriptive Statistics by group and subscale:")
print(desc_stats)

# Step 6. check_assumptions
# Fit an initial mixed-model to obtain residuals for assumption testing.
initial_model <- afex::aov_car(mean_agreement ~ subscale * group + Error(participant/subscale), 
                               data = aggregated_data, factorize = FALSE)
model_residuals <- residuals(initial_model)
shapiro_test <- shapiro.test(model_residuals)
print("Shapiro-Wilk test for normality of residuals:")
print(shapiro_test)

# Check homogeneity of variance using Levene's Test
levene <- car::leveneTest(mean_agreement ~ group * subscale, data = aggregated_data)
print("Levene's Test for homogeneity of variance:")
print(levene)

# Step 7. run_mixed_anova
anova_model <- afex::aov_car(mean_agreement ~ subscale * group + Error(participant/subscale), 
                             data = aggregated_data, factorize = FALSE)
print("Mixed ANOVA results:")
print(summary(anova_model))

# Step 8. extract_anova_results
anova_table <- anova_model$anova_table
interaction_row <- anova_table[rownames(anova_table) == "subscale:group", ]
print("Interaction effect (mixed ANOVA) results:")
print(interaction_row)

# Step 9. conduct_post_hoc_tests
# Obtain estimated marginal means for the interaction
emm <- emmeans::emmeans(anova_model, ~ group | subscale)

# Get pairwise comparisons within each subscale level
post_hoc_results <- pairs(emm, adjust = "none")
post_hoc_summary <- summary(post_hoc_results)

print("Post-hoc t-tests comparing groups within each subscale:")
print(post_hoc_summary)

# Step 10. compile_final_results
cat("FINAL RESULTS:\n")
cat("Mixed ANOVA interaction effect (subscale*group):\n")
cat(sprintf("F(%s, %s) = %.2f, p = %.3f\n", 
            interaction_row["num Df"], interaction_row["den Df"], interaction_row["F value"], interaction_row["Pr(>F)"]))
cat("Calculated partial eta-squared (ηp²) from the model output (if provided) should be reported.\n")
cat("\nPost-hoc comparisons:\n")
print(post_hoc_summary)

# Optional: Generate an interaction plot (Fig. 2)
plot_data <- aggregated_data %>%
  group_by(group, subscale) %>%
  summarize(mean_agreement = mean(mean_agreement, na.rm = TRUE), .groups = 'drop')

interaction_plot <- ggplot(plot_data, aes(x = subscale, y = mean_agreement, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(title = "Interaction Plot: Temporal Focus by Group",
       x = "Temporal Focus (subscale)",
       y = "Mean Agreement Rating") +
  theme_minimal()
print(interaction_plot)