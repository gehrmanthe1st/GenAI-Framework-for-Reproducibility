setwd("C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_4")

library(dplyr)     # Data manipulation and summarization
library(tidyr)     # Data reshaping and pivoting
library(afex)      # Mixed ANOVA models
library(emmeans)   # Post-hoc pairwise comparisons
library(car)       # Levene's test for homogeneity of variance
library(ggplot2)   # Plotting for interaction graph (Fig. 2)

# Step 2. read_data
data <- read.csv('data.csv', header = TRUE, stringsAsFactors = FALSE)
# The dataset contains: group, participant, subscale, item, Agreement_scale

# Step 3. clean_and_recode
data$group <- factor(data$group, levels = c('young Spaniard', 'Moroccan'))
data$participant <- factor(data$participant)
data$subscale <- factor(data$subscale, levels = c('PAST', 'FUTURE'))

# Step 4. aggregate_ratings
aggregated_data <- data %>%
  group_by(group, participant, subscale) %>%
  summarize(mean_agreement = mean(Agreement_scale), .groups = 'drop')

# Step 5. compute_descriptives
desc_stats <- aggregated_data %>%
  group_by(group, subscale) %>%
  summarize(mean_rating = mean(mean_agreement),
            sd_rating = sd(mean_agreement),
            n = n(),
            .groups = 'drop')
print("Descriptive Statistics by group and subscale:")
print(desc_stats)

# Step 6. check_assumptions
# Fit an initial mixed-model to obtain residuals for assumption testing.
# Using afex, the model handles within-subject error structure.
initial_model <- afex::aov_car(mean_agreement ~ subscale * group + Error(participant/subscale), 
                               data = aggregated_data, factorize = FALSE)
# Extract residuals for the overall model, note: This is a simplified check.
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
# Expected reported results: F(1,78) with corresponding p-value and partial eta-squared (ηp²)

# Step 9. conduct_post_hoc_tests
# Obtain estimated marginal means
emm <- emmeans::emmeans(anova_model, ~ subscale * group)
# Perform pairwise comparisons for each subscale:
# For past-focused (subscale "PAST")
contrast_past <- pairs(emmeans::contrast(emm, interaction = "pairwise"), adjust = "none") %>%
  summary() %>%
  filter(contrast %in% c("groupyoung Spaniard - groupMoroccan"))
print("Post-hoc t-test for past-focused statements:")
print(contrast_past)

# For future-focused (subscale "FUTURE")
# Restrict the estimated marginal means to FUTURE level and compare groups
emm_future <- emmeans::emmeans(anova_model, ~ group, at = list(subscale = "FUTURE"))
contrast_future <- pairs(emm_future, adjust = "none") %>% summary()
print("Post-hoc t-test for future-focused statements:")
print(contrast_future)

# Step 10. compile_final_results
cat("FINAL RESULTS:\n")
cat("Mixed ANOVA interaction effect (subscale*group):\n")
cat(sprintf("F(%s, %s) = %.2f, p = %.3f\n", 
            interaction_row["num Df"], interaction_row["den Df"], interaction_row["F value"], interaction_row["Pr(>F)"]))
cat("Calculated partial eta-squared (ηp²) from the model output (if provided) should be reported.\n")
cat("\nPost-hoc comparisons:\n")
cat("Past-focused statements: Report the t-test statistic and p-value comparing young Spaniards and Moroccans.\n")
print(contrast_past)
cat("Future-focused statements: Report the t-test statistic and p-value comparing young Spaniards and Moroccans.\n")
print(contrast_future)

# Optional: Generate an interaction plot (Fig. 2)
# Create a summary dataframe for plotting
plot_data <- aggregated_data %>%
  group_by(group, subscale) %>%
  summarize(mean_agreement = mean(mean_agreement), .groups = 'drop')

interaction_plot <- ggplot(plot_data, aes(x = subscale, y = mean_agreement, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(title = "Interaction Plot: Temporal Focus by Group",
       x = "Temporal Focus (subscale)",
       y = "Mean Agreement Rating") +
  theme_minimal()
print(interaction_plot)
# Uncomment the next line to save the figure if needed
# ggsave("Fig2_interaction_plot.png", plot = interaction_plot)