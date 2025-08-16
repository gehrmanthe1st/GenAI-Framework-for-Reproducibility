setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_13')

# 1. import_libraries: Load required R packages
library(dplyr)    # Data manipulation and summary statistics.
library(ggplot2)  # Creating bar plots with error bars.
library(readr)    # Reading CSV files.
library(car)      # Conducting ANOVA assumption checks and effect size calculations.
library(emmeans)  # Performing post hoc pairwise comparisons with Sidak adjustments.
library(effsize)  # Computing Cohen's d for effect size.
library(broom)    # Tidying model outputs into summary tables.

# 2. read_data: Import the dataset and ensure all columns are imported, including OVERALL.
df <- read_csv("dataset.csv")

# 3. clean_and_prepare: Convert 'Age (years)' to numeric, recode 'Age Group' as a factor with specified levels, and subset columns.
df <- df %>%
  mutate(`Age (years)` = as.numeric(`Age (years)`),
         `Age Group` = factor(`Age Group`, levels = c("3-4 year olds", "5-6 year olds", "7-10 year olds", "Adults"))) %>%
  select(`Participant ID`, `Age (years)`, `Age Group`, NICE, STRONG, SMART, OVERALL)

# 4. calculate_descriptives:
# Compute overall means for NICE, STRONG, and SMART across all participants
overall_means <- df %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            mean_STRONG = mean(STRONG, na.rm = TRUE),
            mean_SMART = mean(SMART, na.rm = TRUE))
print("Overall Means for Traits:")
print(overall_means)

# Calculate mean consensus scores grouped by Age Group for each trait
group_means <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            mean_STRONG = mean(STRONG, na.rm = TRUE),
            mean_SMART = mean(SMART, na.rm = TRUE),
            mean_OVERALL = mean(OVERALL, na.rm = TRUE))
print("Mean Consensus by Age Group:")
print(group_means)

# 5. test_consensus_vs_chance: Perform one-sample t-tests for NICE, STRONG, and SMART against chance level (mu=0.5)
t_test_NICE <- t.test(df$NICE, mu = 0.5)
t_test_STRONG <- t.test(df$STRONG, mu = 0.5)
t_test_SMART <- t.test(df$SMART, mu = 0.5)
print("T-test results for NICE:")
print(tidy(t_test_NICE))
print("T-test results for STRONG:")
print(tidy(t_test_STRONG))
print("T-test results for SMART:")
print(tidy(t_test_SMART))

# 6. calculate_effect_sizes_consensus: Compute Cohen's d for each one-sample t-test with mu = 0.5
cohen_NICE <- cohen.d(df$NICE, f= NA, mu = 0.5)
cohen_STRONG <- cohen.d(df$STRONG, f= NA, mu = 0.5)
cohen_SMART <- cohen.d(df$SMART, f= NA, mu = 0.5)
print("Cohen's d for NICE:")
print(cohen_NICE)
print("Cohen's d for STRONG:")
print(cohen_STRONG)
print("Cohen's d for SMART:")
print(cohen_SMART)

# 7. run_anova_analyses: Perform separate one-way ANOVAs for NICE, STRONG, SMART, and OVERALL across Age Group
anova_NICE <- aov(NICE ~ `Age Group`, data = df)
anova_STRONG <- aov(STRONG ~ `Age Group`, data = df)
anova_SMART <- aov(SMART ~ `Age Group`, data = df)
anova_OVERALL <- aov(OVERALL ~ `Age Group`, data = df)
print("ANOVA results for NICE:")
print(summary(anova_NICE))
print("ANOVA results for STRONG:")
print(summary(anova_STRONG))
print("ANOVA results for SMART:")
print(summary(anova_SMART))
print("ANOVA results for OVERALL:")
print(summary(anova_OVERALL))

# 8. conduct_posthoc_tests: Run post hoc comparisons for NICE and STRONG using emmeans with pairwise comparisons and Sidak adjustment.
# For NICE:
emmeans_NICE <- emmeans(anova_NICE, "Age Group")
posthoc_NICE <- pairs(emmeans_NICE, adjust = "sidak")
print("Post hoc comparisons for NICE:")
print(posthoc_NICE)

# For STRONG:
emmeans_STRONG <- emmeans(anova_STRONG, "Age Group")
posthoc_STRONG <- pairs(emmeans_STRONG, adjust = "sidak")
print("Post hoc comparisons for STRONG:")
print(posthoc_STRONG)

# 9. compile_statistical_results: Create a summary table
# Print combined overall means expected from the article reference:
expected_means <- data.frame(Trait = c("NICE", "STRONG", "SMART"),
                             Combined_Mean = c(0.93, 0.85, 0.76))
print("Expected Combined Overall Means for Traits (as per article):")
print(expected_means)
# Print t-test and effect size results have been printed above.
# Print group means from ANOVA (already stored in group_means).
# Print post hoc results (already printed above).

# 10. create_figure_2: Generate bar plot for NICE trait by Age Group with error bars (±1 SEM)
sem <- function(x) { sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))) }
plot_data_NICE <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            sem_NICE = sem(NICE))
fig2 <- ggplot(plot_data_NICE, aes(x = `Age Group`, y = mean_NICE*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_NICE - sem_NICE)*100, ymax = (mean_NICE + sem_NICE)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (NICE)", x = "Age Group") +
  ggtitle("Fig. 2: NICE Trait Consensus by Age Group")
print(fig2)

# 11. create_figure_3: Generate bar plot for STRONG trait by Age Group with error bars (±1 SEM)
plot_data_STRONG <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_STRONG = mean(STRONG, na.rm = TRUE),
            sem_STRONG = sem(STRONG))
fig3 <- ggplot(plot_data_STRONG, aes(x = `Age Group`, y = mean_STRONG*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_STRONG - sem_STRONG)*100, ymax = (mean_STRONG + sem_STRONG)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (STRONG)", x = "Age Group") +
  ggtitle("Fig. 3: STRONG Trait Consensus by Age Group")
print(fig3)

# 12. create_figure_4: Generate bar plot for SMART trait by Age Group with error bars (±1 SEM)
plot_data_SMART <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_SMART = mean(SMART, na.rm = TRUE),
            sem_SMART = sem(SMART))
fig4 <- ggplot(plot_data_SMART, aes(x = `Age Group`, y = mean_SMART*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_SMART - sem_SMART)*100, ymax = (mean_SMART + sem_SMART)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (SMART)", x = "Age Group") +
  ggtitle("Fig. 4: SMART Trait Consensus by Age Group")
print(fig4)
