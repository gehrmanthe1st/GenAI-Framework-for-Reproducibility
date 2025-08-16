('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_9')

# Step 0: import_libraries – Load required packages
library(dplyr)    # Data manipulation functions
library(tidyr)    # Data reshaping (pivot_longer)
library(car)      # Effect size calculation (etaSquared)
library(effectsize)

# Step 1: read_data – Import dataset with proper data type control
data <- read.csv("dataset.csv", stringsAsFactors = FALSE)

# Step 2: clean_and_recode – Recode variables for analysis
data <- data %>%
  mutate(control_group = factor(ifelse(`Yoked..0.with.control.` == 0, "with-control", "yoked"),
                                levels = c("with-control", "yoked")),
         reward_freq = factor(`P.reward.`,
                              levels = c("0.1", "0.2", "1"),
                              labels = c("extremely_low", "moderate", "extremely_high")))

# Step 3: isolate_target_subset – Select columns relevant to the analysis
data_subset <- data %>%
  select(Subject, reward_freq, control_group, 
         `Exploration.Rates_Block1`, `Exploration.Rates_Block2`, 
         `Exploration.Rates_Block3`, `Exploration.Rates_Block4`)

# Step 4: reshape_for_analysis – Convert data to long format for ANOVA
long_data <- data_subset %>%
  pivot_longer(cols = starts_with("Exploration.Rates_Block"),
               names_to = "Block",
               names_pattern = "Exploration.Rates_Block(\\d)",
               values_to = "exploration_rate") %>%
  mutate(Block = factor(Block, levels = c("1", "2", "3", "4")),
         Subject = factor(Subject))

# Step 5: descriptive_summary – Compute mean, SD, and count for each group
descriptive_stats <- long_data %>%
  group_by(reward_freq, control_group, Block) %>%
  summarise(mean_exploration = mean(exploration_rate, na.rm = TRUE),
            sd_exploration = sd(exploration_rate, na.rm = TRUE),
            n = n(),
            .groups = "drop")
print(descriptive_stats)

# Step 6: verify_assumptions – Check normality using Shapiro-Wilk tests for each group
residuals_tests <- long_data %>%
  group_by(reward_freq, control_group, Block) %>%
  do({
    mod <- lm(exploration_rate ~ 1, data = .)
    data.frame(shapiro_p = shapiro.test(residuals(mod))$p.value)
  })
print(residuals_tests)

# Step 7: run_primary_analysis – Execute repeated measures ANOVA using aov
aov_model <- aov(exploration_rate ~ reward_freq * control_group * Block + Error(Subject/(Block)), data = long_data)
anova_results <- summary(aov_model)
print(anova_results)

# Step 8: extract_effect_sizes – Calculate partial eta squared for the three-way interaction
# Extract the main model without Error term for effect size calculation
main_model <- aov(exploration_rate ~ reward_freq * control_group * Block, data = long_data)
effect_sizes <- eta_squared(main_model, partial = TRUE)
print(effect_sizes)

# Step 9: conduct_posthoc_tests – Perform Tukey-adjusted pairwise comparisons using TukeyHSD
# Focus on specific comparisons mentioned in target text
posthoc_results <- TukeyHSD(main_model)
print(posthoc_results)
actual_means = aggregate(exploration_rate ~ reward_freq + control_group + Block, data = long_data, mean)

# Step 10: compile_results – Print ANOVA table and relevant posthoc test summaries
print("ANOVA Results:")
print(anova_results)
print("Effect Sizes:")
print(effect_sizes)
print("Post-hoc Comparisons:")
print(posthoc_results)
print(actual_means)



