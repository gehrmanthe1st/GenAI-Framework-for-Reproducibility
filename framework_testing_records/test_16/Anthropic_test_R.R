setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_16')

# 1. import_libraries: Load required R packages.
library(readr)      # Import CSV data
library(dplyr)      # Data manipulation and cleaning
library(tidyr)      # Data reshaping
library(ez)         # Repeated measures ANOVA and assumption tests
library(effectsize) # Calculation of effect sizes (Cohen's d, partial eta²)
library(broom)      # Formatting statistical output
library(rstatix)    # Pairwise paired t-tests and effect size calculations

# 2. read_data: Import the dataset with explicit column handling.
data <- read_csv("dataset.csv", col_types = cols(
  Participant = col_double(),
  DirectionofRotation = col_double(),
  NormalisedDataUploadedToDataverse = col_double(),
  Condition1_Gain0.8 = col_double(),
  Condition2_Gain1 = col_double(),
  Condition3_Gain1.2 = col_double(),
  RawData = col_double(),
  Point8 = col_double(),
  One = col_double(),
  Onepoint2 = col_double(),
  NormalisedDataNotUploaded = col_double(),
  Point.8 = col_double(),
  One1 = col_double(),
  One1.2 = col_double()
))

# 3. clean_data: Remove columns with all missing values.
data_clean <- data %>% 
  select(-all_of(c("NormalisedDataUploadedToDataverse", "RawData", "NormalisedDataNotUploaded")))

# 4. recode_variables: Convert Participant to factor and add DirectionofRotation as factor.
data_clean <- data_clean %>% 
  mutate(
    Participant = as.factor(Participant),
    DirectionofRotation = as.factor(DirectionofRotation)
  )

# 5. aggregate_directions: Keep rotation directions separate (no averaging).
# Data is already in the correct format with separate rows for each direction

# 6. reshape_for_analysis: Transform data into long format keeping directions separate.
data_long <- data_clean %>% 
  pivot_longer(
    cols = c(Condition1_Gain0.8, Condition2_Gain1, Condition3_Gain1.2),
    names_to = "Condition",
    values_to = "Range"
  )

data_long$Subj <- interaction(data_long$Participant, data_long$DirectionofRotation, drop = TRUE)

# 7. verify_assumptions: Test normality excluding Condition2_Gain1 if it has constant values.
# First check if Condition2_Gain1 has only one unique value
condition2_check <- data_long %>% 
  filter(Condition == "Condition2_Gain1") %>%
  summarise(n_unique = n_distinct(Range, na.rm = TRUE))

print("Number of unique values in Condition2_Gain1:")
print(condition2_check)

# Normality: Apply Shapiro-Wilk test excluding Condition2_Gain1 if it has only 1 value
if(condition2_check$n_unique > 1) {
  normality_tests <- data_long %>% 
    group_by(Condition) %>% 
    summarise(shapiro_p = shapiro.test(Range)$p.value)
} else {
  normality_tests <- data_long %>% 
    filter(Condition != "Condition2_Gain1") %>%
    group_by(Condition) %>% 
    summarise(shapiro_p = shapiro.test(Range)$p.value)
  print("Note: Shapiro-Wilk test excluded Condition2_Gain1 due to constant values")
}

print("Shapiro-Wilk normality test p-values by Condition:")
print(normality_tests)

# Sphericity: Run ezANOVA to retrieve Mauchly's test outcome.
anova_for_assump <- ezANOVA(
  data = data_long,
  dv = Range,
  wid = Subj,
  within = .(Condition),
  detailed = TRUE,
  return_aov = TRUE
)
print("ezANOVA output (including Mauchly's sphericity test):")
print(anova_for_assump)

# 8. run_repeated_measures_anova: Conduct the repeated measures ANOVA.
anova_results <- ezANOVA(
  data = data_long,
  dv = Range,
  wid = Subj,
  within = .(Condition),
  detailed = TRUE,
  return_aov = TRUE
)
print("Repeated Measures ANOVA results:")
print(anova_results)

eta_sq = effectsize::eta_squared(
  anova_results$aov,          
  partial      = TRUE,        
  generalized  = FALSE,
  include_intercept = FALSE   
)

# 9. conduct_pairwise_comparisons: Perform Bonferroni-corrected paired t-tests.
pairwise_results <- data_long %>% 
  pairwise_t_test(
    Range ~ Condition,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print("Pairwise paired t-test results with Bonferroni correction:")
print(pairwise_results)



# 10. calculate_effect_sizes: Compute Cohen's d for specific contrasts.
# Understated feedback: Condition1_Gain0.8 vs accurate Condition2_Gain1
d_understated <- data_long %>%
  filter(Condition %in% c("Condition1_Gain0.8", "Condition2_Gain1")) %>%
  effectsize::cohens_d(Range ~ Condition, data = .)

# Overstated feedback: Condition3_Gain1.2 vs accurate Condition2_Gain1
d_overstated <- data_long %>%
  filter(Condition %in% c("Condition3_Gain1.2", "Condition2_Gain1")) %>%
  effectsize::cohens_d(Range ~ Condition, data=.)

print("Cohen's d for Understated feedback (Condition1_Gain0.8 vs Condition2_Gain1):")
print(d_understated)
print("Cohen's d for Overstated feedback (Condition3_Gain1.2 vs Condition2_Gain1):")
print(d_overstated)

# 11. calculate_percent_changes: Compute percent changes with 95% confidence intervals.
# Compute means for each condition across all observations (no averaging by participant first)
means <- data_long %>% 
  group_by(Condition) %>%
  summarise(mean_range = mean(Range, na.rm = TRUE))

mean_understated <- means$mean_range[means$Condition == "Condition1_Gain0.8"]
mean_accurate    <- means$mean_range[means$Condition == "Condition2_Gain1"]
mean_overstated  <- means$mean_range[means$Condition == "Condition3_Gain1.2"]

# Calculate percent changes
percent_change_under <- (mean_understated - mean_accurate) / mean_accurate * 100
percent_change_over  <- (mean_overstated - mean_accurate) / mean_accurate * 100

# Get paired data for t-tests
understated_data <- data_long %>% filter(Condition == "Condition1_Gain0.8") %>% arrange(Participant, DirectionofRotation)
accurate_data <- data_long %>% filter(Condition == "Condition2_Gain1") %>% arrange(Participant, DirectionofRotation)
overstated_data <- data_long %>% filter(Condition == "Condition3_Gain1.2") %>% arrange(Participant, DirectionofRotation)

# Paired t-tests to get 95% confidence intervals for percent changes.
# Understated feedback
t_under <- t.test(understated_data$Range, accurate_data$Range, paired = TRUE)
ci_under <- t_under$conf.int * 100 / mean_accurate

# Overstated feedback
t_over <- t.test(overstated_data$Range, accurate_data$Range, paired = TRUE)
ci_over <- t_over$conf.int * 100 / mean_accurate

print("Percent change for Understated feedback (Condition1_Gain0.8 vs Condition2_Gain1):")
print(paste0(round(percent_change_under, 2), "%, 95% CI: [", round(ci_under[1],2), "%, ", round(ci_under[2],2), "%]"))
print("Percent change for Overstated feedback (Condition3_Gain1.2 vs Condition2_Gain1):")
print(paste0(round(percent_change_over, 2), "%, 95% CI: [", round(ci_over[1],2), "%, ", round(ci_over[2],2), "%]"))

# 12. compile_final_results: Output a summary of the key results.
cat("\n--- FINAL RESULTS ---\n")
cat("\nRepeated Measures ANOVA:\n")
print(anova_results$ANOVA)
cat("\nPairwise Comparisons:\n")
print(pairwise_results)
cat("\nEffect Sizes:\n")
print("Understated (Cohen's d):")
print(d_understated)
print("Overstated (Cohen's d):")
print(d_overstated)
cat("\nPercent Changes:\n")
cat("Understated feedback: ", round(percent_change_under,2), "% (95% CI: ", round(ci_under[1],2), "%, ", round(ci_under[2],2), "%)\n", sep = "")
cat("Overstated feedback: ", round(percent_change_over,2), "% (95% CI: ", round(ci_over[1],2), "%, ", round(ci_over[2],2), "%)\n", sep = "")
print(eta_sq)



