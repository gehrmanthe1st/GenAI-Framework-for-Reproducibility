setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_15')

# 1. import_libraries: Load required libraries
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation and filtering
library(stats)    # For statistical tests (t.test, shapiro.test)
library(effsize)  # For effect size calculations (cohen.d)
library(broom)    # For tidying output of statistical tests
library(effectsize)

# 2. read_data: Load the dataset from a CSV file into a dataframe (adjust the file path as needed)
data <- read_csv("dataset.csv")

# 3. eligibility_filtering: Filter dataset to retain only eligible participants (Eligible == 1)
eligible_data <- data %>% 
  filter(Eligible == 1)

# 4. create_analysis_subsets: Create a subset with Race in c('White', 'Asian', 'Black', 'Hispanic', 'Other')
analysis_data <- eligible_data %>% 
  filter(Race %in% c("White", "Asian", "Black", "Hispanic", "Other"))

# 5. verify_aggregate_scores: Check that aggregate implicit scores (WhiteImplicit, BlackImplicit, AsianImplicit, HispImplicit)
# sum approximately equals 0 for each participant (tolerance set to 0.1)
analysis_data <- analysis_data %>% 
  mutate(aggregate_sum = WhiteImplicit + BlackImplicit + AsianImplicit + HispImplicit)

# Print summary of aggregate_sum to verify it is around 0
print(summary(analysis_data$aggregate_sum))

# 6. assumption_checking: For each racial subgroup, check normality of differences using shapiro.test()
race_groups <- unique(analysis_data$Race)
assumption_results <- list()

for (race in race_groups) {
  subgroup <- analysis_data %>% filter(Race == race)
  
  # Compute paired differences
  diff_WA <- subgroup$WhiteImplicit - subgroup$AsianImplicit
  diff_AB <- subgroup$AsianImplicit - subgroup$BlackImplicit
  diff_BH <- subgroup$BlackImplicit - subgroup$HispImplicit
  
  # Shapiro tests for normality
  shapiro_WA <- shapiro.test(diff_WA)
  shapiro_AB <- shapiro.test(diff_AB)
  shapiro_BH <- shapiro.test(diff_BH)
  
  assumption_results[[race]] <- list(
    shapiro_WA = shapiro_WA,
    shapiro_AB = shapiro_AB,
    shapiro_BH = shapiro_BH
  )
}

# Optionally print assumption checking results
print(assumption_results)

# 7. primary_analysis: For each racial subgroup, perform three paired t-tests
primary_results <- list()

for (race in race_groups) {
  subgroup <- analysis_data %>% filter(Race == race)
  
  # Paired t-test: WhiteImplicit vs AsianImplicit
  ttest_WA <- t.test(subgroup$WhiteImplicit, subgroup$AsianImplicit, paired = TRUE)
  # Paired t-test: AsianImplicit vs BlackImplicit
  ttest_AB <- t.test(subgroup$AsianImplicit, subgroup$BlackImplicit, paired = TRUE)
  # Paired t-test: BlackImplicit vs HispImplicit
  ttest_BH <- t.test(subgroup$BlackImplicit, subgroup$HispImplicit, paired = TRUE)
  
  primary_results[[race]] <- list(
    ttest_WA = ttest_WA,
    ttest_AB = ttest_AB,
    ttest_BH = ttest_BH
  )
}

# 8. effect_size_calculation: Calculate Cohen's d for each paired comparison
effect_sizes <- list()

for (race in race_groups) {
  subgroup <- analysis_data %>% filter(Race == race)
  
  # Remove NA pairs before calculating Cohen's d
  complete_WA <- complete.cases(subgroup$WhiteImplicit, subgroup$AsianImplicit)
  complete_AB <- complete.cases(subgroup$AsianImplicit, subgroup$BlackImplicit)
  complete_BH <- complete.cases(subgroup$BlackImplicit, subgroup$HispImplicit)
  
  # Calculate Cohen's d for each paired difference with paired = TRUE
  d_WA <- cohens_d(subgroup$WhiteImplicit[complete_WA], subgroup$AsianImplicit[complete_WA], paired = TRUE)
  d_AB <- cohens_d(subgroup$AsianImplicit[complete_AB], subgroup$BlackImplicit[complete_AB], paired = TRUE)
  d_BH <- cohens_d(subgroup$BlackImplicit[complete_BH], subgroup$HispImplicit[complete_BH], paired = TRUE)
  
  effect_sizes[[race]] <- list(
    d_WA = d_WA,
    d_AB = d_AB,
    d_BH = d_BH
  )
}

# 9. results_validation: Compare outputs to target values in Table S1 (validation is printed to console)
# Here we print t-test summaries and effect sizes for manual comparison.
results_validation <- list()

for (race in race_groups) {
  ttest_WA <- primary_results[[race]]$ttest_WA
  ttest_AB <- primary_results[[race]]$ttest_AB
  ttest_BH <- primary_results[[race]]$ttest_BH
  
  d_WA <- effect_sizes[[race]]$d_WA
  d_AB <- effect_sizes[[race]]$d_AB
  d_BH <- effect_sizes[[race]]$d_BH
  
  results_validation[[race]] <- list(
    ttest_WA = tidy(ttest_WA),
    ttest_AB = tidy(ttest_AB),
    ttest_BH = tidy(ttest_BH),
    cohen_d_WA = d_WA,
    cohen_d_AB = d_AB,
    cohen_d_BH = d_BH
  )
}

# Print the validation results
print(results_validation)

# 10. compile_final_results: Aggregate all results into a final results table
final_results <- data.frame()

for (race in race_groups) {
  
  # Extract tidy results for each test
  res_WA <- tidy(primary_results[[race]]$ttest_WA) %>% 
    mutate(Comparison = "WhiteImplicit_vs_AsianImplicit", Race = race)
  res_AB <- tidy(primary_results[[race]]$ttest_AB) %>% 
    mutate(Comparison = "AsianImplicit_vs_BlackImplicit", Race = race)
  res_BH <- tidy(primary_results[[race]]$ttest_BH) %>% 
    mutate(Comparison = "BlackImplicit_vs_HispImplicit", Race = race)
  
  # Combine the results
  combined <- bind_rows(res_WA, res_AB, res_BH) %>% 
    select(Race, Comparison, statistic, parameter, p.value)
  
  # Add Cohen's d values for corresponding comparisons
  d_WA <- effect_sizes[[race]]$d_WA$Cohens_d
  d_AB <- effect_sizes[[race]]$d_AB$Cohens_d
  d_BH <- effect_sizes[[race]]$d_BH$Cohens_d
  combined <- combined %>% 
    mutate(Cohen_d = case_when(
      Comparison == "WhiteImplicit_vs_AsianImplicit" ~ d_WA,
      Comparison == "AsianImplicit_vs_BlackImplicit" ~ d_AB,
      Comparison == "BlackImplicit_vs_HispImplicit" ~ d_BH
    ))
  
  final_results <- bind_rows(final_results, combined)
}

# Print the final results table to console
print(final_results)
