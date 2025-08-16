setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_15')

# 1. import_libraries: Load required packages
library(readr)        # For reading CSV data
library(dplyr)        # For data manipulation and wrangling
library(effectsize)   # For computing Cohen's d via cohens_d()
library(broom)        # For tidying statistical test outputs
library(stats)        # For t.test() and shapiro.test()

# 2. read_data: Import the dataset from the CSV file
data <- read_csv("dataset.csv")

# 3. clean_and_recode: Convert 'nan' strings to NA, enforce numeric types, and filter eligible participants
numeric_cols <- c("Age", "TrialErrorPct", "TooFastPct", "WhiteExplicit", "BlackExplicit", "AsianExplicit", "HispanicExplicit",
                  "WhiteImplicit", "BlackImplicit", "AsianImplicit", "HispImplicit", "WhitevsAsianBIAT", "WhitevsHispanicBIAT",
                  "WhitevsBlackBIAT", "AsianvsHispanicBIAT", "AsianvsBlackBIAT", "HispanicvsBlackBIAT", 
                  "WhitevsAsianExp", "WhitevsHispanicExp", "WhitevsBlackExp", "AsianvsBlackExp", "AsianvsHispanicExp",
                  "BlackvsHispanicExp", "Eligible")

eligible_data <- data %>% 
  filter(Eligible == 1)

# 4. isolate_target_subset: Retain rows with Race in target groups including 'Other'
data_target <- eligible_data %>%
  filter(Race %in% c("White", "Asian", "Black", "Hispanic", "Other"))

# 5. compute_aggregate_scores: Calculate aggregate implicit scores per participant
data_target <- data_target %>%
  mutate(
    WhiteAgg = (WhitevsAsianBIAT + WhitevsHispanicBIAT + WhitevsBlackBIAT) / 3,
    AsianAgg = (-WhitevsAsianBIAT + AsianvsHispanicBIAT + AsianvsBlackBIAT) / 3,
    BlackAgg = (-WhitevsBlackBIAT - AsianvsBlackBIAT + HispanicvsBlackBIAT) / 3,
    HispAgg  = (-WhitevsHispanicBIAT - AsianvsHispanicBIAT - HispanicvsBlackBIAT) / 3
  )

# 6. verify_hierarchy_order: Compute mean aggregate scores by Race to confirm expected order
hierarchy_check <- data_target %>%
  group_by(Race) %>%
  summarise(
    mean_WhiteAgg = mean(WhiteAgg, na.rm = TRUE),
    mean_AsianAgg = mean(AsianAgg, na.rm = TRUE),
    mean_BlackAgg = mean(BlackAgg, na.rm = TRUE),
    mean_HispAgg  = mean(HispAgg, na.rm = TRUE),
    .groups = "drop"
  )
print(hierarchy_check)

# 7. prepare_pairwise_comparisons:
# Reorder aggregate scores so that the participant's in-group aggregate is first,
# then the remaining aggregates follow the fixed order: White, Asian, Black, Hispanic.
data_target <- data_target %>%
  mutate(
    firstAgg = case_when(
      Race == "White"    ~ WhiteImplicit,
      Race == "Asian"    ~ AsianImplicit,
      Race == "Black"    ~ BlackImplicit,
      Race == "Hispanic" ~ HispImplicit,
      TRUE               ~ WhiteImplicit  # For 'Other', default to White first
    ),
    secondAgg = case_when(
      Race == "White"    ~ AsianImplicit,  # White participants: White, Asian, Black, Hispanic
      Race == "Asian"    ~ WhiteImplicit,  # Asian participants: Asian, White, Black, Hispanic
      Race == "Black"    ~ WhiteImplicit,  # Black participants: Black, White, Asian, Hispanic
      Race == "Hispanic" ~ WhiteImplicit,  # Hispanic participants: Hispanic, White, Asian, Black
      TRUE               ~ AsianImplicit   # Other participants: White, Asian, Black, Hispanic
    ),
    thirdAgg = case_when(
      Race == "White"    ~ BlackImplicit,
      Race == "Asian"    ~ BlackImplicit,
      Race == "Black"    ~ AsianImplicit,
      Race == "Hispanic" ~ AsianImplicit,
      TRUE               ~ BlackImplicit
    ),
    fourthAgg = case_when(
      Race == "White"    ~ HispImplicit,
      Race == "Asian"    ~ HispImplicit,
      Race == "Black"    ~ HispImplicit,
      Race == "Hispanic" ~ BlackImplicit,
      TRUE               ~ HispImplicit
    )
  ) %>%
  # Compute pairwise difference scores for consecutive aggregates
  mutate(
    diff_1 = firstAgg - secondAgg,
    diff_2 = secondAgg - thirdAgg,
    diff_3 = thirdAgg - fourthAgg
  )

# 8. verify_assumptions: Run shapiro.test() on each difference score within each Race group
assumption_tests <- data_target %>%
  group_by(Race) %>%
  summarise(
    shapiro_diff1 = shapiro.test(diff_1)$p.value,
    shapiro_diff2 = shapiro.test(diff_2)$p.value,
    shapiro_diff3 = shapiro.test(diff_3)$p.value,
    .groups = "drop"
  )
print(assumption_tests)

# 9. run_primary_analysis: Run paired t-tests for each participant Race group on consecutive aggregate differences
t_test_results <- list()
for(r in unique(data_target$Race)) {
  subset_data <- filter(data_target, Race == r)
  # Paired t-test for first vs second aggregate difference
  t1 <- t.test(subset_data$firstAgg, subset_data$secondAgg, paired = TRUE)
  # Paired t-test for second vs third aggregate difference
  t2 <- t.test(subset_data$secondAgg, subset_data$thirdAgg, paired = TRUE)
  # Paired t-test for third vs fourth aggregate difference
  t3 <- t.test(subset_data$thirdAgg, subset_data$fourthAgg, paired = TRUE)
  t_test_results[[r]] <- list(test1 = t1, test2 = t2, test3 = t3)
}

# 10. calculate_effect_sizes: Compute Cohen's d for each paired t-test using effectsize::cohens_d()
effect_size_results <- list()
for(r in unique(data_target$Race)) {
  subset_data <- filter(data_target, Race == r)
  d1 <- cohens_d(subset_data$firstAgg, subset_data$secondAgg, paired = TRUE)
  d2 <- cohens_d(subset_data$secondAgg, subset_data$thirdAgg, paired = TRUE)
  d3 <- cohens_d(subset_data$thirdAgg, subset_data$fourthAgg, paired = TRUE)
  effect_size_results[[r]] <- list(d1 = d1, d2 = d2, d3 = d3)
}

# 11. compile_results: Combine t-test results and effect sizes into a summary table
results_list <- lapply(names(t_test_results), function(race) {
  t1 <- t_test_results[[race]]$test1
  t2 <- t_test_results[[race]]$test2
  t3 <- t_test_results[[race]]$test3
  d1 <- effect_size_results[[race]]$d1$Cohens_d
  d2 <- effect_size_results[[race]]$d2$Cohens_d
  d3 <- effect_size_results[[race]]$d3$Cohens_d
  data.frame(
    Race = race,
    Comparison = c("Aggregate 1 vs Aggregate 2", "Aggregate 2 vs Aggregate 3", "Aggregate 3 vs Aggregate 4"),
    t_value = c(t1$statistic, t2$statistic, t3$statistic),
    df = c(t1$parameter, t2$parameter, t3$parameter),
    p_value = c(t1$p.value, t2$p.value, t3$p.value),
    Cohens_d = c(d1, d2, d3)
  )
})
final_results <- do.call(rbind, results_list)
print(final_results)
