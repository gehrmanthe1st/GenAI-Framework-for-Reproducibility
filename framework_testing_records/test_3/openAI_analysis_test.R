setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_3')

# 1. import_libraries: Load required packages using readr, dplyr, broom, stats, and effsize.
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation
library(broom)   # For tidying statistical test outputs
library(stats)   # For t.test and shapiro.test
library(effsize) # For computing Cohen's d

# 2. read_data: Read the dataset from CSV file; participant count check is skipped.
data <- read.csv("dataset.csv")

# 3. map_prime_conditions: Create cue condition variable mapping predictive conditions for switch trials.
data <- data %>%
  mutate(CueCondition = case_when(
    TrialType == 2 & swt_8.1 == 1 ~ "predictive",
    TrialType == 2 & swt_2.1 == 1 ~ "nonpredictive",
    TRUE ~ NA_character_
  ))

# 4. calculate_overall_descriptive: Calculate overall descriptive measures for switch and repeat trials.
# For switch trials (TrialType == 2)
overall_switch <- data %>%
  filter(TrialType == 2) %>%
  group_by(Participant) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE),
            accuracy = mean(CorrResp, na.rm = TRUE))
# For repeat trials (assuming TrialType == 1 represents repeat trials)
overall_repeat <- data %>%
  filter(TrialType == 1) %>%
  group_by(Participant) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE),
            accuracy = mean(CorrResp, na.rm = TRUE))
# Print overall descriptive measures averaged across participants
print("Overall switch trials summary (mean RT and accuracy):")
print(overall_switch %>% summarise(across(c(mean_RT, accuracy), mean, na.rm = TRUE)))
print("Overall repeat trials summary (mean RT and accuracy):")
print(overall_repeat %>% summarise(across(c(mean_RT, accuracy), mean, na.rm = TRUE)))

# 5. isolate_switch_trials: Filter for switch trials only.
switch_trials <- data %>% filter(TrialType == 2)

# 6. separate_cue_conditions: Create subsets for predictive and nonpredictive switch trials.
predictive_switch <- switch_trials %>% filter(CueCondition == "predictive")
nonpredictive_switch <- switch_trials %>% filter(CueCondition == "nonpredictive")

# 7. calculate_participant_means: Calculate mean RT and accuracy for each Participant in each cue condition.
predictive_summary <- predictive_switch %>%
  group_by(Participant) %>%
  summarise(predictive_RT = mean(RT, na.rm = TRUE),
            predictive_accuracy = mean(CorrResp, na.rm = TRUE))

nonpredictive_summary <- nonpredictive_switch %>%
  group_by(Participant) %>%
  summarise(nonpredictive_RT = mean(RT, na.rm = TRUE),
            nonpredictive_accuracy = mean(CorrResp, na.rm = TRUE))

# Merge participant-level summaries to obtain paired data
participant_means <- merge(predictive_summary, nonpredictive_summary, by = "Participant")
print("Participant-level means for predictive and nonpredictive switch trials:")
print(participant_means)

# 8. compute_overall_switch_cost: Calculate overall switch cost comparing switch vs. repeat trials.
overall_switch_cost <- list(
  RT_difference = mean(overall_switch$mean_RT, na.rm = TRUE) - mean(overall_repeat$mean_RT, na.rm = TRUE),
  accuracy_difference = mean(overall_switch$accuracy, na.rm = TRUE) - mean(overall_repeat$accuracy, na.rm = TRUE)
)
print("Overall switch cost (Switch - Repeat):")
print(overall_switch_cost)

# 9. verify_normality: Test normality of within-participant RT differences.
participant_means <- participant_means %>%
  mutate(RT_diff = nonpredictive_RT - predictive_RT)
normality_test <- shapiro.test(participant_means$RT_diff)
print("Shapiro-Wilk normality test for RT differences:")
print(tidy(normality_test))

# 10. conduct_rt_analysis: Run paired t-test on participant-level mean RTs.
rt_ttest <- t.test(participant_means$nonpredictive_RT, participant_means$predictive_RT, paired = TRUE)
print("Paired t-test results for RT differences:")
print(tidy(rt_ttest))

# 11. conduct_accuracy_analysis: Run paired t-test on participant-level accuracy rates.
accuracy_ttest <- t.test(participant_means$nonpredictive_accuracy, participant_means$predictive_accuracy, paired = TRUE)
print("Paired t-test results for accuracy differences:")
print(tidy(accuracy_ttest))

# 12. calculate_effect_size: Compute Cohen's d for the RT difference.
rt_cohen_d <- cohen.d(participant_means$nonpredictive_RT, participant_means$predictive_RT, paired = TRUE)
print("Cohen's d for RT differences:")
print(rt_cohen_d)

# 13. format_results: Extract and print final key statistical values.
final_results <- list(
  rt_ttest = tidy(rt_ttest),
  accuracy_ttest = tidy(accuracy_ttest),
  rt_effect_size = rt_cohen_d
)
print("Final formatted results:")
print(final_results)

#User addition
RT_pred = mean(participant_means$predictive_RT, na.rm = TRUE)
RT_nonpred = mean(participant_means$nonpredictive_RT, na.rm = TRUE)
RT_diff = mean(participant_means$RT_diff, na.rm = TRUE)
paste("Mean RT for predictive switch trials:", RT_pred)
paste("Mean RT for nonpredictive switch trials:", RT_nonpred)
paste("Mean RT difference (Predictive - Nonpredictive):", RT_diff)
