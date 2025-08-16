setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_10')

# 1. import_libraries – Load required R packages
library(readr)      # For reading data files
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization
library(effsize)    # For effect size calculations (e.g., Cohen's d)
library(car)        # For assumption testing

# 2. read_data – Import dataset
data <- read.csv('dataset.csv', header = TRUE, stringsAsFactors = FALSE)

# 3. data_validation – Verify sample size and check for duplicate subject IDs
if(nrow(data) != 54) {
  stop("Sample size does not equal 54!")
}
if(any(duplicated(data$subid))) {
  warning("There are duplicate subid values!")
}

# 4. recode_condition – Convert 'condition' variable to a factor with meaningful labels
data$condition <- factor(data$condition, levels = c(0, 1), labels = c("no-pain", "pain"))

# 5. calculate_composite_scores – Compute composite measures
# Compute Threat_TOT as mean of (task_threatening, task_fearful, task_worrying, task_hostile, task_frightening, task_terrifying)
computed_threat <- rowMeans(data[, c("task_threatening", "task_fearful", "task_worrying", "task_hostile", "task_frightening", "task_terrifying")], na.rm = TRUE)
# Compute Challenge_TOT as mean of (task_challenging, task_stimulating, task_exhilarating, task_exciting, task_informative, task_enjoyable)
computed_challenge <- rowMeans(data[, c("task_challenging", "task_stimulating", "task_exhilarating", "task_exciting", "task_informative", "task_enjoyable")], na.rm = TRUE)
# Compute GroupCohension_TOT as mean of (group101, group102, group103, group104, group105, group106, group107)
computed_group <- rowMeans(data[, c("group101", "group102", "group103", "group104", "group105", "group106", "group107")], na.rm = TRUE)

# 6. verify_composite_integrity – Check that calculated composites match existing columns (if present)
if("Threat_TOT" %in% names(data)) {
  threat_check <- all.equal(data$Threat_TOT, computed_threat, tolerance = 0.001)
  if(!isTRUE(threat_check)) { warning("Threat_TOT composite does not match computed values!") }
} else {
  data$Threat_TOT <- computed_threat
}

if("Challenge_TOT" %in% names(data)) {
  challenge_check <- all.equal(data$Challenge_TOT, computed_challenge, tolerance = 0.001)
  if(!isTRUE(challenge_check)) { warning("Challenge_TOT composite does not match computed values!") }
} else {
  data$Challenge_TOT <- computed_challenge
}

if("GroupCohension_TOT" %in% names(data)) {
  group_check <- all.equal(data$GroupCohension_TOT, computed_group, tolerance = 0.001)
  if(!isTRUE(group_check)) { warning("GroupCohension_TOT composite does not match computed values!") }
} else {
  data$GroupCohension_TOT <- computed_group
}

# 7. create_analysis_subset – Select variables for analysis
analysis_data <- data %>%
  select(condition, task_intense, task_unpleasant, Pos_PANAS, Neg_PANAS, Threat_TOT, Challenge_TOT, GroupCohension_TOT)

# 8. descriptive_statistics – Calculate group means, standard deviations, and 95% confidence intervals
desc_stats <- analysis_data %>%
  group_by(condition) %>%
  summarise(
    mean_task_intense = mean(task_intense, na.rm = TRUE),
    sd_task_intense = sd(task_intense, na.rm = TRUE),
    mean_task_unpleasant = mean(task_unpleasant, na.rm = TRUE),
    sd_task_unpleasant = sd(task_unpleasant, na.rm = TRUE),
    mean_Pos_PANAS = mean(Pos_PANAS, na.rm = TRUE),
    sd_Pos_PANAS = sd(Pos_PANAS, na.rm = TRUE),
    mean_Neg_PANAS = mean(Neg_PANAS, na.rm = TRUE),
    sd_Neg_PANAS = sd(Neg_PANAS, na.rm = TRUE),
    mean_Threat_TOT = mean(Threat_TOT, na.rm = TRUE),
    sd_Threat_TOT = sd(Threat_TOT, na.rm = TRUE),
    mean_Challenge_TOT = mean(Challenge_TOT, na.rm = TRUE),
    sd_Challenge_TOT = sd(Challenge_TOT, na.rm = TRUE),
    mean_GroupCohension_TOT = mean(GroupCohension_TOT, na.rm = TRUE),
    sd_GroupCohension_TOT = sd(GroupCohension_TOT, na.rm = TRUE)
  )
print(desc_stats)

# 9. assumption_testing – Test normality and homogeneity of variance for each dependent variable by condition
dep_vars <- c("task_intense", "task_unpleasant", "Pos_PANAS", "Neg_PANAS", "Threat_TOT", "Challenge_TOT", "GroupCohension_TOT")
for (var in dep_vars) {
  # Fit an ANOVA model for the variable
  model <- aov(as.formula(paste(var, "~ condition")), data = analysis_data)
  # Test normality on residuals
  cat("Shapiro-Wilk test for", var, ":\n")
  print(shapiro.test(resid(model)))
  # Test homogeneity of variance
  group_levels <- levels(analysis_data$condition)
  grp1 <- analysis_data[analysis_data$condition == group_levels[1], var]
  grp2 <- analysis_data[analysis_data$condition == group_levels[2], var]
  cat("Variance test for", var, ":\n")
  print(var.test(grp1, grp2))
}

# 10. manipulation_checks – Independent samples t-tests for task_intense and task_unpleasant
t_test_task_intense <- t.test(task_intense ~ condition, data = analysis_data, var.equal = TRUE)
t_test_task_unpleasant <- t.test(task_unpleasant ~ condition, data = analysis_data, var.equal = TRUE)
print(t_test_task_intense)
print(t_test_task_unpleasant)

# 11. affect_analysis – Independent samples t-tests for Pos_PANAS and Neg_PANAS
t_test_Pos_PANAS <- t.test(Pos_PANAS ~ condition, data = analysis_data, var.equal = TRUE)
t_test_Neg_PANAS <- t.test(Neg_PANAS ~ condition, data = analysis_data, var.equal = TRUE)
print(t_test_Pos_PANAS)
print(t_test_Neg_PANAS)

# 12. appraisal_analysis – Independent samples t-tests for Threat_TOT and Challenge_TOT
t_test_Threat_TOT <- t.test(Threat_TOT ~ condition, data = analysis_data, var.equal = TRUE)
t_test_Challenge_TOT <- t.test(Challenge_TOT ~ condition, data = analysis_data, var.equal = TRUE)
print(t_test_Threat_TOT)
print(t_test_Challenge_TOT)

# 13. bonding_analysis – One-way ANOVA for GroupCohension_TOT
anova_bonding <- aov(GroupCohension_TOT ~ condition, data = analysis_data)
anova_summary <- summary(anova_bonding)
print(anova_summary)

# 14. effect_size_calculation – Compute Cohen's d for bonding effect
cohen_d_bonding <- cohen.d(GroupCohension_TOT ~ condition, data = analysis_data, pooled = TRUE, conf.level = 0.95)
print(cohen_d_bonding)

# 15. confidence_intervals – Calculate 95% confidence intervals for group means of GroupCohension_TOT
no_pain_data <- analysis_data$GroupCohension_TOT[analysis_data$condition == "no-pain"]
pain_data <- analysis_data$GroupCohension_TOT[analysis_data$condition == "pain"]
ci_no_pain <- t.test(no_pain_data)$conf.int
ci_pain <- t.test(pain_data)$conf.int
cat("95% CI for no-pain group GroupCohension_TOT: ", ci_no_pain, "\n")
cat("95% CI for pain group GroupCohension_TOT: ", ci_pain, "\n")

# 16. compile_results_table – Create structured output table with consolidated results
results_table <- data.frame(
  Variable = c("task_intense", "task_unpleasant", "Pos_PANAS", "Neg_PANAS", "Threat_TOT", "Challenge_TOT", "GroupCohension_TOT"),
  Pain_Mean = c(
    tapply(analysis_data$task_intense, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$task_unpleasant, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$Pos_PANAS, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$Neg_PANAS, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$Threat_TOT, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$Challenge_TOT, analysis_data$condition, mean)["pain"],
    tapply(analysis_data$GroupCohension_TOT, analysis_data$condition, mean)["pain"]
  ),
  Pain_SD = c(
    tapply(analysis_data$task_intense, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$task_unpleasant, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$Pos_PANAS, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$Neg_PANAS, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$Threat_TOT, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$Challenge_TOT, analysis_data$condition, sd)["pain"],
    tapply(analysis_data$GroupCohension_TOT, analysis_data$condition, sd)["pain"]
  ),
  NoPain_Mean = c(
    tapply(analysis_data$task_intense, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$task_unpleasant, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$Pos_PANAS, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$Neg_PANAS, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$Threat_TOT, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$Challenge_TOT, analysis_data$condition, mean)["no-pain"],
    tapply(analysis_data$GroupCohension_TOT, analysis_data$condition, mean)["no-pain"]
  ),
  NoPain_SD = c(
    tapply(analysis_data$task_intense, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$task_unpleasant, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$Pos_PANAS, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$Neg_PANAS, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$Threat_TOT, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$Challenge_TOT, analysis_data$condition, sd)["no-pain"],
    tapply(analysis_data$GroupCohension_TOT, analysis_data$condition, sd)["no-pain"]
  ),
  t_value = c(
    t_test_task_intense$statistic,
    t_test_task_unpleasant$statistic,
    t_test_Pos_PANAS$statistic,
    t_test_Neg_PANAS$statistic,
    t_test_Threat_TOT$statistic,
    t_test_Challenge_TOT$statistic,
    NA  # ANOVA does not produce a t-test statistic for bonding
  ),
  df = c(
    t_test_task_intense$parameter,
    t_test_task_unpleasant$parameter,
    t_test_Pos_PANAS$parameter,
    t_test_Neg_PANAS$parameter,
    t_test_Threat_TOT$parameter,
    t_test_Challenge_TOT$parameter,
    anova_summary[[1]]$Df[2]  # degrees of freedom from ANOVA for bonding
  ),
  p_value = c(
    t_test_task_intense$p.value,
    t_test_task_unpleasant$p.value,
    t_test_Pos_PANAS$p.value,
    t_test_Neg_PANAS$p.value,
    t_test_Threat_TOT$p.value,
    t_test_Challenge_TOT$p.value,
    summary(anova_bonding)[[1]]["Pr(>F)"][1,1]  # p-value for bonding ANOVA
  ),
  effect_size = c(
    NA,  # not computed for task_intense
    NA,  # not computed for task_unpleasant
    NA,  # not computed for Pos_PANAS
    NA,  # not computed for Neg_PANAS
    NA,  # not computed for Threat_TOT
    NA,  # not computed for Challenge_TOT
    cohen_d_bonding$estimate  # Cohen's d for bonding
  )
)
print(results_table)

# 17. validate_reported_values – Cross-check computed statistics against reported values
cat("Please verify that the reported means, SDs, t-values, F-statistic, p-values, and effect sizes match the values in the article.\n")

aov_data <- d %>% select(subid, condition, GroupCohension_TOT)

aov_out <- ezANOVA(aov_data, wid = subid, 
                   dv = GroupCohension_TOT , within = NULL, 
                   between = condition , observed = NULL , diff = NULL, 
                   reverse_diff = FALSE , type = 2)

d.out <- cohensD(filter(aov_data, condition == 0) %>% 
                   pull(GroupCohension_TOT), filter(aov_data, condition == 1) %>% 
                   pull(GroupCohension_TOT))
aov_out
