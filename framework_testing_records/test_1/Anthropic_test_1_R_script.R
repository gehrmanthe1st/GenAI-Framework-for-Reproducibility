setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_1')

# 1. library(dplyr); library(readr); library(boot)
library(dplyr)  # for data manipulation (filtering, grouping, summarizing)
library(readr)  # for reading CSV files
library(boot)   # for bootstrap confidence intervals

# 2. data <- read_csv('data.csv')
data <- read_csv('data/JaraEttinger_Tenenbaum_Schulz_Results.csv')

# 3. # First identify all inclusion/exclusion categories as described in the paper
exclusion_counts <- data %>%
  filter(Experiment == "Experiment 3") %>%
  group_by(Condition, Status) %>%
  summarise(count = n(), .groups = "drop")

# 4. # Filter only data subjects and verify excluded counts match paper
exp3_data <- data %>%
  filter(Experiment == "Experiment 3" & Status == "Data" & !is.na(Choice) & 
           Choice != "None")

# 5. # Verify we have 16 subjects per condition as reported
condition_counts <- exp3_data %>%
  group_by(Condition) %>%
  summarise(n = n(), .groups = "drop")
print(condition_counts)

# 6. # Define bootstrap function for confidence intervals of proportions for Competent choices
boot_prop <- function(choice_data, indices) { 
  mean(choice_data[indices] == "Competent")
}

# 7. # Play condition analysis - 81.25% chose competent agent
play_data <- exp3_data %>% filter(Condition == "Play")
play_comp_count <- sum(play_data$Choice == "Competent")
play_comp_pct <- mean(play_data$Choice == "Competent") * 100
play_boot <- boot(play_data$Choice, boot_prop, R = 10000)
play_ci <- boot.ci(play_boot, type = "perc", conf = 0.95)$percent[4:5] * 100

# 8. # Nicer condition analysis - 31.25% chose competent agent
nicer_data <- exp3_data %>% filter(Condition == "Nicer")
nicer_comp_count <- sum(nicer_data$Choice == "Competent")
nicer_comp_pct <- mean(nicer_data$Choice == "Competent") * 100
nicer_boot <- boot(nicer_data$Choice, boot_prop, R = 10000)
nicer_ci <- boot.ci(nicer_boot, type = "perc", conf = 0.95)$percent[4:5] * 100

# 9. # Fisher's exact test comparing play vs nicer condition (competent choice)
play_vs_nicer <- matrix(c(play_comp_count, nrow(play_data) - play_comp_count,
                          nicer_comp_count, nrow(nicer_data) - nicer_comp_count), nrow = 2)
fisher_play_nicer <- fisher.test(play_vs_nicer)

# 10. # Calculate bootstrap confidence interval for difference in proportions between play and nicer conditions
diff_boot <- boot(c(rep(1, play_comp_count),
                    rep(0, nrow(play_data) - play_comp_count),
                    rep(0, nicer_comp_count),
                    rep(1, nrow(nicer_data) - nicer_comp_count)),
                  function(d, i) {
                    play_mean <- mean(d[i][1:nrow(play_data)])
                    nicer_mean <- mean(d[i][(nrow(play_data) + 1):length(d[i])])
                    play_mean - nicer_mean
                  },
                  R = 10000)
diff_ci <- boot.ci(diff_boot, type = "perc", conf = 0.95)$percent[4:5] * 100

# 11. # Control condition analysis - incompetent agent choice comparison
control_data <- exp3_data %>% filter(Condition == "Control")
control_incomp_count <- sum(control_data$Choice == "Incompetent")
control_incomp_pct <- mean(control_data$Choice == "Incompetent") * 100
nicer_incomp_count <- sum(nicer_data$Choice == "Incompetent")
nicer_incomp_pct <- mean(nicer_data$Choice == "Incompetent") * 100

# 12. # Fisher's exact test comparing nicer vs control condition (incompetent choice)
nicer_vs_control <- matrix(c(nicer_incomp_count, nrow(nicer_data) - nicer_incomp_count,
                             control_incomp_count, nrow(control_data) - control_incomp_count), nrow = 2)
fisher_nicer_control <- fisher.test(nicer_vs_control)

# 13. # Calculate bootstrap confidence interval for difference in proportions (incompetent choice) between nicer and control conditions
incomp_diff_boot <- boot(c(rep(1, nicer_incomp_count),
                           rep(0, nrow(nicer_data) - nicer_incomp_count),
                           rep(0, control_incomp_count),
                           rep(1, nrow(control_data) - control_incomp_count)),
                         function(d, i) {
                           nicer_mean <- mean(d[i][1:nrow(nicer_data)])
                           control_mean <- mean(d[i][(nrow(nicer_data) + 1):length(d[i])])
                           nicer_mean - control_mean
                         },
                         R = 10000)
incomp_diff_ci <- boot.ci(incomp_diff_boot, type = "perc", conf = 0.95)$percent[4:5] * 100

# 14. # Print the results to the console
print(list(
  play_comp_count = play_comp_count,
  play_comp_pct = play_comp_pct,
  play_ci = play_ci,
  nicer_comp_count = nicer_comp_count,
  nicer_comp_pct = nicer_comp_pct,
  nicer_ci = nicer_ci,
  fisher_play_nicer = fisher_play_nicer,
  diff_ci = diff_ci,
  control_incomp_count = control_incomp_count,
  control_incomp_pct = control_incomp_pct,
  nicer_incomp_count = nicer_incomp_count,
  nicer_incomp_pct = nicer_incomp_pct,
  fisher_nicer_control = fisher_nicer_control,
  incomp_diff_ci = incomp_diff_ci
))
