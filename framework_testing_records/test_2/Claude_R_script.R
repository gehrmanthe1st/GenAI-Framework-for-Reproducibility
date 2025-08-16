setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_2')

# 1. Import libraries: load packages for data import, manipulation, and statistical tests.
library(readr)   # for reading CSV files
library(dplyr)   # for data manipulation and filtering
library(stats)   # for t.test(), shapiro.test(), and qt()
library(effsize) # for cohen.d effect size calculation

# 2. Load data: import the dataset from the CSV file.
data <- read_csv("dataset.csv", col_types = cols())

# 3. Apply exclusion criteria: remove participants with prime discrimination accuracy > 65% in the unconscious block.
# (Assuming col_6 == 0 indicates unconscious/ masked condition)
valid_participants <- data %>%
  filter(col_6 == 0) %>%
  group_by(col_0) %>%
  summarize(prime_acc = mean(col_13) * 100) %>%
  filter(prime_acc <= 65) %>%
  pull(col_0)

# 4. Filter to valid participants: keep only rows for participants meeting the exclusion criteria.
clean_data <- data %>%
  filter(col_0 %in% valid_participants)

# 5. Calculate prime accuracy per participant: compute percentage correct for primes.
# Filter for unconscious condition (col_6 == 0)
prime_acc <- clean_data %>%
  filter(col_6 == 0) %>%
  group_by(col_0) %>%
  summarize(accuracy = mean(col_13) * 100)

# 6. Calculate target accuracy per participant: compute percentage correct for targets.
# Filter for unconscious condition (col_6 == 0)
target_acc <- clean_data %>%
  filter(col_6 == 0) %>%
  group_by(col_0) %>%
  summarize(accuracy = mean(col_11) * 100)

# 7. Calculate descriptive statistics for primes: mean and 95% confidence interval.
n_prime <- nrow(prime_acc)
prime_mean <- mean(prime_acc$accuracy)
prime_sd <- sd(prime_acc$accuracy)
prime_se <- prime_sd / sqrt(n_prime)
t_crit <- qt(0.975, df = n_prime - 1)
prime_stats <- c(mean = prime_mean,
                 lower_ci = prime_mean - t_crit * prime_se,
                 upper_ci = prime_mean + t_crit * prime_se)

# 8. Calculate descriptive statistics for targets: mean and 95% confidence interval.
n_target <- nrow(target_acc)
target_mean <- mean(target_acc$accuracy)
target_sd <- sd(target_acc$accuracy)
target_se <- target_sd / sqrt(n_target)
t_crit_target <- qt(0.975, df = n_target - 1)
target_stats <- c(mean = target_mean,
                  lower_ci = target_mean - t_crit_target * target_se,
                  upper_ci = target_mean + t_crit_target * target_se)

# 9. Check normality assumption: test normality of the prime accuracy distribution.
normality_test <- shapiro.test(prime_acc$accuracy)

# 10. Perform one-sample t-test: test if prime accuracy differs from chance level (50%).
t_test_result <- t.test(prime_acc$accuracy, mu = 50)

# 11. Calculate effect size: Compute Cohen's d using the effsize package
cohen_d_result <- cohen.d(prime_acc$accuracy, mu = 50)

# 12. Format prime accuracy: format as 'XX.X [XX.X, XX.X]'.
prime_accuracy_text <- sprintf("%.1f [%.1f, %.1f]", 
                               prime_stats["mean"], prime_stats["lower_ci"], prime_stats["upper_ci"])

# 13. Format t-test results: format as 't(X) = X.XX, p = .XX'.
t_test_text <- sprintf("t(%d) = %.2f, p = %.2f", 
                       t_test_result$parameter, t_test_result$statistic, t_test_result$p.value)

# 14. Format target accuracy: format as 'XX.X [XX.X, XX.X]'.
target_accuracy_text <- sprintf("%.1f [%.1f, %.1f]", 
                                target_stats["mean"], target_stats["lower_ci"], target_stats["upper_ci"])

# 15. Compile final results: combine formatted prime accuracy, t-test results, and target accuracy.
final_results <- list(PrimeAccuracy = prime_accuracy_text,
                      TTest = t_test_text,
                      TargetAccuracy = target_accuracy_text)

# Print the final results to console.
print(final_results)
