setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_12')

# Step 1: import_libraries: Load required packages
library(readr)     # To read CSV files into R data frames
library(dplyr)     # For data manipulation and transformation
library(stats)     # To perform statistical tests such as t.test and shapiro.test
library(effsize)   # For calculating effect sizes like Cohen's d
library(knitr)     # For neatly formatted tables with kable()

# Step 2: read_data: Load the dataset and assign it to a data frame
data <- read_csv('dataset.csv')


# Step 4: verify_data_types: Ensure required variables are numeric
numeric_vars <- c("T1_Curious", "T2_Curious", 
                  "T1_Interest_Composite", "T2_Interest_Composite",
                  "T1_Surprised", "T2_Surprised",
                  "T1_Meaningful", "T2_Meaningful",
                  "T1_Interesting", "T2_Interesting")
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)

# Step 5: isolate_target_subset: Select only the relevant columns for Study 1 analysis
data_subset <- data %>% select(T1_Curious, T2_Curious, 
                               T1_Interest_Composite, T2_Interest_Composite, 
                               T1_Surprised, T2_Surprised, 
                               T1_Meaningful, T2_Meaningful, 
                               T1_Interesting, T2_Interesting)

# Step 6: descriptive_summary: Compute descriptive statistics for each variable
desc_stats <- data_subset %>%
  summarise(
    mean_T1_Curious = mean(T1_Curious, na.rm = TRUE), sd_T1_Curious = sd(T1_Curious, na.rm = TRUE),
    mean_T2_Curious = mean(T2_Curious, na.rm = TRUE), sd_T2_Curious = sd(T2_Curious, na.rm = TRUE),
    mean_T1_Interest_Composite = mean(T1_Interest_Composite, na.rm = TRUE), sd_T1_Interest_Composite = sd(T1_Interest_Composite, na.rm = TRUE),
    mean_T2_Interest_Composite = mean(T2_Interest_Composite, na.rm = TRUE), sd_T2_Interest_Composite = sd(T2_Interest_Composite, na.rm = TRUE),
    mean_T1_Surprised = mean(T1_Surprised, na.rm = TRUE), sd_T1_Surprised = sd(T1_Surprised, na.rm = TRUE),
    mean_T2_Surprised = mean(T2_Surprised, na.rm = TRUE), sd_T2_Surprised = sd(T2_Surprised, na.rm = TRUE),
    mean_T1_Meaningful = mean(T1_Meaningful, na.rm = TRUE), sd_T1_Meaningful = sd(T1_Meaningful, na.rm = TRUE),
    mean_T2_Meaningful = mean(T2_Meaningful, na.rm = TRUE), sd_T2_Meaningful = sd(T2_Meaningful, na.rm = TRUE),
    mean_T1_Interesting = mean(T1_Interesting, na.rm = TRUE), sd_T1_Interesting = sd(T1_Interesting, na.rm = TRUE),
    mean_T2_Interesting = mean(T2_Interesting, na.rm = TRUE), sd_T2_Interesting = sd(T2_Interesting, na.rm = TRUE)
  )
print(desc_stats)

# Step 7: verify_normality_assumptions: Test normality of the paired differences
shapiro_curious <- shapiro.test(data_subset$T2_Curious - data_subset$T1_Curious)
shapiro_interest <- shapiro.test(data_subset$T2_Interest_Composite - data_subset$T1_Interest_Composite)
print(shapiro_curious)
print(shapiro_interest)

# Step 8: run_curiosity_ttest: Conduct a paired t-test for curiosity ratings
ttest_curious <- t.test(data_subset$T2_Curious, data_subset$T1_Curious, paired = TRUE, conf.level = 0.95)
print(ttest_curious)

# Step 9: run_interest_ttest: Conduct a paired t-test for interest composite ratings
ttest_interest <- t.test(data_subset$T2_Interest_Composite, data_subset$T1_Interest_Composite, paired = TRUE, conf.level = 0.95)
print(ttest_interest)

# Step 10: calculate_curiosity_effect_size: Calculate Cohen's d for curiosity paired samples
cohen_d_curious <- effsize::cohen.d(data_subset$T2_Curious, data_subset$T1_Curious)
print(cohen_d_curious)

# Step 11: calculate_interest_effect_size: Calculate Cohen's d for interest paired samples
cohen_d_interest <- effsize::cohen.d(data_subset$T2_Interest_Composite, data_subset$T1_Interest_Composite)
print(cohen_d_interest)

# Step 12: compile_results: Construct and display a table replicating Table 1 results
# Function to calculate confidence intervals for means
calc_ci <- function(x, conf.level = 0.95) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(n)
  alpha <- 1 - conf.level
  t_val <- qt(1 - alpha/2, df = n - 1)
  ci_lower <- mean_x - t_val * se
  ci_upper <- mean_x + t_val * se
  return(c(ci_lower, ci_upper))
}

# Create results table with 4 required columns
variables <- c("T1_Curious", "T2_Curious", "T1_Interest_Composite", "T2_Interest_Composite",
               "T1_Surprised", "T2_Surprised", "T1_Meaningful", "T2_Meaningful", 
               "T1_Interesting", "T2_Interesting")

# Initialize results table
results_table <- data.frame(
  Variable = character(),
  Time1_Mean_CI = character(),
  Time2_Mean_CI = character(),
  Difference_CI = character(),
  p_value = character(),
  stringsAsFactors = FALSE
)

# Process each pair of variables
var_pairs <- list(
  c("T1_Curious", "T2_Curious"),
  c("T1_Interest_Composite", "T2_Interest_Composite"),
  c("T1_Surprised", "T2_Surprised"),
  c("T1_Meaningful", "T2_Meaningful"),
  c("T1_Interesting", "T2_Interesting")
)

for (pair in var_pairs) {
  t1_var <- pair[1]
  t2_var <- pair[2]
  
  # Calculate means and CIs for Time 1 and Time 2
  t1_mean <- mean(data_subset[[t1_var]], na.rm = TRUE)
  t2_mean <- mean(data_subset[[t2_var]], na.rm = TRUE)
  
  t1_ci <- calc_ci(data_subset[[t1_var]])
  t2_ci <- calc_ci(data_subset[[t2_var]])
  
  # Calculate difference and its CI using paired t-test
  ttest_result <- t.test(data_subset[[t2_var]], data_subset[[t1_var]], paired = TRUE, conf.level = 0.95)
  diff_mean <- t2_mean - t1_mean
  diff_ci <- ttest_result$conf.int
  
  # Format results
  t1_mean_ci <- paste0(round(t1_mean, 2), " [", round(t1_ci[1], 2), ", ", round(t1_ci[2], 2), "]")
  t2_mean_ci <- paste0(round(t2_mean, 2), " [", round(t2_ci[1], 2), ", ", round(t2_ci[2], 2), "]")
  diff_ci_str <- paste0(round(diff_mean, 2), " [", round(diff_ci[1], 2), ", ", round(diff_ci[2], 2), "]")
  p_val <- ifelse(ttest_result$p.value < 0.001, "< 0.001", round(ttest_result$p.value, 3))
  
  # Add to results table
  results_table <- rbind(results_table, data.frame(
    Variable = gsub("T1_", "", t1_var),
    Time1_Mean_CI = t1_mean_ci,
    Time2_Mean_CI = t2_mean_ci,
    Difference_CI = diff_ci_str,
    p_value = p_val,
    stringsAsFactors = FALSE
  ))
}

# Display the results table
print(kable(results_table, caption = "Table 1 Replication: Descriptive and Inferential Statistics"))
