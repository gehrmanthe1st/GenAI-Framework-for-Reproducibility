setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_7')

# 1. import_libraries: Load required R packages for data manipulation, reading CSV, and tidying statistical test outputs.
library(dplyr)    # Data manipulation
library(readr)    # CSV file reading
library(broom)    # Tidying model outputs

# 2. read_data: Import dataset and store in data frame 'df_raw'.
df_raw <- read_csv('dataset.csv', col_types = cols())

# 3. validate_data: Check if expected columns exist.
required_cols <- c("gender", "race", "risk", "zIAT", "zattitude", "warmth_white", "warmth_black", "att_composite", "IAT", "attitude")
if(!all(required_cols %in% names(df_raw))) {
  stop("One or more required columns are missing in the dataset.")
}
print(dim(df_raw))  # Print dimensions for validation

# 4. clean_numeric_variables: Convert key variables to numeric.
df_raw <- df_raw %>% 
  mutate(
    risk = as.numeric(risk),
    zIAT = as.numeric(zIAT),
    zattitude = as.numeric(zattitude),
    warmth_white = as.numeric(warmth_white),
    warmth_black = as.numeric(warmth_black),
    att_composite = as.numeric(att_composite),
    IAT = as.numeric(IAT),
    attitude = as.numeric(attitude)
  )

# 5. handle_missing_data: Document missing data patterns and apply listwise deletion for key variables.
# (For demonstration, we simply count missing values.)
missing_summary <- sapply(df_raw[required_cols], function(x) sum(is.na(x)))
print(missing_summary)

# 6. filter_target_population: Isolate White women with valid conception risk and composite bias data.
df_study1 <- df_raw %>% 
  filter(gender == FALSE, race == 6, !is.na(risk), !is.na(att_composite))

# 7. validate_sample_size: Report the final sample size for Study 1.
sample_size <- nrow(df_study1)
print(paste("Final sample size for Study 1:", sample_size))

# 8. compute_descriptive_statistics: Calculate mean and standard deviation for key variables on full dataset.
desc_stats <- df_raw %>%
  summarise(
    mean_IAT = mean(IAT, na.rm = TRUE),
    sd_IAT = sd(IAT, na.rm = TRUE),
    mean_attitude = mean(attitude, na.rm = TRUE),
    sd_attitude = sd(attitude, na.rm = TRUE),
    mean_warmth_white = mean(warmth_white, na.rm = TRUE),
    sd_warmth_white = sd(warmth_white, na.rm = TRUE),
    mean_warmth_black = mean(warmth_black, na.rm = TRUE),
    sd_warmth_black = sd(warmth_black, na.rm = TRUE),
    mean_risk = mean(risk, na.rm = TRUE),
    sd_risk = sd(risk, na.rm = TRUE)
  )
print(desc_stats)

# 9. check_correlation_assumptions: Examine normality using Shapiro-Wilk tests for 'risk' and 'att_composite'.
shapiro_risk <- shapiro.test(df_study1$risk)
shapiro_att_comp <- shapiro.test(df_study1$att_composite)
print(shapiro_risk)
print(shapiro_att_comp)

# 10. run_correlation_analysis: Compute Pearson correlation between 'risk' and 'att_composite'.
cor_test_result <- cor.test(df_study1$risk, df_study1$att_composite, method = "pearson", conf.level = 0.95)

# 11. extract_correlation_results: Tidy the correlation test output to extract r, p-value, and 95% CI.
cor_results <- tidy(cor_test_result)
print(cor_results)

# 12. compile_final_results: Combine descriptive statistics and correlation results into one results data frame.
final_results <- list(
  descriptive_statistics = desc_stats,
  correlation_results = cor_results
)
print(final_results)

# 13. generate_summary_report: Print a formatted summary report to the console.
cat("\n--- Summary Report for Study 1 ---\n")
cat("Sample Size:", sample_size, "\n\n")
cat("Descriptive Statistics:\n")
print(desc_stats)
cat("\nCorrelation between Conception Risk and Composite Racial Bias:\n")
print(cor_results)
