setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_6')
# 1. import_libraries: Load required R packages
library(readr)   # Import data from CSV files
library(dplyr)   # Data manipulation and summarization
library(ggplot2) # Plotting results
library(stats)   # Statistical tests such as chi-square

# 2. read_data: Load the dataset ensuring correct columns are imported
data <- read.csv("dataset.csv", colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric"))
# Verify the column names
print(names(data))

# 3. clean_and_recode: Convert variables to factors with specified labels and verify structure
data$condition <- factor(data$condition, levels = c(0, 1), labels = c("hypothetical", "real"))
data$exchangeinfo <- factor(data$exchangeinfo, levels = c(1, 2), labels = c("yes", "no"))
# Verify the data types
str(data)

# 4. create_analysis_subset: Extract relevant variables for chi-square analysis
analysis_data <- data %>% select(condition, exchangeinfo)

# 5. compute_descriptives: Calculate frequency counts and proportions for 'exchangeinfo' by 'condition'
freq_table <- table(analysis_data$condition, analysis_data$exchangeinfo)
prop_table <- prop.table(freq_table, margin = 1)
print("Frequency Table:")
print(freq_table)
print("Proportion Table:")
print(prop_table)

# 6. verify_chisq_assumptions: Check chi-square test assumptions using expected frequencies
chisq_test_temp <- chisq.test(freq_table)
expected_freq <- chisq_test_temp$expected
print("Expected Frequencies:")
print(expected_freq)
if(all(expected_freq >= 5)) {
  print("All expected frequencies are >= 5; chi-square assumptions met.")
} else {
  print("One or more expected frequencies are less than 5; reconsider using chi-square test.")
}

# 7. run_chisq_test: Perform chi-square test of independence
chisq_result <- chisq.test(freq_table)
print("Chi-square Test Result:")
print(chisq_result)

# 8. calculate_effect_size: Compute phi coefficient for chi-square test using sample size 132
sample_size <- 132
phi_coefficient <- sqrt(chisq_result$statistic / sample_size)
print(paste("Phi Coefficient:", round(phi_coefficient, 3)))

# 9. format_primary_results: Report chi-square results in APA format
chi_square_stat <- round(chisq_result$statistic, 2)
p_value <- round(chisq_result$p.value, 3)
apa_result <- paste0("χ²(1, N = 132) = ", chi_square_stat, ", p = ", p_value)
print("APA formatted result:")
print(apa_result)

# 10. compile_final_output: Present complete analysis results
# Calculate sample sizes for each condition
sample_sizes <- table(data$condition)
print("Sample sizes by condition:")
print(sample_sizes)

# Calculate acceptance rates for each condition from the 'yes' counts over total in each condition
acceptance_rates <- prop_table[,"yes"] * 100
print("Acceptance rates (in %) by condition:")
print(round(acceptance_rates, 1))

# Final output message with all key results
final_output <- list(
  sample_sizes = sample_sizes,
  acceptance_rates_percent = round(acceptance_rates, 1),
  chi_square_statistic = chi_square_stat,
  degrees_of_freedom = chisq_result$parameter,
  p_value = p_value,
  phi_coefficient = round(as.numeric(phi_coefficient), 3)
)
print("Final Analysis Output:")
print(final_output)
