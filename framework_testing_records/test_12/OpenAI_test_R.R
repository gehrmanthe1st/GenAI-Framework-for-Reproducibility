setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_12')

# Step 1: import_libraries: Load required packages
library(readr)     # To read CSV files into R data frames
library(dplyr)     # For data manipulation and transformation
library(stats)     # To perform statistical tests such as t.test and shapiro.test
library(effsize)   # For calculating effect sizes like Cohen's d
library(knitr)     # For neatly formatted tables with kable()

# Step 2: read_data: Load the dataset and assign it to a data frame
data <- read_csv('dataset.csv')

# Step 3: clean_missing_data: Remove rows with missing values
data <- na.omit(data)

# Step 4: verify_data_types: Ensure required variables are numeric
numeric_vars <- c("T1_Curious", "T2_Curious", 
                  "T1_Interest_Composite", "T2_Interest_Composite",
                  "T1_Surprised", "T2_Surprised",
                  "T1_Meaningful", "T2_Meaningful",
                  "T1_Interesting", "T2_Interesting", "Order")
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)

# Step 5: isolate_target_subset: Select only the relevant columns for Study 1 analysis
data_subset <- data %>% select(T1_Curious, T2_Curious, 
                               T1_Interest_Composite, T2_Interest_Composite, 
                               T1_Surprised, T2_Surprised, 
                               T1_Meaningful, T2_Meaningful, 
                               T1_Interesting, T2_Interesting, Order)

# Step 6: descriptive_summary: Compute descriptive statistics for each variable
desc_stats <- data_subset %>%
  summarise(
    mean_T1_Curious = mean(T1_Curious), sd_T1_Curious = sd(T1_Curious),
    mean_T2_Curious = mean(T2_Curious), sd_T2_Curious = sd(T2_Curious),
    mean_T1_Interest_Composite = mean(T1_Interest_Composite), sd_T1_Interest_Composite = sd(T1_Interest_Composite),
    mean_T2_Interest_Composite = mean(T2_Interest_Composite), sd_T2_Interest_Composite = sd(T2_Interest_Composite),
    mean_T1_Surprised = mean(T1_Surprised), sd_T1_Surprised = sd(T1_Surprised),
    mean_T2_Surprised = mean(T2_Surprised), sd_T2_Surprised = sd(T2_Surprised),
    mean_T1_Meaningful = mean(T1_Meaningful), sd_T1_Meaningful = sd(T1_Meaningful),
    mean_T2_Meaningful = mean(T2_Meaningful), sd_T2_Meaningful = sd(T2_Meaningful),
    mean_T1_Interesting = mean(T1_Interesting), sd_T1_Interesting = sd(T1_Interesting),
    mean_T2_Interesting = mean(T2_Interesting), sd_T2_Interesting = sd(T2_Interesting)
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
# Create a data frame with descriptive and inferential statistics
results_table <- data.frame(
  Measure = c("T1_Curious", "T2_Curious", "T1_Interest_Composite", "T2_Interest_Composite",
              "T1_Surprised", "T2_Surprised", "T1_Meaningful", "T2_Meaningful", "T1_Interesting", "T2_Interesting"),
  Mean = c(round(as.numeric(desc_stats$mean_T1_Curious), 2),
           round(as.numeric(desc_stats$mean_T2_Curious), 2),
           round(as.numeric(desc_stats$mean_T1_Interest_Composite), 2),
           round(as.numeric(desc_stats$mean_T2_Interest_Composite), 2),
           round(as.numeric(desc_stats$mean_T1_Surprised), 2),
           round(as.numeric(desc_stats$mean_T2_Surprised), 2),
           round(as.numeric(desc_stats$mean_T1_Meaningful), 2),
           round(as.numeric(desc_stats$mean_T2_Meaningful), 2),
           round(as.numeric(desc_stats$mean_T1_Interesting), 2),
           round(as.numeric(desc_stats$mean_T2_Interesting), 2)),
  SD = c(round(as.numeric(desc_stats$sd_T1_Curious), 2),
         round(as.numeric(desc_stats$sd_T2_Curious), 2),
         round(as.numeric(desc_stats$sd_T1_Interest_Composite), 2),
         round(as.numeric(desc_stats$sd_T2_Interest_Composite), 2),
         round(as.numeric(desc_stats$sd_T1_Surprised), 2),
         round(as.numeric(desc_stats$sd_T2_Surprised), 2),
         round(as.numeric(desc_stats$sd_T1_Meaningful), 2),
         round(as.numeric(desc_stats$sd_T2_Meaningful), 2),
         round(as.numeric(desc_stats$sd_T1_Interesting), 2),
         round(as.numeric(desc_stats$sd_T2_Interesting), 2))
)

# Add inferential statistics for paired t-tests (curiosity and interest composite)
# Here, we assume degrees of freedom and effect sizes as computed; these values should match those reported in Table 1.
inferential_stats <- data.frame(
  Test = c("Curiosity", "Interest Composite"),
  t_value = c(round(ttest_curious$statistic, 2), round(ttest_interest$statistic, 2)),
  df = c(ttest_curious$parameter, ttest_interest$parameter),
  p_value = c(round(ttest_curious$p.value, 3), round(ttest_interest$p.value, 3)),
  cohen_d = c(round(cohen_d_curious$estimate, 2), round(cohen_d_interest$estimate, 2))
)

# Display the results table and inferential statistics table
print(kable(results_table, caption = "Descriptive Statistics Replicating Table 1"))
print(kable(inferential_stats, caption = "Inferential Statistics from Paired t-tests"))


