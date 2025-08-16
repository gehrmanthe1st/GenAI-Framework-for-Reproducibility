setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_11')

# Step 1: import_libraries: Load required packages
library(readr)      # To import CSV data
library(dplyr)      # For data manipulation and summarization
library(tidyr)      # To reshape data (pivot_longer)
library(reshape2)   # Additional reshaping functions if needed
library(ez)         # To perform repeated measures ANOVA (ezANOVA)
library(car)        # For model assumption tests (e.g., Mauchly's test)
library(lsr)        # For computing effect sizes (etaSquared, cohensD)
library(emmeans)    # For post-hoc comparisons and estimated marginal means

# Step 2: read_data: Load the dataset and inspect its structure
data <- read_csv("dataset.csv")
str(data)  # Inspect structure to confirm 10 subjects plus one 'mean' row with 16 measurement columns

# Step 3: clean_and_recode: Remove 'mean' row and recode columns
data <- data %>% filter(subject != "mean")
data$subject <- as.factor(data$subject)
# Confirm measurement columns are numeric (assuming they are imported as numeric)

# Step 4: isolate_target_subset: Select only Experiment 1 measurement columns 
# (all columns containing 'grasping' or 'estimation' with '3cm' or '3.75cm')
data_subset <- data %>% 
  select(subject, matches("(closed-loop|open-loop)-(grasping|estimation)_(uncrowded|crowded)_(3cm|3\\.75cm)"))

# Step 5: descriptive_summary: Calculate descriptive statistics for each measurement condition
descriptive_stats <- data_subset %>% 
  summarise(across(-subject, list(mean = ~mean(. , na.rm = TRUE),
                                  sd = ~sd(. , na.rm = TRUE))))
print(descriptive_stats)

# Step 6: reshape_data: Convert data to long format
long_data <- data_subset %>%
  pivot_longer(cols = -subject,
               names_to = c("viewing", "task", "crowding", "target_size"),
               names_pattern = "(closed-loop|open-loop)-(grasping|estimation)_(uncrowded|crowded)_(3cm|3\\.75cm)",
               values_to = "value")
# Convert variables to factors where appropriate
long_data <- long_data %>%
  mutate(viewing = as.factor(viewing),
         task = as.factor(task),
         crowding = as.factor(crowding),
         target_size = as.factor(target_size))

# Step 7: verify_assumptions: Check normality (Shapiro-Wilk on residuals for each condition) and sphericity
# Fit a preliminary ANOVA model to extract residuals for assumption checks
aov_model <- aov(value ~ task * crowding * viewing * target_size + Error(subject/(task*crowding*viewing*target_size)), data = long_data)
# Extract residuals from the overall model (Note: this is a simplified extraction for illustration)
residuals_overall <- residuals(aov_model[[1]])
shapiro_result <- shapiro.test(residuals_overall)
print(shapiro_result)
# Sphericity can be checked using Mauchly's test on a repeated measures model if applicable
# (Here we assume a full repeated measures design and would normally extract covariance matrix for Mauchly's test)

# Step 8: run_primary_anova: Execute four-way repeated measures ANOVA using ezANOVA()
anova_results <- ezANOVA(
  data = long_data,
  dv = value,
  wid = subject,
  within = .(task, crowding, viewing, target_size),
  detailed = TRUE,
  type = 3
)
print(anova_results)

# Step 9: extract_interaction_effects: Extract and report the task × crowding interaction effect
# Here we extract from the ANOVA table from ezANOVA output
anova_table <- anova_results$ANOVA
interaction_effect <- anova_table %>% filter(Effect == "task:crowding")
print(interaction_effect)

# Step 10: run_posthoc_tests: Run paired t-tests for each specified condition comparison
# Create a list to store test results
t_test_results <- list()

# Define the combinations for conditions
conditions <- expand.grid(
  viewing = c("closed-loop", "open-loop"),
  task = c("estimation", "grasping"),
  crowding = c("uncrowded", "crowded"),
  stringsAsFactors = FALSE
)

# Iterate over each condition combination and perform paired t-test comparing target_size "3cm" and "3.75cm"
for(i in 1:nrow(conditions)) {
  cond <- conditions[i, ]
  subset_data <- long_data %>%
    filter(viewing == cond$viewing,
           task == cond$task,
           crowding == cond$crowding)
  
  # Reshape so that each row is a subject with two measurements corresponding to target sizes "3cm" and "3.75cm"
  wide_data <- subset_data %>%
    select(subject, target_size, value) %>%
    pivot_wider(names_from = target_size, values_from = value)
  
  # Perform paired t-test between "3cm" and "3.75cm"
  test <- t.test(wide_data$`3cm`, wide_data$`3.75cm`, paired = TRUE)
  
  # Save results keyed by condition
  condition_name <- paste(cond$viewing, cond$task, cond$crowding, sep = "_")
  t_test_results[[condition_name]] <- test
}
# Print all t-test results
print(t_test_results)

# Step 11: calculate_effect_sizes: Compute ANOVA effect sizes and Cohen's d for t-tests
# For ANOVA: Compute partial eta-squared from the aov model (using etaSquared from lsr)
anova_eta <- etaSquared(aov_model[[1]], anova = TRUE)
print(anova_eta)

# For each paired t-test, compute Cohen's d (using cohensD from lsr)
cohen_d_results <- list()
for(name in names(t_test_results)) {
  # Compute Cohen's d for paired samples
  d_val <- cohensD(t_test_results[[name]]$estimate)
  cohen_d_results[[name]] <- d_val
}
print(cohen_d_results)

# Step 12: validate_results: Cross-check computed statistics against reported values
# Print the relevant reported statistics for verification purposes
cat("Reported ANOVA interaction: F(1,9) = 6.818, p = .028\n")
print(interaction_effect)
cat("Reported t-test values for specific conditions should be compared manually with the t-test outputs printed above.\n")

# Step 13: compile_results: Create comprehensive result summaries and print to console
# ANOVA Summary
cat("----- ANOVA RESULTS -----\n")
print(anova_results)

# Formatted t-test results for each condition
cat("----- Paired t-test RESULTS -----\n")
for(name in names(t_test_results)) {
  cat("Condition:", name, "\n")
  print(t_test_results[[name]])
  cat("\n")
}

# Descriptive statistics summary
cat("----- DESCRIPTIVE STATISTICS -----\n")
print(descriptive_stats)