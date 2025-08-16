setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_5')

# 1. import_libraries: Load necessary packages
library(readr)    # Read CSV files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping (wide to long)
library(ez)       # Repeated measures ANOVA and assumption tests
library(ggplot2)  # Plotting

# 2. read_data: Import the dataset from 'data.csv'
data <- read.csv("dataset.csv")

# 3. clean_and_recode: Recode Trust1Attrc0 to a factor with specified levels and create participant_id
data <- data %>%
  mutate(Trust1Attrc0 = factor(Trust1Attrc0, levels = c(0, 1), labels = c("Trustworthiness", "Attractiveness")),
         participant_id = row_number())

# 4. transform_to_long: Convert from wide to long format on columns starting with 'DFT_'
long_data <- data %>%
  pivot_longer(cols = starts_with("DFT_"), 
               names_to = "dft_trial", 
               values_to = "judgment")

# 5. extract_dft_info: Parse 'dft_trial' to extract dft_level and trial number using "_" as delimiter
long_data <- long_data %>%
  separate(dft_trial, into = c("discard", "dft_level", "trial"), sep = "_", convert = TRUE) %>%
  select(-discard)  # Remove the redundant column

# 6. average_across_trials: Group by participant_id, Trust1Attrc0, and dft_level then calculate mean judgment
anova_data <- long_data %>%
  group_by(participant_id, Trust1Attrc0, dft_level) %>%
  summarise(mean_judgment = mean(judgment), .groups = "drop")

# 7. prepare_anova_data: Convert dft_level to factor with 11 levels and ensure participant_id is a factor
anova_data <- anova_data %>%
  mutate(dft_level = factor(sprintf("%02d", as.numeric(dft_level)), 
                            levels = c("00", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100")),
         participant_id = as.factor(participant_id))

# 8. verify_assumptions: Run ezANOVA with return_aov = TRUE to obtain Mauchly's test for sphericity
anova_assumptions <- ezANOVA(data = anova_data,
                             dv = mean_judgment,
                             wid = participant_id,
                             within = dft_level,
                             between = Trust1Attrc0,
                             detailed = TRUE,
                             return_aov = TRUE)
print("Assumption check (Mauchly's test for sphericity):")
print(anova_assumptions$`Mauchly's Test for Sphericity`)

# 9. run_primary_analysis: Conduct repeated measures ANOVA
anova_results <- ezANOVA(data = anova_data,
                         dv = mean_judgment,
                         wid = participant_id,
                         within = dft_level,
                         between = Trust1Attrc0,
                         detailed = TRUE)
print("Repeated measures ANOVA results:")
print(anova_results)

# 10. extract_key_statistics: Extract key F-statistics, p-values, and partial eta-squared for DFT main effect and interaction
anova_table <- anova_results$ANOVA
# Assuming the rows in anova_table correspond to factors; extract by matching names
df_main <- anova_table[anova_table$Effect == "dft_level", ]
df_interaction <- anova_table[anova_table$Effect == "Trust1Attrc0:dft_level", ]
print("Key statistics for DFT main effect:")
print(df_main[, c("F", "p", "ges")])
print("Key statistics for DFT x Judgment Type interaction:")
print(df_interaction[, c("F", "p", "ges")])

# 11. validate_reported_values: Confirm values match reported statistics
cat("Validation of reported values:\n")
cat("Reported main effect: F(10,37)=4.05, p<.001, ηp²=0.52\n")
cat("Reported interaction: F(10,37)=5.95, p<.001, ηp²=0.62\n")
print("Please visually compare the extracted statistics above with the reported values.")

# 12. prepare_plotting_data: Calculate means and standard errors by Trust1Attrc0 and dft_level
plot_data <- anova_data %>%
  group_by(Trust1Attrc0, dft_level) %>%
  summarise(mean_judgment = mean(mean_judgment),
            se = sd(mean_judgment) / sqrt(n()), .groups = "drop") %>%
  mutate(dft_percentage = as.numeric(as.character(dft_level)))

# 13. generate_figure2: Create a line plot with error bars for mean judgment vs DFT percentage for each judgment type
figure2 <- ggplot(plot_data, aes(x = dft_percentage, y = mean_judgment, color = Trust1Attrc0, group = Trust1Attrc0)) +
  geom_line() +                                    # Connected lines by judgment type
  geom_point() +                                   # Data points
  geom_errorbar(aes(ymin = mean_judgment - se, ymax = mean_judgment + se), width = 2) +
  labs(x = "DFT (%)", y = "Mean Judgment", color = "Judgment Type") +
  theme_minimal()
print(figure2)

# 14. compile_results: Print ANOVA summary and display Figure 2
print("Final ANOVA Summary:")
print(anova_table)
print("Figure 2 generated above represents the mean judgments versus DFT percentage with error bars.")
