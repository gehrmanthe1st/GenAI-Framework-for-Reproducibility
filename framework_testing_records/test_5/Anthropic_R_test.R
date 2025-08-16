setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_5')

# 1. import_libraries: Load necessary packages
library(readr)    # Read CSV files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping (wide to long)
library(car)      # For multivariate ANOVA (Anova function with type III)
library(ggplot2)  # Plotting
library(effectsize)
library(heplots)

options(contrasts = c("contr.sum", "contr.poly"))

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
  summarise(mean_judgment = mean(judgment, na.rm = TRUE), .groups = "drop")

# 7. prepare_anova_data: Convert dft_level to factor with 11 levels and ensure participant_id is a factor
anova_data <- anova_data %>%
  mutate(dft_level = factor(sprintf("%02d", as.numeric(dft_level)), 
                            levels = c("00", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100")),
         participant_id = as.factor(participant_id))

# 8-9. run_multivariate_analysis: Use multivariate approach to get df(10,37)
# Reshape data to wide format for multivariate analysis - each DFT level as separate column
wide_anova_data <- anova_data %>%
  pivot_wider(names_from = dft_level, 
              values_from = mean_judgment, 
              names_prefix = "DFT_")

# Create matrix of DFT measurements for multivariate analysis
dft_cols <- paste0("DFT_", sprintf("%02d", seq(0, 100, 10)))   # 11 DV columns

# Define within-subjects factor 'factor1' (polynomial contrasts)
idata <- data.frame(factor1 = ordered(seq(0, 100, 10)))
rownames(idata) <- dft_cols
contrasts(idata$factor1) <- contr.poly(11)   # matches “Polynomial” in SPSS

# Fit the multivariate lm and run Type III repeated-measures tests
rm_fit <- Anova(
  lm(as.matrix(wide_anova_data[, dft_cols]) ~ Trust1Attrc0,
     data = wide_anova_data),
  idata      = idata,
  idesign    = ~ factor1,
  type       = "III",
  test.statistic = "Pillai"
)

cat("\n── Multivariate Tests (Pillai, Wilks, Roy, Hotelling) ──\n")
print(summary(rm_fit))                         # SPSS “Multivariate Tests”

cat("\n── Within-Subjects Effects + GG/HF sphericity corrections ──\n")
uni_tab <- summary(rm_fit, multivariate = FALSE)  # SPSS “Within-Subjects Effects”
print(uni_tab)

# Partial-η² for every effect (SPSS /PRINT=ETASQ)
eta_tab <- etasq(rm_fit, anova = TRUE)
cat("\n── Partial eta-squared (ηp²) ──\n")
print(eta_tab)

# Grab a single number, e.g. ηp² for DFT main effect
pes_DFT <- eta_tab["factor1", "eta^2"]

# 12. prepare_plotting_data: Calculate means and standard errors by Trust1Attrc0 and dft_level
plot_data <- anova_data %>%
  group_by(Trust1Attrc0, dft_level) %>%
  summarise(mean_judgment = mean(mean_judgment, na.rm = TRUE),
            se = sd(mean_judgment, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
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
print("Final Multivariate ANOVA Summary:")
print(repeated_manova)
print("Figure 2 generated above represents the mean judgments versus DFT percentage with error bars.")

# User addition
library(effectsize)
eta_out <- eta_squared(repeated_manova, partial = TRUE)
eta_out

# Anthropic R test for Hardwicke et al. (2023) using multivariate ANOVA
## 1  contrasts
options(contrasts = c("contr.sum", "contr.poly"))

## 2  idata
dft_cols <- paste0("DFT_", sprintf("%02d", seq(0, 100, 10)))
idata <- data.frame(factor1 = ordered(seq(0, 100, 10)))
rownames(idata) <- dft_cols
contrasts(idata$factor1) <- contr.poly(11)

## 3  repeated-measures GLM (Type III)
rm_fit <- Anova(
  lm(as.matrix(wide_anova_data[, dft_cols]) ~ Trust1Attrc0,
     data = wide_anova_data),
  idata   = idata,
  idesign = ~ factor1,
  type    = "III"
)

## 4  SPSS-style output
summary(rm_fit)                        # Multivariate Tests
uni_tab <- summary(rm_fit, multivariate = FALSE)   # Within-Subjects Effects
uni_tab

## 5  partial-η² for every effect
library(heplots)
etasq(rm_fit, anova = TRUE)

## 6  polynomial components (optional)
poly_tests <- uni_tab$`Univariate Tests`[
  grepl("^factor1\\(", rownames(uni_tab$`Univariate Tests`)), ]
poly_tests