setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_8')

# 1. import_libraries: Load required R packages.
library(tidyverse)   # For data manipulation and reading CSV.
library(car)         # For Levene's test and other diagnostic tests.
library(emmeans)     # To compute estimated marginal means.
library(lsr)         # For the etaSquared function to compute effect sizes.
library(effectsize)

# 2. read_data: Read dataset.
data <- read_csv("dataset.csv")

# 3. clean_and_recode: Convert variables to factors with proper coding.
data <- data %>%
  mutate(
    plev = factor(plev, levels = c(-1, 1), labels = c("low", "high")),
    vsex = factor(vsex, levels = c(-1, 1), labels = c("male", "female")),
    # Ensure acoustic variables are numeric
    pitch_smean = as.numeric(pitch_smean),
    pitch_svar = as.numeric(pitch_svar),
    intense_smean = as.numeric(intense_smean),
    intense_svar = as.numeric(intense_svar),
    form_smean = as.numeric(form_smean),
    form_svar = as.numeric(form_svar),
    pitch_rmean = as.numeric(pitch_rmean),
    pitch_rvar = as.numeric(pitch_rvar),
    intense_rmean = as.numeric(intense_rmean),
    intense_rvar = as.numeric(intense_rvar),
    form_rmean = as.numeric(form_rmean),
    form_rvar = as.numeric(form_rvar)
  )

# 4. isolate_target_subset: Select complete cases for relevant variables.
target_vars <- c("plev", "vsex",
                 "pitch_smean", "pitch_svar", "intense_smean", "intense_svar", "form_smean", "form_svar",
                 "pitch_rmean", "pitch_rvar", "intense_rmean", "intense_rvar", "form_rmean", "form_rvar")
data_subset <- data %>% 
  select(all_of(target_vars))

# 5. descriptive_summary: Calculate group means by plev and vsex.
desc_summary <- data_subset %>%
  group_by(plev, vsex) %>%
  summarise(
    mean_pitch_smean = mean(pitch_smean),
    mean_pitch_rmean = mean(pitch_rmean),
    mean_pitch_svar = mean(pitch_svar),
    mean_pitch_rvar = mean(pitch_rvar),
    mean_intense_smean = mean(intense_smean),
    mean_intense_rmean = mean(intense_rmean),
    mean_intense_svar = mean(intense_svar),
    mean_intense_rvar = mean(intense_rvar),
    mean_form_smean = mean(form_smean),
    mean_form_rmean = mean(form_rmean),
    mean_form_svar = mean(form_svar),
    mean_form_rvar = mean(form_rvar),
    .groups = "drop"
  )
print(desc_summary)

# 6. check_ancova_assumptions: For each acoustic cue, check assumptions.
# Define function to check assumptions for each model
check_assumptions <- function(model, outcome_var, covariate_var, data) {
  cat("\n=== Assumptions for", outcome_var, "===\n")
  
  # a. Linearity: Plot scatterplot
  plot(data[[covariate_var]], data[[outcome_var]], 
       main = paste("Scatterplot:", covariate_var, "vs.", outcome_var), 
       xlab = covariate_var, ylab = outcome_var)
  
  # b. Homogeneity of regression slopes: Check interaction significance
  cat("ANOVA table:\n")
  print(anova(model))
  
  # c. Normality of residuals
  cat("Shapiro-Wilk test for normality:\n")
  print(shapiro.test(residuals(model)))
  
  # d. Homoscedasticity: Levene's test
  cat("Levene's test for homoscedasticity:\n")
  print(leveneTest(residuals(model) ~ data$plev))
}

# Check assumptions for all six acoustic variables
model_pitch <- lm(pitch_smean ~ plev * vsex + pitch_rmean, data = data_subset)
check_assumptions(model_pitch, "pitch_smean", "pitch_rmean", data_subset)

model_pitch_var <- lm(pitch_svar ~ plev * vsex + pitch_rvar, data = data_subset)
check_assumptions(model_pitch_var, "pitch_svar", "pitch_rvar", data_subset)

model_intense <- lm(intense_smean ~ plev * vsex + intense_rmean, data = data_subset)
check_assumptions(model_intense, "intense_smean", "intense_rmean", data_subset)

model_intense_var <- lm(intense_svar ~ plev * vsex + intense_rvar, data = data_subset)
check_assumptions(model_intense_var, "intense_svar", "intense_rvar", data_subset)

model_form <- lm(form_smean ~ plev * vsex + form_rmean, data = data_subset)
check_assumptions(model_form, "form_smean", "form_rmean", data_subset)

model_form_var <- lm(form_svar ~ plev * vsex + form_rvar, data = data_subset)
check_assumptions(model_form_var, "form_svar", "form_rvar", data_subset)

# 7. run_primary_analysis: Run ANCOVA models for each acoustic cue.
# ANCOVA for pitch (mean)
ancova_pitch <- aov(pitch_smean ~ plev * vsex + pitch_rmean, data = data_subset)
summary(ancova_pitch)

# ANCOVA for pitch variability
ancova_pitch_var <- aov(pitch_svar ~ plev * vsex + pitch_rvar, data = data_subset)
summary(ancova_pitch_var)

# ANCOVA for loudness variability (intensity variability)
ancova_intense_var <- aov(intense_svar ~ plev * vsex + intense_rvar, data = data_subset)
summary(ancova_intense_var)

# ANCOVA for resonance (formant mean) - example additional cue
ancova_form <- aov(form_smean ~ plev * vsex + form_rmean, data = data_subset)
summary(ancova_form)

# ANCOVA for resonance variability (formant variance) - example additional cue
ancova_form_var <- aov(form_svar ~ plev * vsex + form_rvar, data = data_subset)
summary(ancova_form_var)

# ANCOVA for loudness (intensity mean) - example additional cue
ancova_intense <- aov(intense_smean ~ plev * vsex + intense_rmean, data = data_subset)
summary(ancova_intense)

# 8. calculate_effect_sizes: Calculate eta squared for each model.
eta_pitch <- effectsize::eta_squared(ancova_pitch, partial = TRUE)
eta_pitch_var <- effectsize::eta_squared(ancova_pitch_var, partial = TRUE)
eta_intense_var <- effectsize::eta_squared(ancova_intense_var, partial = TRUE)
eta_form <- effectsize::eta_squared(ancova_form, partial = TRUE)
eta_form_var <- effectsize::eta_squared(ancova_form_var, partial = TRUE)
eta_intense <- effectsize::eta_squared(ancova_intense, partial = TRUE)
print("Eta Squared for pitch model:")
print(eta_pitch)
print("Eta Squared for pitch variability model:")
print(eta_pitch_var)
print("Eta Squared for loudness variability model:")
print(eta_intense_var)

# 9. extract_adjusted_means: Obtain condition-adjusted means.
emm_pitch <- emmeans(ancova_pitch, "plev")
emm_pitch_var <- emmeans(ancova_pitch_var, "plev")
emm_intense_var <- emmeans(ancova_intense_var, "plev")
emm_form <- emmeans(ancova_form, "plev")
emm_form_var <- emmeans(ancova_form_var, "plev")
emm_intense <- emmeans(ancova_intense, "plev")
print("Adjusted means for pitch:")
print(emm_pitch)
print("Adjusted means for pitch variability:")
print(emm_pitch_var)
print("Adjusted means for loudness variability:")
print(emm_intense_var)

# 10. compile_results_table: Compile results into a summary table.
# Creating a summary table for three measures: pitch, pitch variability, and loudness variability.
# Convert emmeans to data frames for safer extraction
emm_pitch_df <- as.data.frame(emm_pitch)
emm_pitch_var_df <- as.data.frame(emm_pitch_var)
emm_intense_var_df <- as.data.frame(emm_intense_var)

results_table <- tibble(
  `Acoustic cue` = c("Pitch (Ƒ₀, in Hz)", "Pitch variability (Hz)", "Loudness variability (dB)"),
  `High-rank condition` = c(
    emm_pitch_df$emmean[emm_pitch_df$plev == "high"],
    emm_pitch_var_df$emmean[emm_pitch_var_df$plev == "high"],
    emm_intense_var_df$emmean[emm_intense_var_df$plev == "high"]
  ),
  `Low-rank condition` = c(
    emm_pitch_df$emmean[emm_pitch_df$plev == "low"],
    emm_pitch_var_df$emmean[emm_pitch_var_df$plev == "low"],
    emm_intense_var_df$emmean[emm_intense_var_df$plev == "low"]
  ),
  `Effect of condition: η²` = c(
    eta_pitch$Eta2_partial[1],
    eta_pitch_var$Eta2_partial[1],
    eta_intense_var$Eta2_partial[1]
  )
)
print(results_table)
