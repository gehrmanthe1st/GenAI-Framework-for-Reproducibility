setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_13')

# 1. import_libraries: Load required R packages
library(dplyr)    # Data manipulation and summary statistics.
library(ggplot2)  # Creating bar plots with error bars.
library(readr)    # Reading CSV files.
library(car)      # Conducting ANOVA assumption checks and effect size calculations.
library(emmeans)  # Performing post hoc pairwise comparisons with Sidak adjustments.
library(effsize)  # Computing Cohen's d for effect size.
library(broom)    # Tidying model outputs into summary tables.
library(tidyr)
library(purrr)


# 2. read_data: Import the dataset and ensure all columns are imported, including OVERALL.
df <- read_csv("dataset.csv")

# 3. clean_and_prepare: Convert 'Age (years)' to numeric, recode 'Age Group' as a factor with specified levels, and subset columns.
df <- df %>%
  mutate(`Age (years)` = as.numeric(`Age (years)`),
         `Age Group` = factor(`Age Group`, levels = c("3-4 year olds", "5-6 year olds", "7-10 year olds", "Adults"))) %>%
  select(`Participant ID`, `Age (years)`, `Age Group`, NICE, STRONG, SMART, OVERALL)

# 4. calculate_descriptives:
# Compute overall means for NICE, STRONG, and SMART across all participants
overall_means <- df %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            mean_STRONG = mean(STRONG, na.rm = TRUE),
            mean_SMART = mean(SMART, na.rm = TRUE))
print("Overall Means for Traits:")
print(overall_means)

# Calculate mean consensus scores grouped by Age Group for each trait
group_means <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            mean_STRONG = mean(STRONG, na.rm = TRUE),
            mean_SMART = mean(SMART, na.rm = TRUE),
            mean_OVERALL = mean(OVERALL, na.rm = TRUE))
print("Mean Consensus by Age Group:")
print(group_means)

# 5. test_consensus_vs_chance: Perform one-sample t-tests for NICE, STRONG, and SMART against chance level (mu=0.5)
#t_test_NICE <- t.test(df$NICE, mu = 0.5)
#t_test_STRONG <- t.test(df$STRONG, mu = 0.5)
#t_test_SMART <- t.test(df$SMART, mu = 0.5)
#print("T-test results for NICE:")
#print(tidy(t_test_NICE))
#print("T-test results for STRONG:")
#print(tidy(t_test_STRONG))
#print("T-test results for SMART:")
#print(tidy(t_test_SMART))

# 6. calculate_effect_sizes_consensus: Compute Cohen's d for each one-sample t-test with mu = 0.5
#cohen_NICE <- cohen.d(df$NICE, f = NA, mu = 0.5)
#cohen_STRONG <- cohen.d(df$STRONG, f = NA, mu = 0.5)
#cohen_SMART <- cohen.d(df$SMART, f = NA, mu = 0.5)
#print("Cohen's d for NICE:")
#print(cohen_NICE)
#print("Cohen's d for STRONG:")
#print(cohen_STRONG)
#print("Cohen's d for SMART:")
#print(cohen_SMART)

# Revised calculations for the each trait for each age:

names(df)[names(df) == "Age Group"] <- "age_group"

df_long = df %>%
  pivot_longer(cols = c(NICE, STRONG, SMART), 
               names_to = "Trait", 
               values_to = "Score")
 
results <- df_long %>%
  group_by(age_group, Trait) %>%
  nest() %>%
  mutate(
    t_test_tbl = map(data, ~ broom::tidy(t.test(.x$Score, mu = 0.5))),
    cohens_val = map(data, ~ effsize::cohen.d(.x$Score, f=NA, mu=0.5)$estimate)
  ) %>%
  unnest(c(cohens_val, t_test_tbl)) %>%
  select(age_group, Trait,
         mean   = estimate,
         t_stat = statistic,
         p.value,
         cohens_val)

results

names(df)[names(df) == "age_group"] <- "Age Group"

# 7. run_anova_analyses: Perform separate one-way ANOVAs for NICE, STRONG, SMART, and OVERALL across Age Group
anova_NICE <- aov(NICE ~ `Age Group`, data = df)
anova_STRONG <- aov(STRONG ~ `Age Group`, data = df)
anova_SMART <- aov(SMART ~ `Age Group`, data = df)
anova_OVERALL <- aov(OVERALL ~ `Age Group`, data = df)
print("ANOVA results for NICE:")
print(summary(anova_NICE))
print("ANOVA results for STRONG:")
print(summary(anova_STRONG))
print("ANOVA results for SMART:")
print(summary(anova_SMART))
print("ANOVA results for OVERALL:")
print(summary(anova_OVERALL))

# 8. conduct_posthoc_tests: Run post hoc comparisons for NICE and STRONG using emmeans with pairwise comparisons and Sidak adjustment.
# For NICE:
emmeans_NICE <- emmeans(anova_NICE, "Age Group")
posthoc_NICE <- pairs(emmeans_NICE, adjust = "sidak")
print("Post hoc comparisons for NICE:")
print(posthoc_NICE)

posthoc_d_NICE = eff_size(posthoc_NICE, sigma = sigma(anova_NICE), edf = df.residual(anova_NICE),
                     method = 'identity')
summary(posthoc_d_NICE)

# For STRONG:
emmeans_STRONG <- emmeans(anova_STRONG, "Age Group")
posthoc_STRONG <- pairs(emmeans_STRONG, adjust = "sidak")
print("Post hoc comparisons for STRONG:")
print(posthoc_STRONG)

posthoc_d_STRONG = eff_size(posthoc_STRONG, sigma = sigma(anova_STRONG), edf = df.residual(anova_STRONG),
                     method = 'identity')
summary(posthoc_d_STRONG)

# 9. compile_statistical_results: Create a summary table
# Print combined overall means expected from the article reference:
expected_means <- data.frame(Trait = c("NICE", "STRONG", "SMART"),
                             Combined_Mean = c(0.93, 0.85, 0.76))
print("Expected Combined Overall Means for Traits (as per article):")
print(expected_means)
# Print t-test and effect size results have been printed above.
# Print group means from ANOVA (already stored in group_means).
# Print post hoc results (already printed above).

# 10. create_figure_2: Generate bar plot for NICE trait by Age Group with error bars (±1 SEM)
sem <- function(x) { sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))) }
plot_data_NICE <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_NICE = mean(NICE, na.rm = TRUE),
            sem_NICE = sem(NICE))
fig2 <- ggplot(plot_data_NICE, aes(x = `Age Group`, y = mean_NICE*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_NICE - sem_NICE)*100, ymax = (mean_NICE + sem_NICE)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (NICE)", x = "Age Group") +
  ggtitle("Fig. 2: NICE Trait Consensus by Age Group")
print(fig2)

# 11. create_figure_3: Generate bar plot for STRONG trait by Age Group with error bars (±1 SEM)
plot_data_STRONG <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_STRONG = mean(STRONG, na.rm = TRUE),
            sem_STRONG = sem(STRONG))
fig3 <- ggplot(plot_data_STRONG, aes(x = `Age Group`, y = mean_STRONG*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_STRONG - sem_STRONG)*100, ymax = (mean_STRONG + sem_STRONG)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (STRONG)", x = "Age Group") +
  ggtitle("Fig. 3: STRONG Trait Consensus by Age Group")
print(fig3)

# 12. create_figure_4: Generate bar plot for SMART trait by Age Group with error bars (±1 SEM)
plot_data_SMART <- df %>%
  group_by(`Age Group`) %>%
  summarise(mean_SMART = mean(SMART, na.rm = TRUE),
            sem_SMART = sem(SMART))
fig4 <- ggplot(plot_data_SMART, aes(x = `Age Group`, y = mean_SMART*100)) +
  geom_bar(stat = "identity", fill = "lightgray") +
  geom_errorbar(aes(ymin = (mean_SMART - sem_SMART)*100, ymax = (mean_SMART + sem_SMART)*100), width = 0.2) +
  ylim(0,100) +
  labs(y = "Percentage of Expected Responses (SMART)", x = "Age Group") +
  ggtitle("Fig. 4: SMART Trait Consensus by Age Group")
print(fig4)





############################################################################################################################
############################################################################################################################
############################################################################################################################
##### This is the revised calculation basis author assistance###############################################################
# Revised calculations for the each trait for each age:


names(df)[names(df) == "Age Group"] <- "age_group"

df_long = df %>%
  pivot_longer(cols = c(NICE, STRONG, SMART), 
               names_to = "Trait", 
               values_to = "Score")

results <- df_long %>%
  group_by(age_group, Trait) %>%
  nest() %>%
  mutate(
    t_test_tbl = map(data, ~ broom::tidy(t.test(.x$Score, mu = 0.5))),
    cohens_val = map_dbl(t_test_tbl, ~ 2 * .x$statistic / sqrt(.x$parameter))
  ) %>%
  unnest(c(cohens_val, t_test_tbl)) %>%
  select(age_group, Trait,
         mean   = estimate,
         t_stat = statistic,
         p.value,
         cohens_val)

results

names(df)[names(df) == "age_group"] <- "Age Group"

############################################################################################################################
############################################################################################################################
############################################################################################################################

library(tidyverse) # for data munging
library(knitr) # for kable table formating
library(haven) # import and export 'SPSS', 'Stata' and 'SAS' Files
library(readxl) # import excel files
library(ReproReports) # custom report functions
library(effsize) #used to calculate effect size
library(car)
library(lsr) # using this to get effect sizes
library(broom)

colnames(d) <- colnames(d) %>% 
  str_replace(pattern = " ", "_") %>%
  str_to_lower()

d_tidy <- d %>% gather(key = attribute, value = rating, nice:smart)

t_test_fun <- function(df) {
  m <- t.test(df$rating, mu = 0.5, alternative = "two.sided")
  m %>% broom::glance()
}

# nest the data for each attribute
d_by_attr <- d_tidy %>% 
  group_by(attribute) %>% 
  nest()

# map the t-test function against random responding (0.5) to each attribute
d_by_attr <- d_by_attr %>% 
  mutate(t_test = purrr::map(data, t_test_fun)) 

by_attr_t <- d_by_attr %>% unnest(t_test) %>% select(-data)
by_attr_t %>% kable(digits = 2)

d_by_age_and_attr <- d_tidy %>% 
  group_by(age_group, attribute) %>% 
  nest()

# map the t-test function against random responding (0.5) to each attribute
d_by_age_and_attr <- d_by_age_and_attr %>% 
  mutate(t_test = purrr::map(data, t_test_fun)) 

by_age_attr_t <- d_by_age_and_attr %>% unnest(t_test) %>% select(-data) %>% mutate(d = 2*statistic/sqrt(parameter))
by_age_attr_t %>% kable(digits = 4)


nice34_56 <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("3-4 year olds", "5-6 year olds"))
t.out <- pairwise.t.test(nice34_56$rating, nice34_56$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice34_56)
t.out;d

nice34_710 <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("3-4 year olds", "7-10 year olds"))
t.out <- pairwise.t.test(nice34_710$rating, nice34_710$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice34_710)
t.out;d

nice34_Adults <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("3-4 year olds", "Adults"))
t.out <- pairwise.t.test(nice34_Adults$rating, nice34_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice34_Adults)
t.out;d

nice56_710 <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("5-6 year olds", "7-10 year olds"))
t.out <- pairwise.t.test(nice56_710$rating, nice56_710$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice56_710)
t.out;d

nice56_Adults <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("5-6 year olds", "Adults"))
t.out <- pairwise.t.test(nice56_Adults$rating, nice56_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice56_Adults)
t.out;d

nice710_Adults <- d_tidy %>% filter(attribute == 'nice', age_group %in% c("7-10 year olds", "Adults"))
t.out <- pairwise.t.test(nice710_Adults$rating, nice710_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = nice710_Adults)
t.out;d

strong34_56 <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("3-4 year olds", "5-6 year olds"))
t.out <- pairwise.t.test(strong34_56$rating, strong34_56$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong34_56)
t.out;d

strong34_710 <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("3-4 year olds", "7-10 year olds"))
t.out <- pairwise.t.test(strong34_710$rating, strong34_710$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong34_710)
t.out;d

strong34_Adults <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("3-4 year olds", "Adults"))
t.out <- pairwise.t.test(strong34_Adults$rating, strong34_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong34_Adults)
t.out;d

strong56_710 <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("5-6 year olds", "7-10 year olds"))
t.out <- pairwise.t.test(strong56_710$rating, strong56_710$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong56_710)
t.out;d

strong56_Adults <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("5-6 year olds", "Adults"))
t.out <- pairwise.t.test(strong56_Adults$rating, strong56_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong56_Adults)
t.out;d

strong710_Adults <- d_tidy %>% filter(attribute == 'strong', age_group %in% c("7-10 year olds", "Adults"))
t.out <- pairwise.t.test(strong710_Adults$rating, strong710_Adults$age_group, paired = FALSE, p.adjust.method = 'none')
d <- cohensD(rating ~ age_group, data = strong710_Adults)
t.out;d
