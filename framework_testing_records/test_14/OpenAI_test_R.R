setwd('C:/Users/harsh/OneDrive/Documents/course/thesis/testing_hardwicke/test_14')

# 1. import_libraries – Load required R packages for data input, manipulation, and reporting.
library(readr)   # Read CSV files
library(dplyr)   # Data manipulation functions
library(tidyr)   # Data reshaping
library(stats)   # Statistical tests
library(knitr)   # Table formatting for output

# 2. read_data – Load the dataset from 'new_data.csv'
data <- read_csv("dataset.csv")

# 3. recode_game_situations – Create game_situation factor from keeper_score
data <- data %>%
  mutate(game_situation = case_when(
    keeper_score == "a_behind" ~ "Behind",
    keeper_score == "atie" ~ "Tied",
    keeper_score == "z_ahead" ~ "Ahead",
    TRUE ~ NA_character_
  ))


dive_sum <- data$dive_right + data$dive_left
if (any(dive_sum != 0 & abs(dive_sum - 1) > 1e-6)) {
  warning("Some observations in dive_right and dive_left do not sum to 1 (excluding center dives)")
}

# Check kick: same logic
kick_sum <- data$kick_right + data$kick_left
if (any(kick_sum != 0 & abs(kick_sum - 1) > 1e-6)) {
  warning("Some observations in kick_right and kick_left do not sum to 1 (excluding center kicks)")
}

# 5. calculate_basic_percentages – Compute percentages for dive direction by game_situation
basic_pct <- data %>%
  group_by(game_situation) %>%
  summarise(
    n = n(),
    percent_dive_right = mean(dive_right, na.rm = TRUE) * 100,
    percent_dive_left = mean(dive_left, na.rm = TRUE) * 100
  )
print(basic_pct)

# 6. calculate_kick_direction_percentages – Compute percentages for kick direction by game_situation
kick_pct <- data %>%
  group_by(game_situation) %>%
  summarise(
    percent_kick_goalie_right = mean(kick_right, na.rm = TRUE) * 100,
    percent_kick_goalie_left = mean(kick_left, na.rm = TRUE) * 100
  )
print(kick_pct)

# 7. calculate_success_rates_by_dive – Compute success rates (goal) for each dive direction by game_situation
success_rates <- data %>%
  group_by(game_situation) %>%
  summarise(
    success_when_dive_right = ifelse(sum(dive_right == 1, na.rm = TRUE) > 0,
                                     mean(goal[dive_right == 1], na.rm = TRUE) * 100, NA),
    success_when_dive_left = ifelse(sum(dive_left == 1, na.rm = TRUE) > 0,
                                    mean(goal[dive_left == 1], na.rm = TRUE) * 100, NA)
  )
print(success_rates)

# Combine the summaries for game_situation grouping
summary_game_situation <- basic_pct %>%
  left_join(kick_pct, by = "game_situation") %>%
  left_join(success_rates, by = "game_situation")

# 8. calculate_binary_comparison – Compute percentages for new_score grouping (binary: behind vs not behind)
binary_summary <- data %>%
  group_by(new_score) %>%
  summarise(
    n = n(),
    percent_dive_right = mean(dive_right, na.rm = TRUE) * 100,
    percent_dive_left = mean(dive_left, na.rm = TRUE) * 100,
    percent_kick_goalie_right = mean(kick_right, na.rm = TRUE) * 100,
    percent_kick_goalie_left = mean(kick_left, na.rm = TRUE) * 100,
    success_when_dive_right = ifelse(sum(dive_right == 1, na.rm = TRUE) > 0,
                                     mean(goal[dive_right == 1], na.rm = TRUE) * 100, NA),
    success_when_dive_left = ifelse(sum(dive_left == 1, na.rm = TRUE) > 0,
                                    mean(goal[dive_left == 1], na.rm = TRUE) * 100, NA)
  )
print(binary_summary)

# 9. test_statistical_significance – Test differences in diving right proportions between 'Behind' and others
behind_data <- data %>% filter(game_situation == "Behind")
non_behind_data <- data %>% filter(game_situation != "Behind")
x <- c(sum(behind_data$dive_right, na.rm = TRUE), sum(non_behind_data$dive_right, na.rm = TRUE))
n_counts <- c(nrow(behind_data), nrow(non_behind_data))
prop_test_result <- prop.test(x = x, n = n_counts, conf.level = 0.95)
print(prop_test_result)

# 10. compile_table1_newdata – Create formatted table (Table 1) for new data
# For game_situation grouping: expect rows Behind (n = 32), Tied (n = 118), Ahead (n = 90)
table1_game_situation <- summary_game_situation %>%
  mutate(Group = case_when(
    game_situation == "Behind" ~ "Behind (n = 32)",
    game_situation == "Tied" ~ "Tied (n = 118)",
    game_situation == "Ahead" ~ "Ahead (n = 90)"
  )) %>%
  select(Group, percent_dive_right, percent_dive_left,
         percent_kick_goalie_right, percent_kick_goalie_left,
         success_when_dive_right, success_when_dive_left)
print(knitr::kable(table1_game_situation, digits = 1))

# For binary new_score grouping: expect rows Behind (n = 72) and Not behind (n = 168)
table1_binary <- binary_summary %>%
  mutate(Group = case_when(
    new_score == "behind" ~ "Behind (n = 72)",
    new_score == "not behind" ~ "Not behind (n = 168)"
  )) %>%
  select(Group, percent_dive_right, percent_dive_left,
         percent_kick_goalie_right, percent_kick_goalie_left,
         success_when_dive_right, success_when_dive_left)
print(knitr::kable(table1_binary, digits = 1))

# 11. document_statistical_conclusions – Report p-value and significance conclusion
cat("Proportion test p-value for diving right across game situations:", prop_test_result$p.value, "\n")
if(prop_test_result$p.value < 0.05) {
  cat("Differences are statistically significant at the 0.05 level.\n")
} else {
  cat("Differences are not statistically significant at the 0.05 level.\n")
}

