set.seed(90095)
library(tidyverse)
library(haven)

d <- readRDS("data_raw/d.RDS")

d |>
  mutate(index = 1:n()) |>
  group_by(a, y, sex, race, mom_educ, dad_educ) |>
  # Replace continuous values with random draws
  pivot_longer(cols = c("log_parent_income","log_parent_wealth","test_percentile")) |>
  mutate(
    mean = mean(value), 
    sd = sd(value),
    sd = ifelse(is.na(sd), mean(sd, na.rm = T), sd)
  ) |>
  select(-value) |>
  pivot_wider(names_from = name, values_from = c("mean","sd")) |>
  ungroup() |>
  # Randomly sample discrete variables jointly from empirical distribution
  slice_sample(prop = 1, replace = T) |>
  # Randomly draw continuous values from conditional distribution given discrete variables
  mutate(
    log_parent_income = rnorm(n(), mean_log_parent_income, sd_log_parent_income),
    log_parent_wealth = rnorm(n(), mean_log_parent_wealth, sd_log_parent_wealth),
    test_percentile = rnorm(n(), mean_test_percentile, sd_test_percentile),
    # Enforce reasonable bounds
    log_parent_wealth = ifelse(log_parent_wealth < 1, 1, log_parent_wealth),
    log_parent_income = ifelse(log_parent_income < 1, 1, log_parent_income),
    test_percentile = case_when(
      test_percentile < 0 ~ 0,
      test_percentile > 100 ~ 100,
      T ~ test_percentile
    )
  ) |>
  select(-contains("mean"), -contains("sd")) |>
  select(-index) |>
  saveRDS("data/nlsy97_simulated.RDS")
  
