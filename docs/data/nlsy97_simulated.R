set.seed(90095)
library(tidyverse)
library(haven)

d <- readRDS("data_raw/d.RDS")

d |>
  group_by(a, y, sex, race, mom_educ, dad_educ) |>
  filter(n() > 1) |>
  mutate(across(c("log_parent_income","log_parent_wealth","test_percentile"), \(x) rnorm(n(), mean(x), sd(x)))) |>
  # Randomly sample discrete variables jointly from empirical distribution
  ungroup() |>
  slice_sample(prop = 1, replace = T) |>
  mutate(id = 1:n()) |>
  # Enforce reasonable bounds
  mutate(
    log_parent_wealth = ifelse(log_parent_wealth < 1, 1, log_parent_wealth),
    log_parent_income = ifelse(log_parent_income < 1, 1, log_parent_income),
    test_percentile = case_when(
      test_percentile < 0 ~ 0,
      test_percentile > 100 ~ 100,
      T ~ test_percentile
    )
  ) |>
  mutate(id = 1:n()) |>
  mutate(
    a = case_when(
      a == "college" ~ "treated",
      a == "no_college" ~ "untreated"
    )
  ) |>
  mutate(sampling_weight = runif(n(), .95, 1.05)) |>
  select(id, sampling_weight, a, y, everything()) |>
  write_csv("data/nlsy97_simulated.csv")
  
