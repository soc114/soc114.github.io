
library(tidyverse)
library(haven)

set.seed(14850)

# Load data from IPUMS
from_ipums <- read_dta("../data_raw/cps_00088.dta")

# Create population means
parent_means <- from_ipums |>
  # Restrict to those with a child in the household
  filter(yngch != 99) |>
  # Restrict to those whose youngest child is no older than 18
  filter(yngch <= 18) |>
  # Restrict to those with a spouse
  filter(!is.na(sex_sp)) |>
  # Determine if they were at work last week
  # by being in universe for actual hours worked last week
  mutate(at_work = ahrsworkt != 999) |>
  # Improve names and labels of predictors
  mutate(
    sex = as_factor(sex),
    child_age = as.numeric(yngch)
  ) |>
  # Select focal variables
  select(at_work, child_age, sex, wtfinl) |>
  # Take weighted mean within years
  group_by(sex, child_age) |>
  summarize(
    at_work = weighted.mean(at_work, w = wtfinl),
    .groups = "drop"
  )
write_csv(parent_means, file = "../data/parent_means.csv")

# Create synthetic data
synthetic <- foreach(i = 1:nrow(parent_means), .combine = "rbind") %do% {
  draws <- rbinom(100, 1,parent_means$at_work[i])
  parent_means[i,] |>
    select(-at_work) |>
    bind_cols(tibble(at_work = draws))
}
write_csv(synthetic, file = "../data/parents.csv")
