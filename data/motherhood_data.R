
library(tidyverse)
set.seed(90095)

# taken from principal stratification 
data <- readRDS("../data_raw/motherhood.RDS") |>
  select(
    treated, employed, age = age_2, sex, race, employed_baseline, 
    educ, marital, fulltime, tenure, experience,
    weight = w
  ) |>
  na.omit()

motherhood_simulated <- data |>
  filter(sex == "Women") |>
  select(-sex) |>
  group_by(treated, employed, race, employed_baseline, educ, marital, fulltime) |>
  filter(n() > 1) |>
  ungroup() |>
  slice_sample(prop = 1, replace = T) |>
  group_by(treated, employed, race, employed_baseline, educ, marital, fulltime) |>
  filter(n() > 1) |>
  mutate(across(
    c("age", "tenure", "experience", "weight"),
    \(x) rnorm(n(), mean(x), sd(x))
  )) |>
  # Enforce reasonable ranges on variables
  mutate(
    age = case_when(
      age < 18 ~ 18, 
      age > 45 ~ 45, 
      T ~ age
    ),
    weight = case_when(
      weight < .5 ~ .5, 
      T ~ weight
    ),
    experience = ifelse(experience > 0.1, experience, 0.1),
    tenure = case_when(
      tenure < .1 ~ .1,
      tenure > experience ~ experience,
      T ~ tenure
    )
  ) |>
  ungroup() |>
  mutate(observation_id = 1:n()) |>
  select(
    observation_id,
    sampling_weight = weight, treated, y = employed, race, pre_age = age, pre_educ = educ,
    pre_marital = marital, pre_employed = employed_baseline,
    pre_fulltime = fulltime, pre_tenure = tenure, pre_experience = experience
  )

write_csv(motherhood_simulated, file = "../data/motherhood_simulated.csv")