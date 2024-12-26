
library(tidyverse)

crosswalk <- read_csv(url("https://osf.io/xb2yz/download"))
gss <- read_csv("../data/gss.csv")

inflation_factor <- read_csv("../data/inflation.csv") |>
  filter(year == 1986) |>
  pull(inflation_factor)
# DO THIS ALL IN SEPARATE FILE TO HAND THEM
gss_with_class <- gss |>
  filter(age >= 25 & age <= 69) |>
  left_join(crosswalk |>
              transmute(occ10 = occ10,
                        respondent_class = egp_label),
            by = "occ10") |>
  left_join(crosswalk |>
              transmute(paocc10 = occ10,
                        father_class = egp_label),
            by = "paocc10") |>
  left_join(crosswalk |>
              transmute(maocc10 = occ10,
                        mother_class = egp_label),
            by = "maocc10") |>
  mutate(
    across(contains("_class"),
           function(x) {
             case_when(x %in% "I" ~ "Higher Professional",
                       x %in% "II" ~ "Lower Professional",
                       x %in% c("IIIa","IVc","V","Military") ~ "Intermediate",
                       x %in% c("VI","IIIb","VIIa","VIIb") ~ "Working Class",
                       T ~ x)
           }
    )) |>
  mutate(parent_class = case_when(
    father_class == "Higher Professional" | mother_class == "Higher Professional" ~ "Higher Professional",
    father_class == "Lower Professional" | mother_class == "Lower Professional" ~ "Lower Professional",
    father_class == "Intermediate" | mother_class == "Intermediate" ~ "Intermediate",
    father_class == "Working Class" | mother_class == "Working Class" ~ "Working Class"
  )) |>
  filter(!is.na(parent_class)) |>
  select(-mother_class, -father_class) |>
  mutate(across(contains("_class"), \(x) factor(x, levels = c("Higher Professional","Lower Professional","Intermediate","Working Class"), ordered = T))) |>
  mutate(across(c("realrinc","realinc"), \(x) x*inflation_factor)) |>
  mutate(respondent_college = case_when(degree >= 0 ~ degree >= 3)) |>
  select(parent_class, respondent_class, realrinc, respondent_college, wtssall) |>
  filter(!is.na(parent_class) & !is.na(respondent_class) & !is.na(respondent_college) & wtssall > 0)

saveRDS(gss_with_class, file = "../data/pset4.RDS")
