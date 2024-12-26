
library(tidyverse)

# Wide version
baseball_3yrs_wide <- read_csv("../data/baseball_3yrs.csv") |>
  arrange(year) |>
  select(player, year, position, team, salary) |>
  pivot_wider(
    names_from = "year",
    values_from = c("salary","team","position")
  )

baseball_3yrs_wide |>
  write_csv("../data/baseball_3yrs_wide.csv")
