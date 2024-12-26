
# Scrapes population of 2023 opening day baseball salaries from USA Today data

library(tidyverse)
library(doParallel)

sink("baseball.txt")
print(Sys.time())

cl <- makeCluster(detectCores())
registerDoParallel(cl)
t0 <- Sys.time()
players <- foreach(
  i = 1:(20*47 + 4), 
  .packages = "tidyverse",
  .combine = "rbind"
) %dopar% {
  tibble(
    line = read_lines(paste0("https://databases.usatoday.com/major-league-baseball-salaries-2023/",i))
  ) |>
    filter(grepl("h4",line) | grepl("h4",lag(line,1))) |>
    mutate(line = str_replace_all(line,"\t|<h4>|<p>|</h4>|</p>","")) |>
    filter(1:n() %in% 3:16) |>
    mutate(name = rep(c("variable","value"),7),
           item = rep(1:7, each = 2)) |>
    pivot_wider(names_from = "name", values_from = "line") |>
    select(-item) |>
    pivot_wider(names_from = "variable", values_from = "value")
}
spent <- difftime(t0,Sys.time())

players |>
  # simplify column names
  rename_with(tolower) |>
  rename_with(
    str_replace_all,
    pattern = " ",
    replacement = "_"
  ) |>
  # make dollar values numeric
  mutate(
    across(
      all_of(c("salary","total_value","average_annual")),
      function(x) {
        x <- str_remove_all(x,"[$]|,")
        x <- as.numeric(x)
        return(x)
      }
    )
  ) |>
  # fill in data for 1-year contracts
  mutate(
    years = str_trim(years),
    total_value = case_when(
      years != "" ~ total_value,
      years == "" ~ salary
    ),
    average_annual = case_when(
      years != "" ~ average_annual,
      years == "" ~ salary
    ),
    years = case_when(
      years != "" ~ years,
      years == "" ~ "1 (2023-23)"
    ),
  ) |>
  # separate years into two columns
  mutate(
    total_years = str_remove(years," .*|[(].*"),
    first_year = str_remove_all(years,".*[(]|-.*"),
    last_year = str_remove_all(years,".*-|[)]"),
    last_year = paste0("20",last_year)
  ) |>
  select(-years) |>
  rename(years = total_years) |>
  mutate(
    across(
      contains("year"),
      as.numeric
    )
  ) |>
  write_csv("../data/baseball.csv")

print(sessionInfo())
print(Sys.time())

