
library(tidyverse)

# Load inflation adjustment data
# Consumer Price Index - All Urban Consumers
cpi_raw <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2023-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-01-22&revision_date=2023-01-22&nd=1947-01-01")

cpi <- cpi_raw %>%
  # Currently each row is a month.
  # Aggregate over months to take the average each year.
  mutate(year = as.numeric(gsub("-.*","",DATE))) %>%
  group_by(year) %>%
  summarize(cpi = mean(CPIAUCSL))

# We want to multiple dollars by `cpi` to get inflation-adjusted 2022 dollars
# To do that, make cpi the CPI in 2022 divided by the CPI in each year.
cpi$inflation_factor <- cpi$cpi[cpi$year == 2023] / cpi$cpi

write_csv(cpi %>% select(year, inflation_factor), file = "../data/inflation.csv")
