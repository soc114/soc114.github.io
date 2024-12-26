library(tidyverse)
theme_set(theme_bw())
library(haven)
library(foreach)

# Write a custom function we will use
weighted.quantile <- function(x, q, w) {
  result.df <- data.frame(x = x, w = w) %>%
    arrange(x) %>%
    mutate(cdf = cumsum(w) / sum(w)) %>%
    filter(cdf >= q) %>%
    filter(1:n() == 1)
  result <- result.df$x
  return(result)
}

# 1. LOAD DATA

# Load microdata from the Current Population Survey
# https://cps.ipums.org/
micro <- read_dta("../data_raw/cps_00062.dta") |>
  filter(year == 2022)

prepared <- micro |>
  # Remove missing household incomes
  filter(hhincome != 99999999) |>
  mutate(hhincome = case_when(hhincome < 0 ~ 0,
                              T ~ hhincome)) |>
  # Restrict to one row per household
  # instead of one row per person
  group_by(year, serial) %>%
  filter(1:n() == 1) |>
  ungroup()

summaries <- prepared |>
  summarize(median = weighted.quantile(hhincome, w = asecwth,
                                       q = .5),
            q10 = weighted.quantile(hhincome, w = asecwth,
                                    q = .1),
            q90 = weighted.quantile(hhincome, w = asecwth,
                                    q = .9),
            over_top = weighted.mean(hhincome > 500e3,
                                    w = asecwth),
            .groups = "drop") |>
  print()

# Big visualization of U.S. household income distribution

p <- prepared |>
  filter(hhincome < 500e3) |>
  ggplot(aes(x = hhincome, weight = asecwth)) +
  geom_histogram(binwidth = 25e3, alpha = .4) +
  scale_x_continuous(labels = scales::label_dollar(),
                     name = "Household Income") +
  scale_y_continuous(breaks = NULL,
                     name = "Density") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank())
p
ggsave(filename = "../slides/lec1/us2022_hh_dist_big.pdf",
       height = 3, width = 4.5)
p +
  geom_vline(xintercept = summaries$median,
             color = "blue", size = 2) +
  annotate(geom = "text", color = "blue", size = 4,
           x = 300e3, y = 2e7,
           label = paste("Median =",scales::dollar(summaries$median)),
           fontface = "bold")
ggsave("../slides/lec1/us2022_hh_dist_median.pdf",
       height = 3, width = 4.5)
p +
  geom_vline(xintercept = summaries$q90,
             color = "blue", size = 2) +
  geom_vline(xintercept = summaries$q10,
             color = "seagreen4", size = 2) +
  annotate(geom = "text", color = "blue", size = 4,
           x = 375e3, y = 2e7,
           label = paste0("90th percentile (",scales::dollar(summaries$q90),")"),
           fontface = "bold") +
  annotate(geom = "text", color = "black", size = 4,
           x = 375e3, y = 1.5e7,
           label = paste0("gets ",
                          round(summaries$q90 / summaries$q10),
                          "x as much as the")) +
  annotate(geom = "text", color = "seagreen4", size = 4,
           x = 375e3, y = 1e7,
           label = paste0("10th percentile (",scales::dollar(summaries$q10),")"),
           fontface = "bold")
ggsave("../slides/lec1/us2022_hh_dist_9010.pdf",
       height = 3, width = 4.5)


prepared |>
  filter(hhincome < 500e3) |>
  ggplot(aes(x = hhincome, weight = asecwth)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::label_dollar(),
                     name = "Household Income") +
  scale_y_continuous(breaks = NULL,
                     name = "Number of\nHouseholds") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank())
ggsave(filename = "../slides/lec1/us2022_hh_dist.pdf",
       height = 1.5, width = 3)

data.frame(hhincome = summaries$median) |>
  ggplot(aes(x = hhincome)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::label_dollar(),
                     name = "Household Income") +
  scale_y_continuous(breaks = NULL,
                     name = "Number of\nHouseholds") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank())
ggsave(filename = "../slides/lec1/equal_dist.pdf",
       height = 1.5, width = 3)


# Jencks 2002 table 1 graph

jencks_table1 <- read_csv("../data/jencks_table1.csv")

# Plot with the U.S.
p <- jencks_table1 |>
  filter(!(country %in% c("Russia","Mexico"))) |>
  ggplot(aes(x = ratio, y = gdp, label = country)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 3) +
  scale_y_continuous(labels = scales::label_percent(),
                     name = "GDP as a Percent of U.S.") +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile\nof household income")
p
ggsave(filename = "../slides/lec1/jencks_table1.pdf",
       height = 4, width = 4)

# Add linear fit
p +
  geom_smooth(method = "lm", se = F, color = "black")
ggsave(filename = "../slides/lec1/jencks_table1_line.pdf",
       height = 4, width = 4)

# Plot without the U.S.
jencks_table1 |>
  filter(!(country %in% c("Russia","Mexico"))) |>
  mutate(group = country == "US") |>
  ggplot(aes(x = ratio, y = gdp, label = country, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  ggrepel::geom_text_repel(size = 3) +
  scale_y_continuous(labels = scales::label_percent(),
                     name = "GDP as a Percent of U.S.") +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile\nof household income") +
  theme(legend.position = "none") +
  scale_color_manual(values = c("blue","seagreen4"))
ggsave(filename = "../slides/lec1/jencks_table1_dropUS.pdf",
       height = 4, width = 4)

jencks_table1 |>
  ggplot(aes(x = ratio, y = gdp, label = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black") +
  ggrepel::geom_text_repel(size = 3) +
  scale_y_continuous(labels = scales::label_percent(),
                     name = "GDP as a Percent of U.S.") +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile\nof household income") +
  theme(legend.position = "none")
ggsave(filename = "../slides/lec1/jencks_table1_all.pdf",
       height = 4, width = 4)

jencks_table1 |>
  filter(!(country %in% c("Russia","Mexico"))) |>
  ggplot(aes(x = ratio, y = life_expectancy, label = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black") +
  ggrepel::geom_text_repel(size = 3) +
  scale_y_continuous(labels = scales::label_percent(),
                     name = "Life Expectancy") +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile\nof household income") +
  theme(legend.position = "none")
ggsave(filename = "../slides/lec1/jencks_table1_health.pdf",
       height = 4, width = 4)

# Hypothetical plot for slide 1
jencks_table1 |>
  filter(!(country %in% c("Russia","Mexico"))) |>
  mutate(gdp = min(gdp) + (ratio - min(ratio)) / diff(range(ratio)) * diff(range(gdp))) |>
  ggplot(aes(x = ratio, y = gdp, label = country)) +
  geom_point(alpha = 0) +
  geom_line() +
  #ggrepel::geom_text_repel(size = 3) +
  scale_y_continuous(labels = scales::label_percent(),
                     name = "GDP as a Percent of U.S.") +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile\nof household income")

ggsave(filename = "../slides/lec1/jencks_table1_hypothetical.pdf",
       height = 4, width = 4)








# Load inflation adjustment data
# Consumer Price Index - All Urban Consumers
cpi_raw <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2022-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-01-22&revision_date=2023-01-22&nd=1947-01-01")

# 2. PREPARE INFLATION ADJUSTMENT

cpi <- cpi_raw %>%
  # Currently each row is a month.
  # Aggregate over months to take the average each year.
  mutate(year = as.numeric(gsub("-.*","",DATE))) %>%
  group_by(year) %>%
  summarize(cpi = mean(CPIAUCSL))

# We want to multiple dollars by `cpi` to get inflation-adjusted 2022 dollars
# To do that, make cpi the CPI in 2022 divided by the CPI in each year.
cpi$cpi <- cpi$cpi[cpi$year == 2022] / cpi$cpi


# 3. AGGREGATE MICRODATA (ON PEOPLE) TO YEAR-LEVEL DATA (ON THE POPULATION)

aggregated <- foreach(quantile.value = c(.1,.5,.9), .combine = "rbind") %do% {
  micro %>%
    # Remove missing household incomes
    filter(hhincome != 99999999) %>%
    filter(hhincome > 0) %>%
    # Restrict to one row per household
    # instead of one row per person
    group_by(year, serial) %>%
    filter(1:n() == 1) %>%
    # Within each year, calculate the chosen quantile of the distribution
    group_by(year) %>%
    summarize(income = weighted.quantile(x = hhincome, 
                                         q = quantile.value, 
                                         w = asecwth)) %>%
    mutate(quantity = paste0(100*quantile.value,"th percentile"))
}