

library(tidyverse)
library(scales)

population <- read_csv("../../data/baseball.csv")

# POPULATION

population_mean <- population |>
  summarize(truth = mean(salary)) |>
  pull(truth)

population |>
  ggplot(aes(x = salary)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(
    name = "Annual Salary",
    labels = label_dollar()
  ) +
  ylab("Count") +
  geom_vline(xintercept = pop_mean, linetype = "dashed") +
  annotate(
    geom = "text", x = pop_mean + 1e6, y = 300, 
    label = paste0("Population\nMean\n",label_dollar()(pop_mean)),
    hjust = 0
  ) +
  ggtitle(paste("Population of MLB players on rosters on Opening Day 2023")) +
  theme_classic()
ggsave("baseball_histogram.pdf",
       height = 4, width = 6.5)

population |>
  group_by(team) |>
  summarize(salary = mean(salary)) |>
  mutate(team = fct_reorder(team,salary)) |>
  arrange(-salary) |>
  ggplot(aes(y = team, x = salary, label = label_dollar()(salary))) +
  geom_point() +
  #geom_text(vjust = -1) +
  scale_x_continuous(
    name = "Mean Salary",
    labels = label_dollar(),
    limits = c(0e6,10e6)
  ) +
  ylab("Team") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = unit(c(5.5, 24, 5.5, 5.5), "points"))
ggsave("all_team_mean.pdf",
       height = 4.5, width = 6.5)

# Sample
# Strategy A
draw_sample_A <- function(population) {
  population |>
    # Define sampling probability and weight
    mutate(
      p_sampled = 60 / n(),
      sampling_weight = 1 / p_sampled
    ) |>
    # Sample 60 players
    sample_n(size = 60)
}
# Strategy B
draw_sample_B <- function(population) {
  population |>
    # Draw sample within each team
    group_by(team) |>
    # Define sampling probability and weight
    mutate(
      p_sampled = 2 / n(),
      sampling_weight = 1 / p_sampled
    ) |>
    # Within each team, sample 2 players
    sample_n(size = 2)
}
# Strategy C
draw_sample_C <- function(population) {
  
  # First, sample 3 teams
  sampled_teams <- population |>
    # Make one row per team
    distinct(team) |>
    # Sample 3 teams
    sample_n(3) |>
    # Store those 3 team names in a vector
    pull()
  
  # Load all data
  population |>
    # Restrict to the chosen teams
    filter(team %in% sampled_teams) |>
    # Define sampling probability and weight
    group_by(team) |>
    mutate(
      p_sampled = (3 / 30) * (20 / n()),
      sampling_weight = 1 / p_sampled
    ) |>
  # Sample 20 players
  sample_n(20) |>
    ungroup()
}


# Estimator

estimator <- function(sample) {
  estimate <- sample |>
    summarize(estimate = weighted.mean(
      x = salary,
      w = sampling_weight
    )) |>
    pull(estimate)
  return(estimate)
}

sample_A <- draw_sample_A(population)
estimate_A <- estimator(sample_A)

sample_B <- draw_sample_B(population)
estimate_B <- estimator(sample_B)

sample_C <- draw_sample_C(population)
estimate_C <- estimator(sample_C)

# Evaluate performance

estimates_A <- replicate(
  n = 1000,
  expr = {
    sample_A <- draw_sample_A(population)
    estimate_A <- estimator(sample_A)
    estimate_A
  }
)

tibble(estimate = estimates_A) |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = population_mean)

# 





# By team
baseball |>
  group_by(team) |>
  summarize(salary = mean(salary)) |>
  mutate(team = fct_reorder(team,salary)) |>
  arrange(-salary) |>
  slice_head(n = 3) |>
  ggplot(aes(y = team, x = salary, label = label_dollar()(salary))) +
  geom_point() +
  geom_text(vjust = -1) +
  scale_x_continuous(
    name = "Mean Salary",
    labels = label_dollar(),
    limits = c(3e6,10e6)
  ) +
  ylab("Team") +
  ggtitle("Mean salary") +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = unit(c(5.5, 9.5, 5.5, 5.5), "points"))
ggsave("high_team_mean.pdf",
       height = 2, width = 6.5)
baseball |>
  group_by(team) |>
  summarize(salary = median(salary)) |>
  mutate(team = fct_reorder(team,salary)) |>
  arrange(-salary) |>
  slice_head(n = 3) |>
  ggplot(aes(y = team, x = salary, label = label_dollar()(salary))) +
  geom_point() +
  geom_text(vjust = -1) +
  scale_x_continuous(
    name = "Median Salary",
    labels = label_dollar(),
    limits = c(3e6,10e6)
  ) +
  ylab("Team") +
  ggtitle("Median salary") +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = unit(c(5.5, 9.5, 5.5, 5.5), "points"))
ggsave("high_team_median.pdf",
       height = 2, width = 6.5)
baseball |>
  filter(team != "Toronto") |>
  group_by(team) |>
  summarize(salary = median(salary)) |>
  mutate(team = fct_reorder(team,salary)) |>
  arrange(-salary) |>
  slice_head(n = 3) |>
  ggplot(aes(y = team, x = salary, label = label_dollar()(salary))) +
  geom_point() +
  geom_text(vjust = -1) +
  scale_x_continuous(
    name = "Median Salary",
    labels = label_dollar(),
    limits = c(3e6,10e6)
  ) +
  ylab("Team") +
  ggtitle("Median salary, excluding Canada") +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = unit(c(5.5, 9.5, 5.5, 5.5), "points"))
ggsave("high_team_median_no_canada.pdf",
       height = 2, width = 6.5)

# By position
baseball |>
  group_by(position) |>
  summarize(salary = mean(salary)) |>
  mutate(position = fct_reorder(position, salary)) |>
  ggplot(aes(y = position, x = salary)) +
  geom_point()


# Example: RHP vs LHP
pitchers <- baseball |>
  filter(position == "LHP" | position == "RHP")

pitchers |>
  ggplot(aes(x = position, y = salary, label = player)) +
  #geom_jitter(width = .05, height = 0) +
  geom_point(alpha = .5) +
  geom_text(
    data = pitchers |>
      mutate(player = str_remove(player,",.*")) |>
      group_by(position) |>
      arrange(-salary) |>
      slice_head(n = 7) |>
      mutate(
        placement = case_when(
          1:n() %% 2 == 0 ~ -.05,
          1:n() %% 2 == 1 ~ .05
        ),
        set = case_when(
          position == "LHP" ~ 1,
          position == "RHP" ~ 2
        ),
        hjust = 1 - ceiling(placement)
      ),
    aes(x = set + placement,
        hjust = hjust),
    size = 3
  ) +
  scale_x_discrete(
    name = "Position",
    labels = function(x) {
      case_when(x == "LHP" ~ "Left Handed Pitcher",
                x == "RHP" ~ "Right Handed Pitcher")
    }
  ) +
  scale_y_continuous(
    name = "Salary",
    labels = label_dollar()
  )
ggsave("pitcher_points.pdf",
       height = 6, width = 5)

pitchers |>
  sample_n(40) |>
  group_by(position) |>
  summarize(salary = mean(salary))

test_difference <- function(data) {
  if (data |> nrow() <= 2) {
    stop("Error: Expects a data frame with more than 2 rows. There should be one row per player")
  }
  if (!any(data |> colnames() == "salary")) {
    stop("Error: Expects salary to be a column of the data")
  }
  if (!any(data |> colnames() == "position")) {
    stop("Error: Expects salary to be a column of the data")
  }
  if (data |> distinct(position) |> nrow() != 2) {
    stop("Error: Expects a data frame with 2 distinct positions")
  }
  
  # Produce estimate in each subgroup
  subgroup_estimates <- data |>
    group_by(position) |>
    summarize(estimate = mean(salary)) |>
    pivot_wider(names_from = "position", values_from = "estimate")
  
  # Produce difference estimate with significance test
  test_of_difference <- data |>
    arrange(position) |>
    group_by(position) |>
    summarize(estimate = mean(salary),
              estimate_var = var(salary) / n()) |>
    summarize(difference = -diff(estimate),
              standard_error = sqrt(sum(estimate_var))) |>
    mutate(
      ci_min = difference - qnorm(.975) * standard_error,
      ci_max = difference + qnorm(.975) * standard_error,
      p_value = 2 * pnorm(abs(difference) / standard_error, lower.tail = F),
      significant = p_value < .05
    )
  
  # Return a combined data frame
  return(
    subgroup_estimates |>
      bind_cols(test_of_difference)
  )
}

good_sample <- F
while(!good_sample) {
  random_seed <- round(10e3*runif(1))
  set.seed(random_seed)
  new_sample <- pitchers |>
    filter(position %in% c("LHP","RHP")) |>
    sample_n(40)
  tested <- test_difference(new_sample)
  good_sample <- tested$significant
}
print(random_seed)
forplot <- new_sample |>
  mutate(position = case_when(
    position == "LHP" ~ "Left-\nHanded",
    position == "RHP" ~ "Right-\nHanded"
  ))
forplot |>
  ggplot(aes(
    x = position, 
    y = salary,
    label = player
  )) +
  geom_point() +
  geom_text(
    data = forplot |> filter(position == "Right-\nHanded") |> filter(salary > 4e6),
    hjust = 0, 
    nudge_x = .1
  ) +
  geom_text(
    data = forplot |> filter(position == "Left-\nHanded") |> filter(salary > 4e6),
    hjust = 1, 
    nudge_x = -.1
  ) +
  scale_y_continuous(
    name = "Salary",
    labels = label_dollar()
  ) +
  scale_x_discrete(
    name = element_blank(),
    expand = expansion(mult = 2)
  ) +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  ggtitle("Sample of 40 Pitchers from Opening Day 2023")
ggsave("baseball_sample.pdf",
       height = 5, width = 5)

set.seed(6330)
pitchers |>
  filter(position %in% c("LHP","RHP")) |>
  sample_n(40) |>
  group_by(position) |>
  summarize(salary = mean(salary))
  
new_sample |>
  arrange(position) |>
  group_by(position) |>
  summarize(estimate = mean(salary),
            se = sd(salary) / sqrt(n())) %>%
  (
    function(.data) {
      .data |>
        bind_rows(
          .data |>
            summarize(estimate = -diff(estimate),
                      se = sqrt(sum(se ^ 2))) |>
            mutate(position = "Difference")
        )
    }
  ) |>
  mutate(
    ci_min = estimate - qnorm(.975) * se,
    ci_max = estimate + qnorm(.975) * se,
    position = case_when(
      position == "LHP" ~ "Left-Handed\nPitchers",
      position == "RHP" ~ "Right-Handed\nPitchers",
      position == "Difference" ~ "Difference"
    ),
    my_facet = case_when(
      position == "Difference" ~ "Difference",
      position != "Difference" ~ "Mean Salaries"
    ),
    my_facet = fct_rev(my_facet)
  ) |>
  ggplot(aes(
    x = position, 
    y = estimate,
    label = label_dollar()(estimate),
    ymin = ci_min, 
    ymax = ci_max
  )) +
  facet_grid(
    ~my_facet,
    scales = "free_x"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label() +
  scale_y_continuous(
    name = "Estimates from a Sample of 40 Pitchers",
    labels = label_dollar()
  ) +
  scale_x_discrete(name = element_blank()) +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12))
ggsave("baseball_one_sample_estimates.pdf",
       height = 5, width = 8)

pitchers |>
  sample_n(40) |>
  test_difference()

many_sample_estimates <- foreach(
  rep = 1:1000,
  .combine = "rbind"
) %do% {
  pitchers |>
    sample_n(40) |>
    test_difference()
}

pct_significant <- many_sample_estimates |>
  summarize(significant = mean(significant)) |>
  mutate(significant = paste0(round(100*significant),"%")) |>
  pull(significant)

truth <- pitchers |>
  group_by(position) |>
  summarize(estimate = mean(salary)) |>
  pivot_wider(names_from = "position", 
              values_from = "estimate") |>
  mutate(difference = LHP - RHP)

many_sample_estimates |>
  ggplot(aes(x = difference)) +
  geom_histogram(alpha = .6)  +
  geom_vline(
    xintercept = truth$difference,
    color = "blue",
    size = 1.2,
    linetype = "dashed"
  ) +
  annotate(
    geom = "text",
    x = truth$difference,
    y = 0,
    angle = 90,
    hjust = -.1,
    vjust = -.7,
    label = "Population Mean",
    color = "blue",
    fontface = "bold"
  ) +
  scale_x_continuous(
    name = "Distribution of Sample Estimates",
    labels = label_dollar()
  ) +
  ggtitle("Mean Salary Difference: Left - Right Handed Pitchers") +
  ylab("Count") +
  theme_bw()
ggsave("pitchers_sample_hist.pdf",
       height = 4, width = 5)

many_sample_estimates |>
  arrange(significant) |>
  ggplot(aes(x = 1, y = difference, color = significant, alpha = significant)) +
  geom_jitter(width = .2, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(
    name = "Statistically\nSignificant",
    values = c("gray","seagreen4")
  ) +
  scale_alpha_manual(
    name = "Statistically\nSignificant",
    values = c(.5,1)
  ) +
  scale_y_continuous(
    name = "Mean Salary Difference\nLeft - Right Handed Pitchers",
    labels = label_dollar(),
    breaks = seq(-5e6,5e6,2.5e6)
  ) +
  scale_x_continuous(
    limits = c(.7,1.3),
    name = element_blank(),
    breaks = NULL
  ) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  ggtitle(
    "Distribution of Sample Estimates",
    subtitle = paste(pct_significant,"of sample estimates are\nstatistically significant")
  )
ggsave("pitchers_sample_scatter.pdf",
       height = 5, width = 4)
  
many_sample_estimates |>
  arrange(difference) |>
  mutate(index = 1:n()) |>
  arrange(significant) |>
  ggplot(aes(x = index, y = difference, color = significant,
             ymin = ci_min, ymax = ci_max)) +
  #geom_point(size = .3) +
  geom_errorbar(width = 0) +
  #geom_point(data = forplot |> filter(significant)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(
    name = "Statistically Significant",
    values = c("gray","seagreen4")
  ) +
  scale_y_continuous(
    name = "Mean Salary Difference\nLeft - Right Handed Pitchers",
    labels = label_dollar()
  ) +
  scale_x_continuous(
    name = "Sample Estimates\nSorted by Estimate Value"
  ) +
  ggtitle(
    "Distribution of Sample Estimates",
    subtitle = paste(pct_significant,"of sample estimates are\nstatistically significant")
  )
ggsave("pitchers_sample_errorbars.pdf",
       height = 5, width = 8)  


# Which position gets the best contract?

baseball |>
  group_by(team) |>
  summarize(salary_mean = mean(salary),
            salary_median = median(salary)) |>
  mutate(team = fct_reorder(team, salary_median)) |>
  ggplot(aes(x = team, xend = team, y = salary_mean, yend = salary_median)) +
  geom_segment() +
  coord_flip()

# Does a typical player for Philadelphia or Seattle make more?

baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  group_by(team) |>
  summarize(median = median(salary),
            mean = mean(salary))

baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  ggplot(aes(x = team, y = salary)) +
  geom_jitter(width = .1, height = 0) +
  theme_classic()

baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  ggplot(aes(x = team, y = salary)) +
  geom_jitter(width = .1, height = 0) +
  theme_classic() +
  scale_y_continuous(
    name = "Salary", 
    labels = label_dollar(),
    limits = c(0,30e6)
  )

baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  summarize(
    salary = mean(salary),
    .by = team
  ) |>
  ggplot(aes(x = team, y = salary)) +
  geom_jitter(width = .1, height = 0) +
  theme_classic() +
  scale_y_continuous(
    name = "Salary", 
    labels = label_dollar(),
    limits = c(0,30e6)
  )
baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  summarize(
    salary = median(salary),
    .by = team
  ) |>
  ggplot(aes(x = team, y = salary)) +
  geom_jitter(width = .1, height = 0) +
  theme_classic() +
  scale_y_continuous(
    name = "Salary", 
    labels = label_dollar(),
    limits = c(0,30e6)
  )

baseball |>
  filter(team == "Philadelphia" | team == "Seattle") |>
  summarize(
    Median = median(salary),
    Mean = mean(salary),
    .by = team
  ) |>
  pivot_longer(
    cols = -team,
    names_to = "Summary\nMeasure",
    values_to = "salary"
  ) |>
  ggplot(aes(x = team, y = salary, color = `Summary\nMeasure`)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(
    name = "Salary", 
    labels = label_dollar(),
    limits = c(0,30e6)
  )



baseball |>
  summarize(
    salary = mean(salary),
    .by = years
  ) |>
  ggplot(aes(x = years, y = salary)) +
  geom_point()

# What could we do to make replication succeed?

players whose names start with XXX

something where the grouping is unclear

baseball |> distinct(team) |> print(n = Inf)

# who is the highest-paid XX in American baseball?

baseball |>
  group_by(position) |>
  filter(salary == max(salary))

baseball |>
  group_by(position) |>
  filter(total_value == max(total_value))

# which player has the best contract?
baseball |>
  arrange(-total_value)
baseball |>
  arrange(-salary)
baseball |>
  arrange(-years)

# What are the top 3 teams in terms of median salary in American baseball?
baseball |>
  group_by(team) |>
  summarize(salary = median(salary)) |>
  arrange(-salary)

# What is the median salary in American baseball?
baseball |>
  summarize(salary = median(salary))
baseball |>
  filter(team != "Toronto") |>
  summarize(salary = median(salary))



baseball |>yearsbaseball |>
  group_by(position) |>
  summarize(salary = mean(salary),
            num = n()) |>
  arrange(-salary)

baseball |>
  group_by(position) |>
  summarize(salary = mean(salary)) |>
  arrange(-salary)

baseball |>
  group_by(position) |>
  summarize(salary = median(salary)) |>
  arrange(-salary)

baseball |>
  group_by(team) |>
  summarize(salary = median(salary)) |>
  arrange(-salary)

baseball |>
  group_by(team) |>
  summarize(salary = mean(salary)) |>
  arrange(-salary)

baseball |>
  group_by(years) |>
  summarize(salary = mean(salary)) |>
  ggplot(aes(x = years, y = salary)) +
  geom_point()

baseball |>
  filter(years > 1) |>
  ggplot(aes(x = first_year, y = salary)) +
  geom_point()
  








