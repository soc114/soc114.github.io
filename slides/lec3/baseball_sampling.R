

baseball <- read_csv("baseball.csv")

# Samples of size 60
# There are 30 teams

# Simple random sample
baseball |>
  # Draw a sample of size 60
  sample_n(size = 60) |>
  # Estimate the mean salary in the sample
  summarize(estimate = mean(salary))

# Stratified by team
baseball |>
  # Carry out steps within teams
  group_by(team) |>
  # Within each team, calculate some numbers
  mutate(
    number_to_sample = 2,
    number_players_on_team = n(),
    p_sampled = number_to_sample / 
      number_players_on_team,
    sampling_weight = 1 / p_sampled
  ) |>
  # Draw the sample
  sample_n(size = 2) |>
  ungroup() |>
  # Estimate the mean salary
  summarize(
    estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )
  )

# Clustered by team
baseball |>
  # Sample 3 teams
  distinct(team) |>
  sample_n(3) |>
  # Append information on all players from those teams
  left_join(baseball,
            by = join_by(team)) |>
  # Now operate within the 3 chosen teams
  group_by(team) |>
  # Within each team, calculate some numbers
  mutate(
    number_to_sample = 20,
    number_players_on_team = n(),
    p_sampled = 1 / number_players_on_team,
    sampling_weight = 1 / p_sampled
  ) |>
  # Draw the sample
  sample_n(20) |>
  # Calculate an estimate
  ungroup() |>
  # Estimate the mean salary
  summarize(
    estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )
  )

# Strategy B
baseball |>
  # Carry out steps within teams
  group_by(team) |>
  # Within each team, sample 2 players
  sample_n(size = 2)

# Strategy C
sampled_teams <- baseball |>
  # Make one row per team
  distinct(team) |>
  # Sample 3 teams
  sample_n(3) |>
  # Store those 3 team names in a vector
  pull()
# Load all the data
baseball |>
  # Restrict to the chosen teams
  filter(team %in% sampled_teams) |>
  # Within the 3 chosen teams,
  group_by(team) |>
  # sample 20 players
  sample_n(20) |>
  ungroup()





# Replicate many times

sample_estimates <- foreach(r = 1:10000, .combine = "rbind") %dopar% {
  
  simple <- baseball |>
    sample_n(60) |>
    summarize(estimate = mean(salary)) |>
    mutate(method = "Simple random sample\nSample 60 players at random")
  
  stratified <- baseball |>
    group_by(team) |>
    mutate(p_sampled = 2 / n()) |>
    sample_n(2) |>
    ungroup() |>
    summarize(estimate = weighted.mean(salary, w = 1 / p_sampled)) |>
    mutate(method = "Stratified random sample\nSample 2 players in each of the 30 teams")

  clustered <- baseball |>
    distinct(team) |>
    sample_n(3) |>
    left_join(baseball,
              by = join_by(team)) |>
    group_by(team) |>
    mutate(p_sampled = 20 / n()) |>
    sample_n(20) |>
    ungroup() |>
    summarize(estimate = weighted.mean(salary, w = 1 / p_sampled)) |>
    mutate(method = "Clustered random sample\nSample 3 teams, then 20 players per team.")
  
  return(
    simple |>
      bind_rows(stratified) |>
      bind_rows(clustered)
  )
}

sample_estimates |>
  mutate(method = fct_rev(method)) |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~ method, ncol = 1) +
  geom_vline(
    xintercept = baseball |>
      summarize(population_mean = mean(salary)) |>
      pull(population_mean)
  ) +
  scale_x_continuous(
    name = "Average Player Salary\n(Distribution over estimates over samples of size 60)",
    labels = label_dollar()
  ) +
  scale_y_continuous(name = "Count") +
  theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), "points"))
ggsave("baseball_sampling.pdf",
       height  = 6, width = 5)

# Note that stratified is in fact a bit better than simple
truth <- baseball |> summarize(salary = mean(salary)) |> pull(salary)
sample_estimates |>
  group_by(method) |>
  mutate(squared_error = (estimate - truth) ^ 2) |>
  summarize(mse = mean(squared_error),
            mse_se = sd(squared_error) / sqrt(n())) |>
  ggplot(aes(x = method, y = mse,
             ymin = mse - qnorm(.975) * mse_se,
             ymax = mse + qnorm(.975) * mse_se)) + 
  geom_point() +
  geom_errorbar()

sample_estimates |>
  mutate(rep = rep(1:(n() / 3), each = 3)) |>
  mutate(method = case_when(
    grepl("Stratified",method) ~ "stratified",
    grepl("Simple",method) ~ "simple"
  )) |>
  filter(!is.na(method)) |>
  pivot_wider(names_from = "method", values_from = "estimate") |>
  mutate(difference = (simple - truth) ^ 2 - (stratified - truth) ^ 2) |>
  summarize(estimate = mean(difference),
            se = sd(difference) / sqrt(n())) |>
  mutate(ci_min = estimate - qnorm(.975) * se,
         ci_max = estimate + qnorm(.975) * se) |>
  ggplot(aes(x = 1, y = estimate, ymin = ci_min, ymax = ci_max)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0)
  group_by(method) |>
  mutate(squared_error = (estimate - truth) ^ 2) |>
  summarize(mse = mean(squared_error),
            mse_se = sd(squared_error) / sqrt(n()))

# would be nice to just have histogram

sample_estimates |>
  group_by(method) |>
  summarize(standard_error = sd(estimate)) |>
  ungroup() |>
  ggplot(aes(y = method, x = standard_error)) +
  geom_point() +
  scale_x_continuous(
    name = "Standard deviation of estimates\nover repeated samples",
    labels = label_dollar()
  ) +
  ylab("Sampling Method")
sample_estimates |>
  group_by(method) |>
  summarize(
    se = sd(estimate) / sqrt(n()),
    estimate = mean(estimate),
    ci_min = estimate - qnorm(.975) * se,
    ci_max = estimate + qnorm(.975) * se
  ) |>
  ungroup() |>
  ggplot(aes(
    y = method, 
    x = estimate,
    xmin = ci_min,
    xmax = ci_max
  )) +
  geom_vline(xintercept = mean(baseball$salary)) +
  geom_point() +
  geom_errorbar(width = .1) +
  scale_x_continuous(
    name = "Average value of estimator\nover repeated samples",
    labels = label_dollar()
  ) +
  ylab("Sampling Method")
