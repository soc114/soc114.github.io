

library(tidyverse)
library(scales)
library(foreach)

baseball <- read_csv("https://info3370.github.io/data/baseball.csv")

# Light up dots for sample
# Add sample estimates to other plot

# Standings from https://www.baseball-reference.com/leagues/majors/2022-standings.shtml
record <- read_csv("/Users/ilundberg/Dropbox/github/info3370.github.io/data_raw/mlb_record_2022.csv") |>
  select(team = Tm, record = `W-L%`) |>
  # convert team names to format of other dataset
  filter(team != "Average") |>
  mutate(team = case_when(
    team == "Chicago Cubs" ~ team,
    team == "Chicago White Sox" ~ team,
    team == "Kansas City Royals" ~ "Kansas City",
    team == "Los Angeles Dodgers" ~ "L.A. Dodgers",
    team == "Los Angeles Angels" ~ "L.A. Angels",
    grepl("New York",team) ~ gsub("New York","N.Y.",team),
    team == "San Diego Padres" ~ "San Diego",
    team == "San Francisco Giants" ~ "San Francisco",
    team == "St. Louis Cardinals" ~ "St. Louis",
    team == "Tampa Bay Rays" ~ "Tampa Bay",
    T ~ str_replace_all(team," .*","")
  ))

# Goal: Mean salary of the Dodgers in 2023
# Estimators:
# - sample mean of everyone
# - sample mean of the Dodgers
# - regression prediction

population <- baseball |>
  left_join(record, by = "team") |>
  select(player, team, salary, record) |>
  mutate(target_subgroup = team == "L.A. Dodgers")

write_csv(population, file = "data/baseball_with_record.csv")

truth <- population |>
  filter(target_subgroup) |>
  summarize(truth = mean(salary)) |>
  pull(truth)

# Visualize the three strategies
set.seed(90090)
sample <- population |>
  group_by(team) |>
  sample_n(5) |>
  ungroup()

dodger_placement <- sample |>
  filter(target_subgroup) |>
  summarize(x = mean(record),
            y = max(salary))

# First show the sampling

base_plot0 <- sample |>
  ggplot(aes(x = record, y = salary)) +
  annotate(
    geom = "rect", 
    xmin = dodger_placement$x - .01,
    xmax = dodger_placement$x + .01,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgray",
    alpha = .3
  ) +
  theme_classic() +
  scale_color_manual(values = c("lightgray","dodgerblue")) +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Player Salary", breaks = NULL) +
  scale_x_continuous(name = "Past Win-Loss Record", breaks = NULL)

base_plot0 +
  geom_point(data = population) +
  annotate(
    geom = "text", 
    x = dodger_placement$x, 
    y = max(population$salary),
    label = "Dodgers",
    angle = 270, size = 3, hjust = 0
  ) +
  coord_cartesian(ylim = range(population$salary)) +
  ggtitle("Population")
ggsave("../slides/lec4/dodgers_population.pdf",
       height = 3, width = 3)

base_plot0 +
  geom_point(data = sample) +
  annotate(
    geom = "text", 
    x = dodger_placement$x, 
    y = max(population$salary),
    label = "Dodgers",
    angle = 270, size = 3, hjust = 0
  ) +
  coord_cartesian(ylim = range(population$salary)) +
  ggtitle("Sample: 5 per team")
ggsave("../slides/lec4/dodgers_sample.pdf",
       height = 3, width = 3)


base_plot <- sample |>
  ggplot(aes(x = record, y = salary)) +
  annotate(
    geom = "rect", 
    xmin = dodger_placement$x - .01,
    xmax = dodger_placement$x + .01,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgray",
    alpha = .3
  ) +
  annotate(
    geom = "text", 
    x = dodger_placement$x, 
    y = dodger_placement$y + 5e6,
    label = "Dodgers",
    angle = 270, size = 3, hjust = 1
  ) +
  theme_classic() +
  scale_color_manual(values = c("lightgray","dodgerblue")) +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Player Salary", breaks = NULL) +
  scale_x_continuous(name = "Past Win-Loss Record", breaks = NULL)

base_plot +
  geom_point(aes(color = target_subgroup)) +
  ggtitle("Estimator 1: Subgroup sample mean")
ggsave("../slides/lec4/dodgers_estimator1.pdf",
       height = 2, width = 3.5)
base_plot +
  geom_point(color = "dodgerblue") +
  ggtitle("Estimator 2: Full sample mean")
ggsave("../slides/lec4/dodgers_estimator2.pdf",
       height = 2, width = 3.5)
fit <- lm(salary ~ record, data = sample)
fitted <- sample |> mutate(fitted = predict(fit))
base_plot +
  geom_point(color = "gray") +
  geom_line(data = fitted,
            aes(y = fitted),
            color = "dodgerblue",
            size = 1.2) +
  geom_point(data = fitted |> filter(team == "L.A. Dodgers"),
             aes(y = fitted),
             color = "dodgerblue", size = 3) +
  ggtitle("Estimator 3: Regression prediction")
ggsave("../slides/lec4/dodgers_estimator3.pdf",
       height = 2, width = 3.5)

full_sample_mean <- function(sample) {
  sample |>
    summarize(estimate = mean(salary)) |>
    mutate(estimator = "Mean: 150 Sampled MLB Players")
}

subgroup_sample_mean <- function(sample) {
  sample |>
    filter(target_subgroup) |>
    summarize(estimate = mean(salary)) |>
    mutate(estimator = "Mean: 5 Sampled Dodgers")
}

ols_prediction <- function(sample) {
  fit <- lm(salary ~ record, data = sample)
  to_predict <- sample |>
    filter(team == "L.A. Dodgers") |>
    distinct(record)
  tibble(
    estimate = predict(fit, newdata = to_predict),
    estimator = "OLS Prediction: Learn on 150 MLB Players, Predict for Dodgers"
  )
}

# SAMPLES OF 5 PER TEAM
# MIDDLE GROUND OLS WINS?
# 20 PER TEAM TEAM SPECIFIC WINS

repeated_sample_estimates <- foreach(
  rep = 1:1000, 
  .combine = "rbind",
  .packages = "tidyverse"
) %dopar% {
  sample <- population |>
    group_by(team) |>
    sample_n(10) |>
    ungroup()
  full_sample_mean(sample) |>
    bind_rows(
      subgroup_sample_mean(sample)
    ) |>
    bind_rows(
      ols_prediction(sample)
    )
}

repeated_sample_estimates |>
  ggplot(aes(x = estimate)) +
  geom_histogram(fill = "dodgerblue", alpha = .8) +
  geom_vline(xintercept = truth, linetype = "dashed") +
  facet_wrap(~estimator, ncol = 3,
             labeller =  as_labeller(function(x) {
               x |>
                 str_replace_all(":",":\n") |>
                 str_replace_all(",",",\n")
             })) +
  theme_bw() +
  ylab("Number of Simulated Samples") +
  scale_x_continuous(name = "Sample Estimate of Mean Dodger Salary",
                     labels = label_dollar(scale = 1e-6, suffix = "m"))
ggsave("dodgers_estimator_histogram.pdf",
       height = 2.5, width = 8)

# Which one is the best? Root mean squared error
repeated_sample_estimates |>
  mutate(
    error = estimate - truth,
    squared_error = error ^ 2
  ) |>
  group_by(estimator) |>
  summarize(
    bias = mean(error),
    standard_deviation = sd(error),
    mean_squared_error = mean(squared_error)
  ) |>
  mutate(
    root_mean_squared_error = sqrt(mean_squared_error)
  ) |>
  select(estimator, bias, standard_deviation, root_mean_squared_error) |>
  mutate(estimator = fct_reorder(estimator, standard_deviation)) |>
  pivot_longer(
    cols = -estimator,
    names_to = "summary",
    values_to = "estimate"
  ) |>
  mutate(summary = fct_relevel(summary, "bias", "standard_deviation", "root_mean_squared_error")) |>
  ggplot(aes(y = estimator, x = estimate)) +
  geom_point() +
  facet_wrap(~summary, ncol = 3,
             labeller = as_labeller(function(x) {
               case_when(
                 x == "bias" ~ "Bias:\nHow does the average\nvalue of the estimator\ndiffer from the truth?",
                 x == "standard_deviation" ~ "Standard Deviation:\nHow much does the\nestimator change from\nsample to sample?",
                 x == "root_mean_squared_error" ~ "Root Mean Squared Error:\nHow far is the estimator\nfrom the truth, on average?\n"
               )
             })) +
  scale_y_discrete(
    labels = as_labeller(function(x) {
      str_replace_all(x,":|,","\n")
    }),
    name = "Estimator"
  ) +
  geom_vline(xintercept = 0) +
  theme(strip.text = element_text(hjust = 0)) +
  scale_x_continuous(
    name = "Value Estimated by Aggregating Repeated Samples",
    labels = label_dollar(
      scale = 1 / 1e6,
      suffix = "m"
    )
  )
ggsave("../slides/lec4/dodger_performance_metrics.pdf",
       height = 3, width = 7)


# TO DO:
# - find right middle ground of sample size
# - consider loess or cubic regression splines
# - get code on website

# prepare data to illustrate learners
set.seed(90090)
sample <- population |>
  group_by(team) |>
  sample_n(20) |>
  ungroup()
to_predict <- population |>
  filter(team == "L.A. Dodgers") |>
  distinct(team, record)
to_predict_all <- population |>
  distinct(team, record, target_subgroup)

# Function to aid plotting
plot_fit <- function(data_with_fitted, step = F) {
  if (step) {
    p <- data_with_fitted |>
      ggplot(aes(x = record, y = fitted)) +
      geom_step()
  } else {
    p <- data_with_fitted |>
      ggplot(aes(x = record, y = fitted)) +
      geom_line()
  }
  p +
    geom_point(aes(color = target_subgroup), size = 2) +
    scale_color_manual(values = c("black","dodgerblue")) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_continuous(
      name = "Team Mean Salary",
      labels = label_dollar(scale = 1e-6, suffix = "m"),
      limits = c(0,10e6)
    ) +
    scale_x_continuous(name = "Past Team Win-Loss Record")
}

# OLS
fit <- lm(
  salary ~ record, 
  data = sample
)
to_predict |>
  mutate(yhat = predict(
    fit,
    newdata = to_predict
  ))
plot_fit(
  to_predict_all |>
    mutate(fitted = predict(fit, newdata = to_predict_all))
)

# Ridge (maybe don't show code because somewhat hard)
library(glmnet)
x <- model.matrix(~record, data = sample)
fit <- cv.glmnet(
  x = model.matrix(~record, data = sample),
  y = sample$salary,
  alpha = 0
)
to_predict |>
  mutate(yhat = predict(
    fit,
    newx = model.matrix(~record, data = to_predict),
    s = "lambda.min"
  ))
plot_fit(
  to_predict_all |>
    mutate(fitted = predict(
      fit, 
      model.matrix(~record, data = to_predict_all),
      s = "lambda.min")
    )
)

# Spline
library(mgcv)
fit <- gam(
  salary ~ s(record),
  data = sample
)
to_predict |>
  mutate(yhat = predict(
    fit,
    newdata = to_predict
  ))
plot_fit(
  to_predict_all |>
    mutate(fitted = predict(fit, newdata = to_predict_all))
)

# Tree
library(rpart)
fit <- rpart(
  salary ~ record,
  data = sample
)
to_predict |>
  mutate(yhat = predict(
    fit,
    newdata = to_predict
  ))
plot_fit(
  population |>
    mutate(fitted = predict(fit, newdata = population)),
  step = T
)
ggsave("tree.pdf",
       height = 2, width = 5)

# ILLUSTRATE OLS
set.seed(90266)
sample <- population |>
  sample_n(30)
fit <- lm(salary ~ record, data = sample)
sample |>
  mutate(fitted = predict(fit)) |>
  ggplot(aes(x = record, y = salary)) +
  geom_segment(aes(xend = record, yend = fitted), color = "gray") +
  geom_point(data = sample, size = .5) +
  geom_line(aes(y = fitted)) +
  scale_y_continuous(
    name = "Mean Salary on Team",
    labels = label_dollar(scale = 1e-6, suffix = "m")
  ) +
  scale_x_continuous(
    name = "Past Team Win-Loss Record"
  )
ggsave("illustrate_ols.pdf",
       height = 3, width = 6)

# ILLUSTRATE PENALIZED
ridge_comparison <- function(n) {
  sample <- population |>
    sample_n(n)
  fit_ols <- lm(salary ~ record,
                data = sample)
  fit_ridge <- gam(salary ~ s(record, bs = "re"),
                   data = sample)
 to_predict_all |>
    mutate(Model = "OLS Regression",
           salary = predict(fit_ols, newdata = to_predict_all)) |>
    bind_rows(
      to_predict_all |>
        mutate(Model = "L2 Penalized Regression",
               salary = predict(fit_ridge, newdata = to_predict_all))
    ) |>
   ggplot(aes(x = record, y = salary)) +
   geom_point(data = sample, size = .5) +
   geom_line(aes(color = Model)) +
   scale_y_continuous(
     name = "Mean Salary on Team",
     labels = label_dollar(scale = 1e-6, suffix = "m")
   ) +
   scale_x_continuous(
     name = "Past Team Win-Loss Record"
   ) +
   ggtitle(paste("Sample of size",n))
}
set.seed(14853)
ridge_comparison(n = 5)
ggsave("ridge_comparison_1.pdf",
       height = 2, width = 5)
ridge_comparison(n = 30)
ggsave("ridge_comparison_2.pdf",
       height = 2, width = 5)
ridge_comparison(n = 300)
ggsave("ridge_comparison_3.pdf",
       height = 2, width = 5)


# ILLUSTRATE SPLINES
set.seed(14850)
sample <- population |>
  sample_n(100)
knot_values <- c(.4,.6)
to_predict <- tibble(
  record = c(knots,seq(.34,.68,.01))
)
make_plot <- function(degree_val) {
  fit <- lm(salary ~ splines::bs(record, degree = degree_val, knots = knot_values),
            data = sample)
  to_predict |>
    mutate(fitted = predict(fit, newdata = to_predict)) |>
    ggplot(aes(x = record, y = fitted)) +
    geom_line() +
    geom_point(aes(x = case_when(record %in% knot_values ~ record))) +
    scale_y_continuous(
      name = "Mean Salary on Team",
      labels = label_dollar(scale = 1e-6, suffix = "m")
    ) +
    scale_x_continuous(
      name = "Past Team Win-Loss Record"
    )
}
make_plot(degree_val = 1) +
  ggtitle("Linear spline")
ggsave("linear_spline.pdf",
       height = 2, width = 5)
make_plot(degree_val = 2) +
  ggtitle("Quadratic spline")
ggsave("quadratic_spline.pdf",
       height = 2, width = 5)
make_plot(degree_val = 3) +
  ggtitle("Cubic spline")
ggsave("cubic_spline.pdf",
       height = 2, width = 5)


# IMPERFECT MODELS

pop_fit <- lm(salary ~ record, data = population)
forplot <- population %>%
  group_by(team, record, target_subgroup) %>%
  summarize(salary = mean(salary),.groups = "drop")
forplot |>
  mutate(fitted = predict(pop_fit, newdata = forplot)) |>
  ggplot(aes(x = record, y = salary)) +
  geom_point(aes(color = target_subgroup)) +
  geom_line(aes(y = fitted, linetype = "Regression in\nFull\nPopulation")) +
  scale_color_manual(
    values = c("lightgray","dodgerblue"),
    name = element_blank(),
    labels = function(x) ifelse(x,"Dodgers","Other teams"),
  ) +
  scale_x_continuous(
    name = "Percent of Games Team Won Last Season",
    labels = label_percent()
  ) +
  scale_y_continuous(
    name = "Mean Player Salary This Season",
    labels = label_dollar(scale = 1e-6, suffix = "m")
  ) +
  scale_linetype_discrete(name = element_blank()) +
  theme_classic()
ggsave("dodger_model_wrong.pdf",
       height = 3, width = 6)

#





population <- baseball |>
  left_join(record, by = "team") |>
  mutate(fielder = position %in% c("1B","2B","SS","3B","OF")) |>
  mutate(subgroup_to_estimate = team == "L.A. Dodgers" & fielder)# |>
  #mutate(salary = log(salary))

population |>
  ggplot(aes(x = record, y = salary, color = subgroup_to_estimate)) +
  geom_point() +
  scale_color_manual(
    values = c("lightgray","dodgerblue"),
    name = element_blank(),
    labels = function(x) ifelse(x,"L.A. Dodger fielders\n(infield and outfield)","Other MLB players"),
  ) +
  scale_x_continuous(
    name = "Percent of Games Team Won Last Season",
    labels = label_percent()
  ) +
  scale_y_continuous(
    name = "Player Salary This Season",
    labels = label_dollar()
  ) +
  theme_classic()

record |>
  arrange(record) |>
  print(n = Inf)

truth <- population |>
  filter(subgroup_to_estimate) |>
  select(salary) |>
  summarize_all(mean) |>
  pull(salary)

set.seed(14850)
simulate <- function() {
  sample <- population |>
    group_by(team, fielder) |>
    mutate(
      p_sampled = 2 / n(),
      sampling_weight = 1 / p_sampled
    ) |>
    sample_n(size = 2) |>
    ungroup()
  
  sample |>
    filter(subgroup_to_estimate) |>
    summarize(estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )) |>
    mutate(method = "Sample mean: Dodger fielders") ->
    estimate_1
  
  sample |>
    filter(team == "L.A. Dodgers") |>
    summarize(estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )) |>
    mutate(method = "Sample mean: All Dodgers") ->
    estimate_2
  
  sample |>
    filter(fielder) |>
    summarize(estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )) |>
    mutate(method = "Sample mean: All fielders") ->
    estimate_3
  
  # Linear regression prediction
  fit <- lm(
    salary ~ record + fielder,
    data = sample,
    weights = sampling_weight
  )
  to_predict <- population |>
    filter(subgroup_to_estimate)
  to_predict |>
    mutate(fitted = predict(fit, newdata = to_predict)) |>
    summarize(estimate = mean(fitted)) |>
    mutate(method = "Linear regression") ->
    estimate_4
  
  # Loess regression prediction
  sample_normalized_weight <- sample |> 
    mutate(sampling_weight = sampling_weight / mean(sampling_weight))

  fit <- gam(
    salary ~ fielder + s(record),
    data = sample_normalized_weight,
    weights = sampling_weight
  )
  
  population |>
    mutate(fitted = predict(fit, newdata = population)) |>
    ggplot(aes(x = record)) +
    geom_point(aes(y = salary)) +
    geom_line(aes(y = fitted)) +
    facet_wrap(~fielder)
  
  to_predict <- population |>
    filter(subgroup_to_estimate) |>
    select(team, fielder, record) |>
    distinct()
  to_predict |>
    mutate(fitted = predict(fit, newdata = to_predict)) |>
    summarize(estimate = mean(fitted)) |>
    mutate(method = "Linear regression among fielders") ->
    estimate_5
  
  estimate_1 |>
    bind_rows(estimate_2) |>
    bind_rows(estimate_3) |>
    bind_rows(estimate_4) |>
    bind_rows(estimate_5)
}

library(foreach)
reps <- foreach(i = 1:100, .combine = "rbind") %do% {
  simulate()
}

reps |>
  group_by(method) |>
  mutate(RMSE = sqrt(mean((estimate - truth) ^ 2))) |>
  ungroup() |>
  mutate(method = paste0(method,"\nRMSE = ", round(RMSE / 1e3),"k"),
         method = fct_reorder(method, RMSE)) |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = truth, linetype = "dashed") +
  facet_wrap(~method, ncol = 1)

# could scrape 2022 and look at year - year prediction

  population |>
    arrange(subgroup_to_estimate) |>
    ggplot(aes(x = record, y = salary)) +
    geom_point(aes(color = subgroup_to_estimate), 
               size = 3) +
    scale_color_manual(values = c("lightgray","dodgerblue")) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_x_continuous(
      name = "Percent of Games Won Last Season",
      labels = label_percent()
    ) +
    scale_y_continuous(
      name = "Player Salary This Season",
      labels = label_dollar()
    ) +
    geom_smooth(method = "loess", se = F, color = "dodgerblue")



data |>
  group_by(position) |>
  count() |>
  ungroup() |>
  mutate(prop = n / sum(n))


|>
  summarize(ready = all(team %in% baseball$team))
    3 Kansas City      
    4 L.A. Angels      
    5 L.A. Dodgers     
    6 N.Y. Mets        
    7 N.Y. Yankees     
    8 San Diego        
    9 San Francisco    
    10 St. Louis        
    11 Tampa Bay
    
    grepl("L.A.|N.Y.",team) ~ team,
    grepl("St. Louis",team) ~ "St. Louis",
    T ~ team
  ))

head(record)

boston <- baseball |>
  filter(team == "Boston") |>
  filter(position == "RHP")
truth <- boston |>
  select(salary) |>
  summarize_all(.funs = mean) |>
  pull(salary)

boston |>
  ggplot(aes(x = 1, y = salary)) +
  geom_point()

performance <- read_csv("https://pmagunia.com/assets/data/csv/dataset-89446.csv")

performance |>
  filter(year == max(year))

