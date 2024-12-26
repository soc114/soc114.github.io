
library(tidyverse)

baseball_3yrs_wide <- read_csv("../../data/baseball_3yrs_wide.csv")

set.seed(14850)

baseball_3yrs_wide |>
  ggplot(aes(x = salary_2021, y = salary_2023)) +
  geom_point() +
  geom_smooth()

filter(position != "designated_hitter") |>
  filter(grepl("L.A.",team)) |>
  ggplot(aes(x = year, y = salary)) +
  facet_grid(position ~ team) +
  geom_point() +
  geom_smooth(method = "lm")

# Check for names that changed
wide |>
  mutate(lastname = str_remove(player, " .*|,.*")) |>
  group_by(lastname) |>
  filter(n() > 1) |>
  ungroup() |>
  select(player) |>
  print(n = Inf)

# Need to resolve some people who have the same name

prepared |>
  group_by(player, year) |>
  filter(n() > 1) |>
  arrange(position, player)

pivoted <- prepared |> 
  group_by(player) |> 
  #filter(n() == 3) |> 
  #filter(n_distinct(year) == 3) |>
  select(player, year, salary) |> 
  mutate(year = paste0("salary_",year)) |> 
  pivot_wider(names_from = "year", values_from = "salary")

# Question: What is the mean pay in 2023
# of those who were paid 5-10 million in 2021?
population <- pivoted |>
  ungroup() |>
  mutate(target_subgroup = salary_2021 >= 5e6 & salary_2021 <= 10e6) |>
  arrange(target_subgroup) |>
  select(player, salary_2021, salary_2023, target_subgroup) |>
  drop_na()

truth <- population |>
  filter(target_subgroup) |>
  summarize(truth = mean(salary_2023)) |>
  pull(truth)

population |>
  ggplot(aes(x = salary_2021, y = salary_2023)) +
  geom_point(aes(color = target_subgroup)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = c("lightgray","dodgerblue")) +
  theme(legend.position = "none") +
  scale_x_continuous(
    name = "Salary in 2021",
    labels = label_dollar(scale = 1 / 1e6, suffix = " million")
  ) +
  scale_y_continuous(
    name = "Salary in 2023",
    labels = label_dollar(scale = 1 / 1e6, suffix = " million")
  ) +
  coord_fixed() +
  annotate(
    geom = "text",
    x = 30e6, 
    y = 30e6,
    label = "Line of Equal Pay",
    angle = 45, vjust = 1.5,
    size = 3
  )


sample_mean_estimator <- function(sample) {
  estimate <- sample |>
    filter(target_subgroup) |>
    summarize(estimate = mean(salary_2023)) |>
    mutate(method = "Sample mean in subgroup")
  return(estimate)
}

ols_estimator <- function(sample) {
  fit <- lm(salary_2023 ~ salary_2021, data = sample)
  estimate <- population |>
    mutate(estimate = predict(fit, newdata = population)) |>
    filter(target_subgroup) |>
    summarize(estimate = mean(estimate)) |>
    mutate(method = "Ordinary Least Squares prediction")
  return(estimate)
}

gam_estimator <- function(sample) {
  fit <- gam(salary_2023 ~ s(salary_2021), data = sample)
  estimate <- population |>
    mutate(estimate = predict(fit, newdata = population)) |>
    filter(target_subgroup) |>
    summarize(estimate = mean(estimate)) |>
    mutate(method = "Generalized Additive Model prediction")
  return(estimate)
}

simulate <- function(n = 50) {
  sample <- population |>
    sample_n(size = n)
  
  estimates <- sample_mean_estimator(sample) |>
    bind_rows(ols_estimator(sample)) |>
    bind_rows(gam_estimator(sample))
  
  return(
    estimates
  )
}

simulations <- foreach(
  rep = 1:100,
  .combine = "rbind"
) %do% {
  simulate(n = 100)
}

rmse <- simulations |>
  mutate(
    error = estimate - truth,
    squared_error = error ^ 2
  ) |>
  group_by(method) |>
  summarize(rmse = sqrt(mean(squared_error)))

simulations |>
  left_join(rmse, by = join_by(method)) |>
  mutate(method = paste0(method,"\nRMSE = ",round(rmse))) |>
  mutate(method = fct_reorder(method, rmse)) |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~ method, ncol = 1) +
  geom_vline(xintercept = truth)

# Repeat that in a way visualizing for class
estimates_mean <- estimates_ols <- estimates_gam <- rep(NA,100)
jitter_val <- runif(100, -.1, .1)
for(i in 1:100) {
  sample <- population |>
    sample_n(size = 100)
  
  fit_ols <- lm(salary_2023 ~ salary_2021, data = sample)
  fit_gam <- gam(salary_2023 ~ s(salary_2021), data = sample)
  
  estimates_mean[i] <- sample_mean_estimator(sample)$estimate
  estimates_ols[i] <- ols_estimator(sample)$estimate
  estimates_gam[i] <- gam_estimator(sample)$estimate
  
  # Visualize estimates up to this point
  visualize <- function(estimates) {
    data.frame(estimate = estimates, jitter_val = jitter_val) |>
      ggplot(aes(x = 1 + jitter_val, y = estimate)) +
      geom_point(height = 0, width = .2) +
      geom_hline(yintercept = truth, linetype = "dashed") +
      annotate(geom = "text", x = 3, y = truth, label = "Target\nPopulation\nMean",
               size = 3, vjust = -.15, hjust = 1) +
      theme_void() +
      scale_y_continuous(
        name = "Sample Estimates",
        limits = c(7e6, 15e6),
        labels = label_dollar(scale = 1 / 1e6, suffix = "m")
      ) +
      scale_x_continuous(limits = c(.4,3)) +
      theme_classic() +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  
  # save graphs for certain indices
  if (i <= 5 | i %% 10 == 0) {
    
    # Plots of estimates
    visualize(estimates_mean)
    ggsave(paste0("../slides/lec4/sims/mean_estimate_",i,".pdf"),
           width = 2.5, height = 4)
    visualize(estimates_ols)
    ggsave(paste0("../slides/lec4/sims/ols_estimate_",i,".pdf"),
           width = 2.5, height = 4)
    visualize(estimates_gam)
    ggsave(paste0("../slides/lec4/sims/gam_estimate_",i,".pdf"),
           width = 2.5, height = 4)
    
    # Plots of estimation procedure
    forplot <- population |>
      mutate(
        ols = predict(fit_ols, newdata = population),
        gam = predict(fit_gam, newdata = population)
      ) |>
      mutate(sampled = player %in% sample$player) |>
      arrange(sampled)
    plot_base <- forplot |>
      ggplot(aes(x = salary_2021, y = salary_2023)) +
      theme_classic() +
      annotate(geom = "rect", xmin = -Inf, xmax = 5e6,
               ymin = -Inf, ymax = Inf,
               #ymin = -Inf, ymax = Inf,
               alpha = .2, fill = "lightgray") +
      annotate(geom = "rect", xmin = 10e6, xmax = Inf,
               ymin = -Inf, ymax = Inf,
               #ymin = -Inf, ymax = Inf,
               alpha = .2, fill = "lightgray") +
      scale_color_manual(values = c("lightgray","dodgerblue")) +
      theme(legend.position = "none") +
      scale_x_continuous(
        name = "Salary in 2021",
        labels = label_dollar(scale = 1 / 1e6, suffix = " million")
      ) +
      scale_y_continuous(
        name = "Salary in 2023",
        labels = label_dollar(scale = 1 / 1e6, suffix = " million")
      ) +
      geom_point(alpha = 0)
    
    default_width <- 5
    default_height <- 4
    if (i == 1) {
      plot_base +
        geom_point(color = "lightgray")
      ggsave(paste0("../slides/lec4/sims/population.pdf"),
             width = default_width, 
             height = default_height)
    }
    
    # Visualize the sample
    plot_base +
      geom_point(aes(color = sampled))
    ggsave(paste0("../slides/lec4/sims/sample_",i,".pdf"),
           width = default_width, 
           height = default_height)
    
    # Sample mean estimator
    plot_base +
      geom_point(data = forplot |>
                   filter(sampled) |>
                   filter(target_subgroup),
                 color = "dodgerblue")
    ggsave(paste0("../slides/lec4/sims/mean_estimator_",i,".pdf"),
           width = default_width, 
           height = default_height)
    
    # Linear regression estimator: Fit in full population
    plot_base +
      geom_point(aes(color = sampled)) +
      geom_line(
        aes(y = ols),
        color = "dodgerblue"
      )
    ggsave(paste0("../slides/lec4/sims/ols_fit_",i,".pdf"),
           width = default_width, 
           height = default_height)
    # Linear regression estimator: Target subgroup
    plot_base +
      geom_point(
        data = forplot |> filter(target_subgroup),
        aes(y = salary_2023, color = sampled)
      ) +
      geom_line(
        data = forplot,
        aes(y = ols),
        color = "dodgerblue"
      )
    ggsave(paste0("../slides/lec4/sims/ols_target_",i,".pdf"),
           width = default_width, 
           height = default_height)
    # Linear regression estimator: Predict in target subgroup
    plot_base +
      geom_point(
        data = forplot |> filter(target_subgroup),
        aes(y = ols, color = sampled)
      ) +
      geom_line(
        data = forplot,
        aes(y = ols),
        color = "dodgerblue"
      )
    ggsave(paste0("../slides/lec4/sims/ols_predicted_",i,".pdf"),
           width = default_width, 
           height = default_height)
    
    # GAM estimator: Fit in full population
    plot_base +
      geom_point(aes(color = sampled)) +
      geom_line(
        aes(y = gam),
        color = "dodgerblue"
      )
    ggsave(paste0("../slides/lec4/sims/gam_fit_",i,".pdf"),
           width = default_width, 
           height = default_height)
    # GAM estimator: Target subgroup
    plot_base +
      geom_point(
        data = forplot |> filter(target_subgroup),
        aes(y = salary_2023, color = sampled)
      ) +
      geom_line(
        data = forplot,
        aes(y = gam),
        color = "dodgerblue"
      )
    ggsave(paste0("../slides/lec4/sims/gam_target_",i,".pdf"),
           width = default_width, 
           height = default_height)
    # GAM estimator: Predict in target subgroup
    plot_base +
      geom_line(
        data = forplot,
        aes(y = gam),
        color = "dodgerblue"
      ) +
      geom_point(
        data = forplot |> filter(target_subgroup),
        aes(y = gam, color = sampled)
      )
    ggsave(paste0("../slides/lec4/sims/gam_predicted_",i,".pdf"),
           width = default_width, 
           height = default_height)
  }
}

# Reality: One sample



# Estimate by the sample mean in the target subgroup
sample_mean <- 
  
  # Estimate by OLS:
  # 1. fit OLS regression in the sample
  # 2. predict in the population
  # 3. average within the target subgroup
  
  
  # Estimate by GAM:
  # 1. fit OLS regression in the sample
  # 2. predict in the population
  # 3. average within the target subgroup
fit_gam <- gam(salary_2023 ~ s(salary_2021), data = sample)
gam_prediction <- population |>
  mutate(estimate = predict(fit_ols, newdata = population)) |>
  filter(target_subgroup) |>
  summarize(estimate = mean(estimate)) |>
  mutate(method = "Generalized Additive Model prediction")

return(
  sample_mean |>
    bind_rows(ols_prediction) |>
    bind_rows(gam_prediction)
)
}



simulations <- foreach(
  i = 1:1000,
  .combine = "rbind"
) %do% {
  simulate()
}









prepared |>
  ggplot(aes(x = year, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
