

library(tidyverse)
library(MatchIt)

data("lalonde", package = "MatchIt")

# Example for illustration

m.out <- matchit(
  treat ~ age + re75, 
  data = lalonde |> select(treat, age, re75),
  method = "nearest",
  replace = T
)

# Code just for me: Find worst match
get_matches(m.out) |>
  tibble() |>
  group_by(subclass) |>
  mutate(total_distance = abs(diff(distance))) |>
  ungroup() |>
  arrange(-total_distance) |>
  slice_head(n = 1) |>
  select(subclass) |>
  left_join(get_matches(m.out), by = join_by(subclass)) |>
  ggplot(aes(x = age, y = re75)) +
  geom_point()

lalonde |>
  #filter(age >= 25 & age <= 40) |>
  #filter(re75 >= 10e3 & re75 <= 40e3) |>
  arrange(treat) |>
  ggplot(aes(x = age, y = re75, color = factor(treat))) +
  geom_point() +
  scale_y_continuous(
    name = "Earnings in 1975",
    labels = scales::label_currency()
  ) +
  scale_color_manual(
    values = c('gray','blue'),
    labels = c("Untreated:\nNo job training","Treated:\nJob training"),
    name = "Treatment"
  ) +
  scale_x_continuous(
    name = "Age"
  ) +
  theme_minimal() +
  theme(legend.key.height = unit(.5,"in"))
ggsave("figures/lalonde_data.pdf", height = 3, width = 5)

# Function to make plot
make_plot <- function(xrange, yrange, hjust = .5, vjust = -.5) {
  lalonde |>
    filter(age >= xrange[1] & age <= xrange[2]) |>
    filter(re75 >= yrange[1] & re75 <= yrange[2]) |>    
    arrange(treat, -re75) |>
    group_by(treat) |>
    mutate(label = paste(ifelse(treat,"Treated\nUnit","Untreated\nUnit"),1:n())) |>
    ggplot(aes(x = age, y = re75, color = factor(treat))) +
    geom_point(size = 4) +
    #geom_text(aes(label = label),
    #          show.legend = F, vjust = vjust, hjust = hjust) +
    ggrepel::geom_text_repel(aes(label = label),
                             show.legend = F) +
    scale_color_manual(
      values = c('black','blue'),
      labels = c("Untreated:\nNo job training","Treated:\nJob training"),
      name = "Treatment"
    ) +
    scale_y_continuous(
      limits = yrange,
      name = "Earnings in 1975",
      labels = scales::label_currency()
    ) +
    scale_x_continuous(
      limits = xrange,
      name = "Age"
    ) +
    theme_minimal() +
    theme(legend.key.height = unit(.5,"in"))
}

# Basic idea
make_plot(
  yrange = c(4.8e3,6.3e3),
  xrange = c(21,23),
)
ggsave("figures/lalonde_basic.pdf", height = 3, width = 5)
make_plot(
  yrange = c(4.8e3,6.3e3),
  xrange = c(21,23),
) +
  annotate(geom = "segment", x = 21, y = 5.8e3, yend = 6.2e3,
           color = "seagreen", linetype = "dashed") +
  annotate(geom = "segment", x = 22, y = 5.7e3, yend = 6.05e3,
           color = "seagreen", linetype = "dashed")
ggsave("figures/lalonde_basic_2.pdf", height = 3, width = 5)

# Caliper
make_plot(
  yrange = c(6e3,26e3),
  xrange = c(33,33)
)
ggsave("figures/lalonde_caliper.pdf", height = 3, width = 5)

# make_plot(
#   yrange = c(17e3,26e3),
#   xrange = c(24,36)
# )

# k:1
make_plot(
  yrange = c(1.1e3,1.6e3),
  xrange = c(17,17)
)
ggsave("figures/lalonde_k1.pdf", height = 3, width = 5)

# With replacement, and greedy vs optimal
make_plot(
  yrange = c(2.5e3,2.9e3),
  xrange = c(19,19)
)
ggsave("figures/lalonde_replacement.pdf", height = 3, width = 5)

# Multivariate matching
make_plot(
  yrange = c(7.5e3,9e3),
  xrange = c(20,24)
)
ggsave("figures/lalonde_multivariate.pdf", height = 3, width = 5)



