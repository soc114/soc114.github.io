
# Absolute mobility
p0 <- tibble(
  i = 1:10,
  parent_index = 1:10,
  child_index = 2 * (1:10)
) |>
  ggplot() +
  geom_point(aes(x = 1, y = parent_index)) +
  geom_point(aes(x = 2, y = child_index)) +
  #geom_text(
  #  aes(x = .9, y = parent_index, label = parent_index, hjust = 1)
  #) +
  #geom_text(
  #  aes(x = 2.1, y = child_index, label = child_index, hjust = 0)
  #) +
  theme_void() +
  annotate(geom = "text", x = .7, y = 5.5, hjust = 1,
           label = "Parent\nIncome\n(Inflation-\nAdjusted\nDollars)") +
  annotate(geom = "text", x = 2.3, y = 11, hjust = 0,
           label = "Child\nIncome\n(Inflation-\nAdjusted\nDollars)") +
  xlim(c(0,3))
p0
ggsave("abs_mobility_1.pdf", height = 4, width = 3)
p0 +
  geom_segment(
    aes(
      x = 1, xend = 1.95, 
      y = parent_index, 
      yend = parent_index + .95 * (child_index - parent_index)
    ),
    arrow = arrow(length = unit(.07,"in"), type = "closed")
  )
ggsave("abs_mobility_2.pdf", height = 4, width = 3)
p1 <- p0 + 
  geom_text(aes(x = .9, y = parent_index, label = 1:10, hjust = 1))
p1
ggsave("rank_parents.pdf", height = 4, width = 3)
p1 +
  geom_text(aes(x = 2.1, y = child_index, label = 1:10, hjust = 0))
ggsave("rank_children.pdf", height = 4, width = 3)

# Visualize relative mobility
library(tidyverse)

base_plot <- tibble(
  i = 1:10,
  parent_index = 1:10,
  child_index = 1:10
) |>
  ggplot() +
  geom_point(aes(x = 1, y = parent_index)) +
  geom_point(aes(x = 2, y = child_index)) +
  geom_text(
    aes(x = .9, y = parent_index, label = parent_index, hjust = 1)
  ) +
  geom_text(
    aes(x = 2.1, y = child_index, label = child_index, hjust = 0)
  ) +
  theme_void() +
  annotate(geom = "text", x = .5, y = 5.5, hjust = 1,
           label = "Parent\nIncome\nRank") +
  annotate(geom = "text", x = 2.5, y = 5.5, hjust = 0,
           label = "Child\nIncome\nRank") +
  xlim(c(0,3))



base_plot
ggsave("ranks_base.pdf", height = 4, width = 3)

update_plot <- function(newdata) {
  base_plot + 
    geom_segment(
      data = newdata,
      aes(
        x = 1, xend = 1.95, 
        y = parent_index, 
        yend = parent_index + .95 * (child_index - parent_index)
      ),
      arrow = arrow(length = unit(.07,"in"), type = "closed")
    )
}

# 50 percent move up
update_plot(tibble(
  parent_index = 1, child_index = 2
))
ggsave("ranks_50pct_1.pdf", height = 4, width = 3)
update_plot(tibble(
  parent_index = 1:2, child_index = 2:1
))
ggsave("ranks_50pct_2.pdf", height = 4, width = 3)
update_plot(tibble(
  parent_index = 1:10, child_index = c(2,1,4,3,6,5,8,7,10,9)
))
ggsave("ranks_50pct.pdf", height = 4, width = 3)

# 90 percent move up
update_plot(tibble(
  parent_index = 1:9, child_index = 2:10
))
ggsave("ranks_90pct_1.pdf", height = 4, width = 3)
update_plot(tibble(
  parent_index = 1:10, child_index = c(2:10,1)
))
ggsave("ranks_90pct_2.pdf", height = 4, width = 3)

# Introduce Chetty summary
p <- tibble(
  i = 1:10,
  parent_index = 1:10,
  child_index = 1:10
) |>
  ggplot() +
  geom_rect(
    data = data.frame(xmin = .8, xmax = 1.2, ymin = seq(.5, 8.5, 2),
                      ymax = seq(2.5,10.5,2), group = 1:5),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
    alpha = .4, fill = NA, color = "black"
  ) +
  geom_point(aes(x = 1, y = parent_index)) +
  geom_point(aes(x = 2, y = child_index)) +
  #geom_text(
  #  aes(x = .9, y = parent_index, label = parent_index, hjust = 1)
  #) +
  #geom_text(
  #  aes(x = 2.1, y = child_index, label = child_index, hjust = 0)
  #) +
  annotate(geom = "text", x = .7, y = 1.5, hjust = 1, label = "Q1: Bottom\nQuintile") +
  annotate(geom = "text", x = .7, y = 9.5, hjust = 1, label = "Q5: Top\nQuintile") + 
  theme_void() +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .5, y = 5.5, hjust = 1,
           label = "Parent\nIncome\nRank") +
  annotate(geom = "text", x = 2.5, y = 5.5, hjust = 0,
           label = "Child\nIncome\nRank") +
  xlim(c(0,3))
p
ggsave("ranks_quintiles_1.pdf", height = 4, width = 3)
p + 
  annotate(geom = "rect", fill = "gray", alpha = .2,
           xmin = 1.8, xmax = 2.2, ymin = 8.5, ymax = 10.5,
           color = "black") +
  geom_segment(
    data = tibble(parent_index = seq(1.5,9.5,2), child_index = seq(8.9,9.5,length.out = 5)),
    aes(
      x = 1.3, 
      xend = 1.7, 
      y = parent_index + .05 * (child_index - parent_index),
      yend = parent_index + .9 * (child_index - parent_index)
    ),
    arrow = arrow(length = unit(.07,"in"), type = "closed")
  )
ggsave("ranks_quintiles_2.pdf", height = 4, width = 3)
#










base_plot + 
  geom_segment(
    data = data.frame()
    aes(
      x = 1, xend = 1.95, 
      y = parent_index, 
      yend = parent_index + .95 * (child_index - parent_index)
    ),
    arrow = arrow(length = unit(.07,"in"), type = "closed")
  )



  pivot_longer(cols = -i) |>
  mutate(x = name == "child_index") |>
  ggplot(aes(x = x, y = value)) +
  geom_point()


make_ladder <- function(y_vals) {
  tibble(y = y_vals) |>
    ggplot() +
    geom_segment(aes(x = 0, xend = 1, y = y, yend = y)) +
    geom_segment(aes(
      x = 0, xend = 0, 
      y = min(y) - .5 * diff(range(y)) / 10, 
      yend = max(y) + .5 * diff(range(y)) / 10
    )) +
    geom_segment(aes(
      x = 1, xend = 1, 
      y = min(y) - .5 * diff(range(y)) / 10, 
      yend = max(y) + .5 * diff(range(y)) / 10
    )) +
    geom_text(aes(
      label = paste0(seq(10,90,10),"th percentile"),
      x = 1.1, y = y
    ), hjust = 0, size = 3) +
    geom_segment(aes(x = 0, xend = 0, y = min(y) - .5, yend = max(y) + .5)) +
    theme_void() +
    xlim(c(0,2)) +
    ylim(c(0,20))
}

# Growth
make_ladder(y_vals = 1:9)
make_ladder(y_vals = 3:11)

# Gains concetrated at the top
make_ladder(y_vals = 1:9)
make_ladder(y_vals = c(1:8,19))

# Rising inequality
make_ladder(y_vals = seq(7,13,length.out = 9))
make_ladder(y_vals = seq(1,19, length.out = 9))

# Most people move down in absolute terms
make_ladder(y_vals = seq(1,19, length.out = 9))
make_ladder(y_vals = exp(seq(log(1),log(19), length.out = 9)))

cbind(
  seq(1,19, length.out = 9),
  exp(seq(log(1),log(19), length.out = 9))
)
