
library(tidyverse)

set.seed(14850)

myfun <- function(x) plogis(3*x)

sim_control <- tibble(x = runif(40)) |>
  mutate(
    pi = myfun(x), 
    y = rbinom(n(), 1, pi),
    a = F
  )

sim_treated <-  tibble(x = rbeta(10,3,1)) |>
  mutate(
    pi = myfun(x),
    y = pi,
    a = T
  )

tibble(group = c("No 3370","With 3370")) |>
  ggplot(aes(x = 1, y = 2:1, label = group, color = group)) +
  geom_point() +
  geom_text(hjust = 0, aes(x = 1.05), fontface = "bold") +
  scale_color_manual(values = c("darkgray","dodgerblue")) +
  theme_void() +
  theme(legend.position = "none") +
  ylim(c(0,3)) +
  xlim(c(.9,1.5))
ggsave("legend.pdf",
       height = 1, width = 2)

p <- sim_control |>
  ggplot(aes(x = x, y = y)) +
  ylab("Support for Taxation\nto Address Injustice") +
  xlab("Past Concern about Inequality") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme_bw()

p +
  geom_point(color = "darkgray")
ggsave("p1.pdf", height = 2, width = 3.5)

p +
  geom_point(color = "darkgray") +
  geom_line(aes(y = pi), color = "darkgray")
ggsave("p2.pdf", height = 2, width = 3.5)

p +
  geom_point(data = sim_treated, aes(y = 1), color = "dodgerblue") +
  geom_line(data = rbind(sim_treated, sim_control), aes(y = pi), color = "darkgray")
ggsave("p3.pdf", height = 2, width = 3.5)

p +
  geom_point(data = sim_treated, aes(y = 1), color = "dodgerblue") +
  geom_line(data = rbind(sim_treated, sim_control), aes(y = pi), color = "darkgray") +
  geom_point(data = sim_treated, aes(y = pi), color = "darkgray")
ggsave("p4.pdf", height = 2, width = 3.5)

p +
  geom_segment(data = sim_treated, aes(xend = x, y = pi, yend = .99),
               arrow = arrow(length = unit(.05,"in")),
               color = "dodgerblue") +
  geom_point(data = sim_treated, aes(y = 1), color = "dodgerblue") +
  geom_line(data = rbind(sim_treated, sim_control), aes(y = pi), color = "darkgray") +
  geom_point(data = sim_treated, aes(y = pi), color = "darkgray")
ggsave("p5.pdf", height = 2, width = 3.5)

