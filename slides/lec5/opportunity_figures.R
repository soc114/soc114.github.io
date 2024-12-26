

library(tidyverse)
library(foreach)

xvals <- seq(0,1,.2)
dens <- foreach(xval = xvals, .combine = "rbind") %do% {
  tibble(
    group = xval,
    xdens = .2*dnorm(seq(-3,3, length.out = 100)),
    ydens = seq(-.1,.1, length.out = 100)
  )
}

dens |>
  ggplot() +
  geom_line(aes(x = group, y = group)) +
  #geom_segment(aes(x = group, xend = group + .1*dnorm(0), y = group, xend = , yend = group), color = "gray") +
  geom_polygon(aes(x = group + xdens, y = group + 3*ydens, group = group), color = "gray", fill = "gray", alpha = .8) +
  theme_classic() +
  scale_x_continuous(name = "Parent Income", breaks = NULL) +
  scale_y_continuous(name = "Child Income", breaks = NULL)
ggsave("unequal.pdf",
       height = 2, width = 3)

dens |>
  ggplot() +
  geom_line(aes(x = group, y = .5)) +
  geom_polygon(aes(x = group + xdens, y = .5 + 5*ydens, group = group), color = "gray", fill = "gray", alpha = .8) +
  theme_classic() +
  scale_x_continuous(name = "Parent Income", breaks = NULL) +
  scale_y_continuous(name = "Child Income", breaks = NULL)
ggsave("equal.pdf",
       height = 2, width = 3)
