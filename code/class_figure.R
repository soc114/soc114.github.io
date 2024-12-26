
library(tidyverse)
gss <- read_csv("https://info3370.github.io/data/gss.csv")

gss |>
  filter(class > 0 & class < 5 & wtssall > 0) |>
  group_by(class) |>
  summarize(prop = sum(wtssall), .groups = "drop") |>
  mutate(prop = prop / sum(prop)) |>
  ggplot(aes(x = class, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = .005, label = scales::label_percent(accuracy = 1)(prop)), color = "white",
            vjust = 0) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Lower\nClass","Working\nClass","Middle\nClass","Upper\nClass")
  ) +
  ylab("Proportion Identifying\nwith Each Class") +
  xlab("Class")

ggsave("../slides/lec8/class.png", height = 3, width = 5.5)
