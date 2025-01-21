

population <- data.frame(row = rep(1:15,10),
                         seat = rep(1:10, each = 15))

vis_sample <- function(sample) {
  sample |>
    ggplot(aes(x = seat, y = row, fill = sampled)) +
    geom_tile(width = 1, height = 1,
              color = "black", alpha = .5) +
    theme_void() +
    annotate(geom = "text", x = 5.5, y = c(-1,17),
             label = c("Front of Room","Back of Room")) +
    scale_fill_manual(values = c("white","blue"),
                      drop = F) +
    theme(legend.position = "none")
}

# Full population
vis_sample(
  population |> 
    mutate(sampled = factor(T, levels = c(F,T)))
)
ggsave("fullcount.pdf",
       height = 3, width = 2)

# Simple random samples
for (i in 1:3) {
  vis_sample(
    population |> 
      mutate(sampled = as.logical(rbinom(n(),1,.1)))
  )
  ggsave(paste0("srs",i,".pdf"),
         height = 3, width = 2)
}

# Unequal probability samples
population |>
  ggplot(aes(x = seat, y = row)) +
  geom_tile(width = 1, height = 1, fill = "white",
            color = "black", alpha = .5) +
  annotate(geom = "rect", color = "gray", size = 2, alpha = 0,
           xmin = .5, xmax = 10.5, ymin = .5, ymax = 3.5) +
  annotate(geom = "rect", color = "black", size = 2, alpha = 0,
           xmin = .5, xmax = 10.5, ymin = 3.5, ymax = 15.5) +
  annotate(geom = "segment", color = "gray", size = 2,
           x = .5, xend = 10.5, y = 3.5, yend = 3.5, linetype = "dashed") +
  theme_void() +
  annotate(geom = "text", x = 5.5, y = c(-1,17),
           label = c("Front of Room","Back of Room")) +
  annotate(geom = "label", 
           x = 5.5, y = c(9,2),
           label = c("P(Sampled) = 0.1",
                     "P(Sampled) = 0.5"))
ggsave(paste0("unequal.pdf"),
       height = 3, width = 2)
for (i in 1:3) {
  vis_sample(
    population |> 
      mutate(p = ifelse(row <= 3, .5, .1),
             sampled = as.logical(rbinom(n(),1,p)))
  )
  ggsave(paste0("unequal",i,".pdf"),
         height = 3, width = 2)
}

population |>
  mutate(sampled = as.logical(rbinom(n(), 1, .1)))
