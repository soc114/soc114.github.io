

page <- read_lines("https://www.ibsf.org/en/result/504954/?cHash=fee0d2f086d9199ad2f5be907dde6ce0", destfile = "result.pdf")



library(pdftools)
pdf_text("https://www.ibsf.org/en/result?document=8a026e45-d865-4721-844d-7343d382c2cb&eID=GetDocument&fileName=Result_505159.pdf&cHash=20b80ee8bf834b6bfb5f62cdee61fcd1")
results <- download.file(
  "https://www.ibsf.org/en/result?document=8a026e45-d865-4721-844d-7343d382c2cb&eID=GetDocument&fileName=Result_505159.pdf",
  destfile = "result.pdf"
)


to_scrape <- list.files("monobob_files")

monobob_data <- foreach(i = 1:length(to_scrape), .combine = "rbind") %do% {
  scraped <- pdf_text(pdf = paste0("monobob_files/",to_scrape[i]))
  
  lines <- read_lines(scraped[[1]])
  
  # Remove a manually caught case where did not start
  if (to_scrape[i] == "Result_505219.pdf") {
    lines <- lines[1:52]
  }
  
  event <- lines[1] |> str_extract(".*Bobsleigh|.*IBSF World Cup Bob & Skeleton|.*North American Cup") |> str_remove("\\s\\s+")
  place <- lines[1] |> str_remove(paste0(".*",event," ")) |> str_extract(".*[)]") |>
    str_replace("St.Moritz","St. Moritz")
  
  results_header_row <- min(which(grepl("Rk", lines)))
  last_total_row <- max(which(grepl("Total",lines))) - results_header_row
  
  result <- lines[-(1:results_header_row)][1:last_total_row] |>
    # Fix one error caught manually where one space instead of 2 in PDF
    str_replace("2 MON BOTTIN","2  MON  BOTTIN") |>
    strsplit(split = "\\s\\s+") |>
    lapply(FUN = \(x) x[which(grepl("Total|[A-Z][A-Z]",x))]) |>
    unlist() |>
    as_tibble() |>
    mutate(
      name = rep(c("country","name","time"),n() / 3),
      rank = rep(1:(n() / 3), each = 3)
    ) |>
    pivot_wider(names_from = "name", values_from = "value")
  
  # Create time for the case with only 1 run
  if (to_scrape[i] == "Result_505251.pdf") {
    result <- result |>
      mutate(
        time = time |> str_remove("Total: "),
        time_seconds = time |> as.numeric()
      ) |>
      mutate(event = event, place = place) |>
      select(event, place, rank, country, name, time_seconds)
      
  } else {
    # Create time per run for competitions that sum 2 runs
    result <- result |>
      mutate(
        time = time |> str_remove("Total: "),
        minutes = time |> str_remove(":.*") |> as.numeric(),
        seconds = time |> str_remove(".*:") |> as.numeric(),
        time_seconds = (60 * minutes + seconds) / 2
      ) |>
      mutate(event = event, place = place) |>
      select(event, place, rank, country, name, time_seconds)
  }

  return(result)
}


monobob_data |>
  filter(rank == 1) |>
  ggplot(aes(x = place, y = time_seconds)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

monobob_data |>
  #filter(place == "Cortina (ITA)") |>
  ggplot(aes(x = time_seconds)) +
  geom_histogram() +
  facet_wrap(~place, ncol = 1)


monobob_data |>
  left_join(bobsleigh_center_data, by = join_by(place)) |>
  mutate(speed = competition_length / time_seconds) |>
  filter(rank == 1) |>
  ggplot(aes(x = speed)) +
  geom_histogram() +
  facet_wrap(~place, ncol = 1)

merged <- monobob_data |>
  left_join(bobsleigh_center_data, by = join_by(place)) |>
  mutate(speed = competition_length / time_seconds)

# Maybe try speed?

loo <- foreach(place_value = unique(monobob_data$place), .combine = "rbind") %do% {
  fit <- lm(
    speed ~ start_altitude + gradient + vertical_drop + curves,
    data = merged |> filter(place != place_value)
  )
  merged |>
    mutate(yhat = predict(fit, newdata = merged)) |>
    filter(place == place_value) |>
    select(place, y = speed, yhat)
}
within_comparison <- merged |>
  group_by(place) |>
  filter(n() > 1) |>
  rename(y = speed) |>
  mutate(yhat = (sum(y) - y) / (n() - 1)) |>
  select(place, y, yhat)

within_comparison |>
  ungroup() |>
  summarize(mse = mean((y - yhat) ^ 2))

loo |>
  filter(place %in% within_comparison$place) |>
  ungroup() |>
  summarize(mse = mean((y - yhat) ^ 2))


# Try to predict particular athlete place?

merged |>
  filter(country == "USA") |>
  distinct(name)

merged |>
  group_by(event, place) |>
  filter(rank <= 2) |>
  mutate(index = rep(1:(n() / 2), each = 2)) |>
  group_by(event, place, index) |>
  summarize(difference = diff(time_seconds)) |>
  left_join(bobsleigh_center_data, by = join_by(place)) |>
  select(difference, all_of(colnames(bobsleigh_center_data))) |>
  pivot_longer(cols = -c("event","place","difference")) |>
  ggplot(aes(x = value, y = difference)) +
  geom_point() +
  facet_wrap(~name, scales = "free_x")

merged |>
  filter(place == "Cortina (ITA)") |>
  ggplot(aes(x = time_seconds, y = 1, color = country == 'USA')) +
  geom_point()

## Predict time of particular athlete

fit <- lm(time_seconds ~ name + place, data = merged)
predict(fit)

merged |>
  ggplot(aes(x = place, y = competition_length / time_seconds)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

monobob_data |>
  filter(time_seconds > 70)

bobsleigh_center_data <- read_csv("bobsleigh_center_data.csv")
cortina <- bobsleigh_center_data |> filter(place == "Cortina (ITA)")

## Predict who will win


monobob_data |>
  filter(country == "USA") |>
  group_by(name) |>
  summarize(average_place = mean(rank, na.rm = T)) |>
  print(n = Inf)

winners <- monobob_data |>
  left_join(bobsleigh_center_data, by = join_by(place)) |>
  filter(place != "Cortina (ITA)") |>
  filter(rank == 1)
winners |>
  pivot_longer(
    cols = c("competition_length","start_altitude","gradient","vertical_drop","curves"),
    names_to = "predictor"
  ) |>
  ggplot(
    aes(x = value, y = time_seconds)
  ) +
  geom_point() +
  facet_wrap(~predictor, scales = "free_x") +
  geom_vline(
    data = cortina |> pivot_longer(cols = -place, names_to = "predictor"),
    aes(xintercept = value),
    linetype = "dashed"
  )

lm(time_seconds ~ competition_length + curves + vertical_drop + gradient + start_altitude, data = winners) |>
  summary()

bobsleigh_center_data |>
  ggplot(aes(x = competition_length, y = curves)) +
  geom_point()



|>
  ggplot(aes(x = vertical_drop, y = time_seconds)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(
    xintercept = cortina |> pull(vertical_drop),
    linetype = "dashed"
  )

mon

monobob_data |>
  filter(time_seconds > 3000)

monobob_data |>
  distinct(place) |>
  print(n = Inf)

data <- read_lines(url("https://www.ibsf.org/en/tracks#cortinadampezzo"))



  pull(place)

lines[7] |>
  strsplit(split = "\\s\\s+") |>
  unlist()

tibble(
  result = read_lines(scraped[[1]])[c(7,10)]
) |>
  separate_wider_delim(cols = result, delim = "  .*", names)
