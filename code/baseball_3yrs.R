
library(tidyverse)
library(doParallel)

# Goal of this script: scrape MLB salaries for 2021, 2022, and 2023
# Return a long data frame with one row for each player-year

source_info <- tibble(
  year = 2021:2023,
  link_string = c(
    "mlb-salaries",
    "mlb-salaries-2022",
    "major-league-baseball-salaries-2023"
  ),
  page_indices = list(
    indices_21 = 1:(20*45 - 2),
    indices_22 = c(1:960,962:972),
    indices_23 = 1:(20*47 + 4)
  )
)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
t0 <- Sys.time()
scraped <- foreach(
  this_year = 2021:2023,
  .combine = "rbind"
) %do% {
  this_source_info <- source_info |>
    filter(year == this_year)
  foreach(
    i = this_source_info$page_indices[[1]], 
    .packages = "tidyverse",
    .combine = "rbind"
  ) %dopar% {
    tibble(
      line = read_lines(
        paste0(
          "https://databases.usatoday.com/",
          this_source_info$link_string,
          "/",
          i
        )
      )
    ) |>
      filter(grepl("h4",line) | grepl("h4",lag(line,1))) |>
      mutate(line = str_replace_all(line,"\t|<h4>|<p>|</h4>|</p>","")) |>
      filter(1:n() %in% 3:16) |>
      mutate(name = rep(c("variable","value"),7),
             item = rep(1:7, each = 2)) |>
      pivot_wider(names_from = "name", values_from = "line") |>
      select(-item) |>
      pivot_wider(names_from = "variable", values_from = "value")
  } |>
    select(Player, Team, Position, Salary) |>
    mutate(year = this_year)
}

baseball_3yrs <- scraped |>
  # Make column names lower case
  rename_with(tolower) |>
  # Remove asterisk for injured list
  mutate(player = str_remove_all(player,"[*]")) |>
  # Remove any double spaces
  mutate(player = str_replace_all(player,"  "," ")) |>
  # Ensure each player has one space after comma
  # Do this by first removing spaces, then adding back for everyone
  mutate(
    player = str_replace(player,", ",","),
    player = str_replace(player,",",", ")
  ) |>
  # Make salary dollar value
  mutate(
    salary = str_replace_all(salary,"[$]|,",""),
    salary = as.numeric(salary)
  ) |>
  select(player, year, team, position, salary) |>
  arrange(player, year) |>
  # Clean up position coding differences
  mutate(position = case_when(
    position %in% c("1B","2B","3B","SS","INF") ~ "infielder",
    position %in% c("OF","0F") ~ "outfielder",
    position %in% c("C") ~ "catcher",
    position %in% c("RHP","LHP","RP","SP") ~ "pitcher",
    position %in% c("DH") ~ "designated_hitter",
    # Ohtani, code as pitcher
    position %in% c("DH/SP","RHP/OF") ~ "pitcher",
    # one player lacks a code
    player == "Maples, Dillon" ~ "pitcher"
  )) |>
  # Clarify players with same names
  mutate(
    player = case_when(
      player == "Smith, Will" ~ paste0(player," (",position,")"),
      player == "Castillo, Diego" ~ paste0(player," (",position,")"),
      player %in% c("Garcia, Luis","Garcia, Luis (Amado)") & team == "San Diego" & position == "pitcher" ~ "Garcia, Luis (pitcher 1)",
      player == "Garcia, Luis" & team == "Houston" & position == "pitcher" ~ "Garcia, Luis (pitcher 2)",
      player == "Garcia, Luis" & position == "infielder" ~ "Garcia, Luis (infielder)",
      player == "Hernandez, Jose" & team != "Pittsburgh" ~ "Hernandez, Jose (pitcher 1)",
      player == "Hernandez, Jose" & team == "Pittsburgh" ~ "Hernandez, Jose (pitcher 2)",
      T ~ player
    )
  ) |>
  # Clarify players with names spelled multiple ways
  mutate(
    player = str_remove_all(player,"[.]"),
    player = str_remove_all(player," Jr| IV| II"),
    player = case_when(
      player == "Baez, Michael" ~ "Baez, Michel",
      player == "Berti, Jon" ~ "Berti, Jonathan",
      player == "Boxberger, Brad" ~ "Boxberger, Bradley",
      player == "Brentz, Jake" ~ "Brentz, Jacob",
      player == "Brubaker, JT" ~ "Brubaker, Jonathan",
      player == "Casali, Curt" ~ "Casali, Curtis",
      player == "Chang, Yu" ~ "Chang, Yu-Cheng",
      player == "Cishek, Steve" ~ "Cishek, Steven",
      player == "Colome, Alex" ~ "Colome, Alexander",
      player == "Coonrod, Sam" ~ "Coonrod, Samuel",
      player %in% c("Davies, Zac","Davies, Zach") ~ "Davies, Zachary",
      player == "Flexen, Chris" ~ "Flexen, Christopher",
      player %in% c("Gurriel, Yuli","Gurriel, Yulieski","Gurriel, Yuri") ~ "Gurriel, Yuli",
      player == "Hill, Rich" ~ "Hill, Richard",
      player == "Martin, Chris" ~ "Martin, Christopher",
      player == "Montas, Franky" ~ "Montas, Frankie",
      player == "Moore, Matt" ~ "Moore, Matthew",
      player == "Pop, Zach" ~ "Pop, Zachary",
      player == "Rodriguez, Chris" ~ "Rodriguez, Christopher",
      player == "Rojas, Josh" ~ "Rojas, Joshua",
      player == "Sims, Luke" ~ "Sims, Lucas",
      player == "Soroka, Mike" ~ "Soroka, Michael",
      player == "Taylor, Michael A." ~ "Taylor, Michael",
      player == "Torkelson, pencer" ~ "Torkelson, Spencer",
      player == "Urshela, Giovanny" ~ "Urshela, Gio",
      player == "Velasquez, Vince" ~ "Velasquez, Vincent",
      player == "Vogelbach, Dan" ~ "Vogelbach, Daniel",
      player == "Wendelken, Jeffrey" ~ "Wendelken, JB",
      T ~ player
    )
  ) |>
  # Clarify teams with names spelled two ways
  mutate(
    team = case_when(
      team == "Chi. Cubs" ~ "Chicago Cubs",
      team == "Chic. Cubs" ~ "Chicago Cubs",
      team == "Chic. White Sox" ~ "Chicago White Sox",
      team == "San Diego " ~ "San Diego",
      T ~ team
    )
  )
spent <- difftime(t0,Sys.time())

baseball_3yrs |>
  write_csv("../data/baseball_3yrs.csv")

