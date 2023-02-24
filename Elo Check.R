library(tidyverse)
library(elo)
library(zoo)
library(broom)
source("helpers.R")

bbb_data <- read.csv(here::here("IPL Ball-by-Ball 2008-2020.csv"))
match_data <- read.csv(here::here("IPL Matches 2008-2020.csv"))

match_data <- match_data |>
  mutate(
    winner = str_replace(winner, "Pune Warriors", "Pune warriors/ SuperGiants"),
    winner = str_replace(winner, "Rising Pune Supergiants", "Pune warriors/ SuperGiants"),
    winner = str_replace(winner, "Rising Pune Supergiant", "Pune warriors/ SuperGiants")
  ) |>
  mutate(
    toss_winner = str_replace(toss_winner, "Pune Warriors", "Pune warriors/ SuperGiants"),
    toss_winner = str_replace(
      toss_winner,
      "Rising Pune Supergiants",
      "Pune warriors/ SuperGiants"
    ),
    toss_winner = str_replace(
      toss_winner,
      "Rising Pune Supergiant",
      "Pune warriors/ SuperGiants"
    )
  ) |>
  mutate(
    team1 = str_replace(team1, "Pune Warriors", "Pune warriors/ SuperGiants"),
    team1 = str_replace(team1, "Rising Pune Supergiants", "Pune warriors/ SuperGiants"),
    team1 = str_replace(team1, "Rising Pune Supergiant", "Pune warriors/ SuperGiants")
  ) |>
  mutate(
    team2 = str_replace(team2, "Pune Warriors", "Pune warriors/ SuperGiants"),
    team2 = str_replace(team2, "Rising Pune Supergiants", "Pune warriors/ SuperGiants"),
    team2 = str_replace(team2, "Rising Pune Supergiant", "Pune warriors/ SuperGiants")
  ) |>
  mutate(
    winner = str_replace(winner, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    winner = str_replace(winner, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    toss_winner = str_replace(toss_winner, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    toss_winner = str_replace(toss_winner, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    team1 = str_replace(team1, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    team1 = str_replace(team1, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    team2 = str_replace(team2, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    team2 = str_replace(team2, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    winner = str_replace(winner, "Deccan Chargers", "Hyd - DeccanChargers/ Sunrisers"),
    winner = str_replace(winner, "Sunrisers Hyderabad", "Hyd - DeccanChargers/ Sunrisers")
  ) |>
  mutate(
    toss_winner = str_replace(
      toss_winner,
      "Deccan Chargers",
      "Hyd - DeccanChargers/ Sunrisers"
    ),
    toss_winner = str_replace(
      toss_winner,
      "Sunrisers Hyderabad",
      "Hyd - DeccanChargers/ Sunrisers"
    )
  ) |>
  mutate(
    team1 = str_replace(team1, "Deccan Chargers", "Hyd - DeccanChargers/ Sunrisers"),
    team1 = str_replace(team1, "Sunrisers Hyderabad", "Hyd - DeccanChargers/ Sunrisers")
  ) |>
  mutate(
    team2 = str_replace(team2, "Deccan Chargers", "Hyd - DeccanChargers/ Sunrisers"),
    team2 = str_replace(team2, "Sunrisers Hyderabad", "Hyd - DeccanChargers/ Sunrisers")
  )

bbb_data <- bbb_data |>
  mutate(
    batting_team = str_replace(batting_team, "Pune Warriors", "Pune warriors/ SuperGiants"),
    batting_team = str_replace(
      batting_team,
      "Rising Pune Supergiants",
      "Pune warriors/ SuperGiants"
    ),
    batting_team = str_replace(
      batting_team,
      "Rising Pune Supergiant",
      "Pune warriors/ SuperGiants"
    )
  ) |>
  mutate(
    bowling_team = str_replace(bowling_team, "Pune Warriors", "Pune warriors/ SuperGiants"),
    bowling_team = str_replace(
      bowling_team,
      "Rising Pune Supergiants",
      "Pune warriors/ SuperGiants"
    ),
    bowling_team = str_replace(
      bowling_team,
      "Rising Pune Supergiant",
      "Pune warriors/ SuperGiants"
    )
  ) |>
  mutate(
    batting_team = str_replace(batting_team, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    batting_team = str_replace(batting_team, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    bowling_team = str_replace(bowling_team, "Delhi Daredevils", "Delhi-Capitals/Daredevils"),
    bowling_team = str_replace(bowling_team, "Delhi Capitals", "Delhi-Capitals/Daredevils")
  ) |>
  mutate(
    batting_team = str_replace(
      batting_team,
      "Deccan Chargers",
      "Hyd - DeccanChargers/ Sunrisers"
    ),
    batting_team = str_replace(
      batting_team,
      "Sunrisers Hyderabad",
      "Hyd - DeccanChargers/ Sunrisers"
    )
  ) |>
  mutate(
    bowling_team = str_replace(
      bowling_team,
      "Deccan Chargers",
      "Hyd - DeccanChargers/ Sunrisers"
    ),
    bowling_team = str_replace(
      bowling_team,
      "Sunrisers Hyderabad",
      "Hyd - DeccanChargers/ Sunrisers"
    )
  )

# match_data$neutral_venue |> table()

df <- merge(bbb_data, match_data, by = "id")
df$date <- df$date |>  as.Date()
df <- df[with(df, order(df$id, df$inning, df$over, df$ball)), ]

df <- df |> drop_na(winner)

match_data <- match_data |>  drop_na(winner)

df_for_form <- match_data |>
  select(c(id, team1, team2, winner)) |> 
  pivot_longer(cols = c(team1, team2)) |> 
  mutate(won = ((winner == value) |> as.numeric()))  |> 
  group_by(value) |> 
  mutate(form = rollapplyr(
    data = won,
    width = 6,
    FUN = function(z) helper_for_fd(z),
    by.column = FALSE,
    partial =TRUE
  ))


df_for_form_team1 <- df_for_form |> subset(name == "team1")
df_for_form_team2 <- df_for_form |> subset(name == "team2")

match_data <- match_data |> 
  mutate("team1_form" = df_for_form_team1$form) |> 
  mutate("team2_form" = df_for_form_team2$form) |> 
  mutate("fd" = team1_form - team2_form)

form_final_df <- match_data |> 
  select(id, team1_form, team2_form)

elo_data_ipl <- match_data |>
  select(id, date, team1, team2, winner) |>
  arrange(id) |>
  mutate(result = if_else(
    as.character(winner) == as.character(team1),
    1,
    if_else(as.character(winner) == as.character(team2), 0, 0.5)
  ))

elo_check <- function(k, df_elo = elo_data_ipl) {
 df_elo  %>%
    elo.run(result ~ team1 + team2, k = k, data = .) |> 
    rank.teams() |> 
    generics::tidy() |> 
    rename(rank = x) |> 
    mutate("k" = k) |> 
    arrange(rank)
    
}

map_dfr(c(10,15,20,25,30,35,40), elo_check) |> pivot_wider(names_from = k, values_from = rank)






?elo.run
