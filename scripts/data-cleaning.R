library(tidyverse)
library(elo)
library(zoo)
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
elo_run_ipl <- elo_data_ipl  %>%
  elo.run(result ~ team1 + team2, k = 10, data = .)
elo <- elo_run_ipl |>  as.data.frame()

match_data <-
  match_data |> mutate(
    "team1_elo" =  elo$elo.A - elo$update.A,
    "team2_elo" = elo$elo.B - elo$update.B
  )

elo_final <- match_data |>
  select(id, team1_elo, team2_elo)


df <- merge(df, elo_final) |> merge(form_final_df)

df <- df |>
  rowwise() |>
  mutate(rd = team1_elo - team2_elo)

First_inning <- df |>  filter(inning == 1)
Second_inning <- df |>  filter(inning == 2)

First_inning <- First_inning |>
  group_by(id) |>
  group_modify( ~ helper_for_grouping(.x))
Second_inning <- Second_inning |>
  group_by(id) |>
  group_modify( ~ helper_for_grouping(.x))

First_inning <- First_inning |>
  rowwise() |>
  mutate(home = helper_for_home(neutral_venue,
                                batting_team, team1)) |>
  rowwise() |> 
  mutate(away = helper_for_away(neutral_venue,
                                batting_team, team1)) |> 
  group_by(id) |>
  mutate(fd = team1_form - team2_form) |> 
  mutate(toss = ((batting_team == toss_winner) |> as.numeric())) |> 
  mutate(wl = cumsum(is_wicket) |> as.numeric()) |>
  mutate(total = cumsum(total_runs)) |>
  mutate(rpo = total / (over + ball / 6)) |>
  mutate(ball_num = over * 6 + ball_clean) |>
  mutate(ball_rem = 120 -  ball_num) |>
  mutate(win_loss = (batting_team == winner) |>  as.numeric()) |> 
  group_modify(~ helper_for_start_first(.x)) |> 
  filter(ball_rem != 0 ) |> 
  group_by(id) |> 
  rowwise() |> 
  mutate(wrl = (100 - dl_values[ball_rem, wl + 3]))


Target <- First_inning |>
  select(id, total) |>
  group_by(id) |> 
  slice_tail(n = 1)
colnames(Target)[2] <- "target"


Target$target <- Target$target + 1
Second_inning <- Second_inning |>
  merge(Target, by = "id") |>
  rowwise() |>
  mutate(home = helper_for_home(neutral_venue,
                                batting_team, team1)) |>
  rowwise() |> 
  mutate(away = helper_for_away(neutral_venue,
                                batting_team, team1)) |> 
  group_by(id) |>
  mutate(fd = team1_form - team2_form) |> 
  mutate(toss = ((batting_team == toss_winner) |> as.numeric())) |> 
  mutate(wl = cumsum(is_wicket)) |>
  mutate(total = cumsum(total_runs)) |>
  mutate(rpo = total / (over + ball / 6)) |>
  mutate(ball_num = over * 6 + ball_clean) |>
  mutate(ball_rem = 120 -  ball_num)  |>
  mutate(win_loss = (batting_team == winner) |>  as.numeric()) |>
  mutate(rrpo = (target - total) / ball_rem * 6) |> 
  group_modify(~ helper_for_start_second(.x)) |>
  filter(ball_rem != 0 ) |>
  group_by(id) |> 
  rowwise() |> 
  mutate(wrl = (100 - dl_values[ball_rem, wl + 3])) 

First_inning <- First_inning |> 
  select(id,  win_loss, home, away,toss,rd,fd,toss, wl, wrl, rpo, ball_rem)

Second_inning <- Second_inning |> 
  select(id,  win_loss, home, away,toss,rd,fd,toss, wl, wrl, rpo, rrpo, ball_rem)

write.csv(First_inning, file = "First.csv", row.names = FALSE)
write.csv(Second_inning, file = "Second.csv", row.names = FALSE)

