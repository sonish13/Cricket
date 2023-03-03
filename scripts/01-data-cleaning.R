library("tidyverse"); library("patchwork"); library("scales")
library("extrafont"); library("glue");
#theme_set(theme_minimal(base_family = "Roboto Condensed"))
theme_update(panel.grid.minor = element_blank())

source(here::here("scripts", "00-helpers.R"))

library(elo)
library(zoo)



# read in ball and match data ---------------------------------------------

ball_data <- read_csv(
  here::here("00-raw-data", "IPL Ball-by-Ball 2008-2020.csv"),
  col_types = cols(
    id = col_character(),
    inning = col_integer(),
    over = col_integer(),
    ball = col_integer(),
    batsman = col_character(),
    non_striker = col_character(),
    bowler = col_character(),
    batsman_runs = col_integer(),
    extra_runs = col_integer(),
    total_runs = col_integer(),
    non_boundary = col_logical(),
    is_wicket = col_logical(),
    dismissal_kind = col_character(),
    player_dismissed = col_character(),
    fielder = col_character(),
    extras_type = col_character(),
    batting_team = col_character(),
    bowling_team = col_character()
  )
)

match_data <- read_csv(
  here::here("00-raw-data", "IPL Matches 2008-2020.csv"),
  col_types = cols(
    id = col_character(),
    city = col_character(),
    date = col_date(),
    player_of_match = col_character(),
    venue = col_character(),
    neutral_venue = col_logical(),
    team1 = col_character(),
    team2 = col_character(),
    toss_winner = col_character(),
    toss_decision = col_character(),
    winner = col_character(),
    result = col_character(),
    result_margin = col_integer(),
    eliminator = col_character(),
    method = col_character(),
    umpire1 = col_character(),
    umpire2 = col_character()
  )
)






# read in duckworth lewis data --------------------------------------------

duckworth_lewis_wide <- read_csv(here::here("00-raw-data", "duckworth-lewis.csv"))
duckworth_lewis_wide$w10 <- rep(0, 20)
(duckworth_lewis_wide <- duckworth_lewis_wide %>% 
  janitor::clean_names() %>% 
  mutate(overs = as.integer(overs)))

(duckworth_lewis_tidy <- duckworth_lewis_wide %>%
  pivot_longer(
    starts_with("w"), 
    names_to = "wickets_lost", 
    values_to = "resources_remaining"
  ) %>% 
  mutate(
    wickets_lost = wickets_lost %>% str_extract("\\d+") %>% as.integer()
  ))

# Helper function for wrl 
helper_for_wrl <- function(balls, wickets, dl_values = duckworth_lewis_tidy) {
  dl_values |> filter(overs == ceiling(balls/6), wickets_lost == wickets) |> 
    select(resources_remaining) |> c() |>  unlist() |> unname()
}

# ggplot(duckworth_lewis_tidy, aes(wickets_lost, resources_remaining)) +
#   geom_point(aes(color = overs, group = overs), size = .5) +
#   geom_line(aes(color = overs, group = overs)) +
#   geom_text(
#     aes(wickets_lost - .05, label = overs), 
#     data = duckworth_lewis_tidy %>% filter(wickets_lost == 0L),
#     hjust = "right", family = "Roboto Condensed", size = 3, color = "gray25"
#   ) +
#   scale_x_continuous(breaks = 0:10) +
#   scale_color_viridis_b(n.breaks = 20, guide = "none") +
#   labs(
#     title = "Resources remaining by wickets lost, by overs",
#     x = "Wickets lost",
#     y = "Resources remaining"
#   )

# compute wicket resources lost




# clean data --------------------------------------------------------------

# clean the teamnames for the columns winner, toss_winner, team1 and team2 colums
match_data <- match_data |>
  mutate(
    across(
      c(winner, toss_winner, team1, team2),
      clean_teamnames
    )
  )
# match_data %>% count(winner)  
# match_data %>% count(toss_winner)  
# match_data %>% count(team1)  
# match_data %>% count(team2)  
  
ball_data <- ball_data |>
  mutate(
    across(c(batting_team, bowling_team), clean_teamnames)
  )
# ball_data %>% count(batting_team)
# ball_data %>% count(bowling_team)



# Merge the two data -----------------------------------------------------
data <- ball_data %>% 
  tidylog::left_join(match_data, by = "id")

# Save data variable as date format
data$date <- data$date |>  as.Date()

# Reorder data in the order we want, first by id, the by innings and over 
# and ball.Over and ball is the important one.
data <- data[with(data, order(data$id, data$inning, data$over, data$ball)), ]
  

# Dropping data with na values. Only four match datas were removed.

data <- data |> drop_na(winner)
match_data <- match_data |>  drop_na(winner)

# Calculating form for both the teams ------------------------------------
data_for_form <- match_data |>
  select(c(id, team1, team2, winner)) |> 
  pivot_longer(cols = c(team1, team2)) |> 
  mutate(won = ((winner == value) |> as.numeric()))  |> 
  group_by(value) |> 
  mutate(form = rollapplyr(
    data = won,
    width = 6,
    FUN = function(z) helper_for_fd(z), # This his a helper function to
    # calculate rolling weighted mean which we have for form  
    by.column = FALSE,
    partial =TRUE
  ))

# Subsetting form data frame for first and second innings data------------

data_for_form_team1 <- data_for_form |> subset(name == "team1")
data_for_form_team2 <- data_for_form |> subset(name == "team2")

# Calculating form difference --------------------------------------------
match_data <- match_data |> 
  mutate("team1_form" = data_for_form_team1$form) |> 
  mutate("team2_form" = data_for_form_team2$form) |> 
  mutate("fd" = team1_form - team2_form)


form_final_data <- match_data |> 
  select(id, team1_form, team2_form)
  
# Calculating elo -------------------------------------------------------
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

# Merging elo scores and form into data ----------------------------------

data <- merge(data, elo_final) |> merge(form_final_data)

# Calculating rd(rating difference)
data <- data |>
  rowwise() |>
  mutate(rd = team1_elo - team2_elo)

# Separate data into first and second innings.
First_inning <- data |>  filter(inning == 1)
Second_inning <- data |>  filter(inning == 2)

# Calculating variables for first innings ---------------------------------

# This part had to be done because the data would count extras(no-balls, wides,
# (etc)  as a ball and result as a single over as having more than 6 balls  So 
# we had to account for than and not count them as legal balls.

First_inning <- First_inning |>
  group_by(id) |>
  group_modify( ~ helper_for_grouping(.x))

Second_inning <- Second_inning |>
  group_by(id) |>
  group_modify( ~ helper_for_grouping(.x))

First_inning <- First_inning |>
  rowwise() |>
  # adding home
  mutate(home = helper_for_home(neutral_venue,
                                batting_team, team1)) |>
  rowwise() |> 
  # adding away
  mutate(away = helper_for_away(neutral_venue,
                                batting_team, team1)) |> 
  group_by(id) |>
  # adding fd ( form difference)
  mutate(fd = team1_form - team2_form) |> 
  # adding toss
  mutate(toss = ((batting_team == toss_winner) |> as.numeric())) |> 
  # adding wl( wicket lost)
  mutate(wl = cumsum(is_wicket) |> as.numeric()) |>
  # adding wrl( wicket resource lost)
  mutate(total = cumsum(total_runs)) |>
  # adding  rpo ( runs per over)
  mutate(rpo = total / (over + ball / 6)) |>
  mutate(ball_num = over * 6 + ball_clean) |>
  # adding ball_rem 
  mutate(ball_rem = 120 -  ball_num) |>
  # adding win_loss
  mutate(win_loss = (batting_team == winner) |>  as.numeric()) |> 
  # adding extra line of data for each game since we need to account at the 
  # start of the game there needs to be 120 ball remaining data
  group_modify(~ helper_for_start_first(.x)) |> 
  # removing end of game data i.e. 0 balls remaining
  filter(ball_rem != 0 ) |> 
  group_by(id) |> 
  rowwise() |> 
  # adding wrl 
  mutate(wrl = helper_for_wrl(ball_rem, wl))


# Getting target variable from first innings data for second innings data 
# which will be required to calculate rrpo

Target <- First_inning |>
  select(id, total) |>
  group_by(id) |> 
  slice_tail(n = 1)
colnames(Target)[2] <- "target"

Target$target <- Target$target + 1

# Repeating the same thing for second innings data.
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
  mutate(wrl = helper_for_wrl(ball_rem, wl)) 

First_inning <- First_inning |> 
  select(id,date, win_loss, home, away,rd,
         fd,toss, wl, wrl, rpo, ball_rem)  |> 
  arrange(date) 

Second_inning <- Second_inning |> 
  select(id, date, win_loss, home, away,toss,rd,
         fd, wl, wrl, rpo, rrpo, ball_rem)  |> 
  arrange(date) 

# Write the data
# write.csv(First_inning, file = here::here("01-data","first-innings.csv"), row.names = FALSE)
# write.csv(Second_inning, file = here::here("01-data","second-innings.csv"), row.names = FALSE)

