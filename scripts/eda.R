library("tidyverse"); library("patchwork"); library("scales")
library("extrafont"); library("glue")
theme_set(theme_minimal(base_family = "Roboto Condensed"))
theme_update(panel.grid.minor = element_blank())






# read in ball data -------------------------------------------------------

read_csv(here::here("00-raw-data", "IPL Ball-by-Ball 2008-2020.csv"))

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


# basic checks
ball_data %>% count(inning, sort = TRUE)

ball_data %>% 
  filter(inning == 1L)

ball_data %>% 
  filter(inning == 2L)

skimr::skim(ball_data)



