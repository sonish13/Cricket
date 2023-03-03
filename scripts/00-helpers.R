clean_teamnames <- function(string) {
  unname(c(
    "Chennai Super Kings"         = "Chennai Super Kings", 
    
    "Deccan Chargers"             = "Deccan Chargers / Sunrisers Hyderabad",
    "Sunrisers Hyderabad"         = "Deccan Chargers / Sunrisers Hyderabad",
    
    "Delhi Capitals"              = "Delhi Capitals", 
    "Delhi Daredevils"            = "Delhi Capitals",
    
    "Gujarat Lions"               = "Gujarat Lions", 
    "Kings XI Punjab"             = "Kings XI Punjab", 
    "Kochi Tuskers Kerala"        = "Kochi Tuskers Kerala", 
    "Kolkata Knight Riders"       = "Kolkata Knight Riders", 
    "Mumbai Indians"              = "Mumbai Indians", 
    "Rajasthan Royals"            = "Rajasthan Royals",
    
    "Pune Warriors"               = "Rising Pune Supergiants", 
    "Rising Pune Supergiant"      = "Rising Pune Supergiants", 
    "Rising Pune Supergiants"     = "Rising Pune Supergiants", 
    
    "Royal Challengers Bangalore" = "Royal Challengers Bangalore"
  )[string])
}
# Helper to identify the number of extras which is not counted as bals in an over
helper_for_extras <- function(df) {
  df$ball - cumsum((df$extras_type == "noball" | df$extras_type =="wides") |>
                     replace_na(FALSE))
}

# This function makes the number of balls clean so as we don't have ball number
# greater than 6 in  our data
helper_for_grouping <- function(df) {
  df |> 
    group_by(over) |> 
    mutate(ball_clean = ball - cumsum((extras_type == "noballs" | 
                                       extras_type == "wides") |> 
                                      replace_na(FALSE)))
}

# Save <- function(df) {
#   path = here::here("data","first",paste0(unique(df$id),".csv"))
#   write_csv(df, path)
# }

# helper to calculate home variabke
helper_for_home <- function(neutral, batting_team, team1) {
  if(neutral) return(0)
  batting_team == team1
}

# helper to calculate away variable
helper_for_away <- function(neutral, batting_team, team1) {
  if(neutral) return(0)
  batting_team != team1
}

# helper to calculate rating difference
helper_for_difference <- function(team1, team2, batting_team, team1_elo, team2_elo) {
  if(batting_team == team1) return(team1_elo - team2_elo)
  team2_elo - team1_elo
}

weights_for_form <- c(1,2,3,4,5,0)/15
# helper to calculate form difference

helper_for_fd <- function(x, w = weights_for_form) {
  if(length(x) == 6) return(weighted.mean(x,w))
  if(length(x) == 1) return(0)
  weighted.mean(x, w[(7 - length(x)):6])
}
# helpers to add ball rem = 120 in each games.
helper_for_start_first <- function(df) {
  x <- df |> slice_head(n=1)
  x$total <- 0
  x$wl <- 0
  x$ball_rem <- 120
  x$rpo <- 0
  rbind(x, df)
}

helper_for_start_second <- function(df) {
  x <- df |> slice_head(n=1)
  x$total <- 0
  x$wl <- 0
  x$ball_rem <- 120
  x$rpo <- 0
  rrpo = (x$target - x$total) / x$ball_rem * 6
  rbind(x, df)
}

# wrapper for bestglm to fit our need
helper_for_bestglm <- function(df, criteria, response, variables) {
  X <- df |> select(vars)
  y <- df |> select(response)
  XY <- as.data.frame(cbind(X,y))
  names(Xy) <- c(variables, response)
  fit <- bestglm(Xy, family = binomial, IC = Criteria)
  out <- (variables %in% 
            (fit$BestModel$coefficients |> 
               names())  ) |> 
    as.matrix() |> 
    t() |> 
    as.data.frame()
  colnames(out) <- variables
  out
}

helper_for_bestglm_first <- function(df, Criteria) {
  X <- df[,c(3:7,9,10)]
  y <- df[,2]
  Xy <- as.data.frame(cbind(X,y))
  names(Xy) <- c("home","away","toss","rd","fd","wrl","rpo","win_loss")
  fit <- bestglm(Xy, family = binomial, IC = Criteria)
  out <- (c("home","away","toss","rd","fd","wrl","rpo") %in% 
            (fit$BestModel$coefficients |> names())  ) |>
    as.matrix() |> t() |> as.data.frame()
  colnames(out) <- c("home","away","toss","rd","fd","wrl","rpo")
  out
}

helper_for_bestglm_second <- function(df, Criteria) {
  X <- df[,c(3:7,9,11)]
  y <- df[,2]
  Xy <- as.data.frame(cbind(X,y))
  Xy
  names(Xy) <- c("home","away","toss","rd","fd","wrl","rrpo","win_loss")
  fit <- bestglm(Xy, family = binomial, IC = Criteria)
  out <- (c("home","away","toss","rd","fd","wrl","rrpo") %in% 
            (fit$BestModel$coefficients |> names())  ) |>
    as.matrix() |> t() |> as.data.frame()
  colnames(out) <- c("home","away","toss","rd","fd","wrl","rrpo")
  out
}

# helper_for_coeff_first <- function(data) {
#   data_models <- dlply(data, "ball_rem", function(df) 
#     glm(win_loss ~ away + rd + wrl + rpo,
#         family = binomial(link = "logit"), data = df))
#   
#   # Apply coef to each model and return a data frame
#   data_coeff <- ldply(data_models, coef)
#   
#   # Smooth rpo
#   ss_f_rpo <- smooth.spline(x = data_coeff$ball_rem, y = data_coeff$rpo, cv = TRUE)
#   df_ss_f_rpo <- data.frame(ball_rem = 1:120, "s_rpo" = ss_f_rpo$y)
#   data <- full_join(data, df_ss_f_rpo, by = "ball_rem")
#   
#   # Refit with smoothed rpo
#   data_models_rpo <- dlply(data, "ball_rem", function(df)
#     glm(win_loss ~ away + rd + wrl + offset(s_rpo * rpo),
#         family = binomial, data = df))
#   
#   data_coeff_rpo <- ldply(data_models_rpo, coef)
#   
#   # Smooth rd
#   ss_f_rd <- smooth.spline(x = data_coeff_rpo$ball_rem, y = data_coeff_rpo$rd, cv = TRUE)
#   df_ss_f_rd <- data.frame(ball_rem = 1:120, "s_rd" = ss_f_rd$y)
#   data <- full_join(data, df_ss_f_rd, by = "ball_rem")
#   
#   # Refit with smoothed rd
#   data_models_rd <- dlply(data, "ball_rem", function(df)
#     glm(win_loss ~ away + offset(s_rd *rd)  +  wrl + offset(s_rpo * rpo),
#         family = binomial, data = df))
#   
#   data_coeff_rd <- ldply(data_models_rd, coef)
#   
#   data_coeff_rd$wrl <- na.spline(data_coeff_rd$wrl, na.rm = FALSE)
#   
#   # Smooth wrl
#   ss_f_wrl <- smooth.spline(x = data_coeff_rd$ball_rem, y = data_coeff_rd$wrl, cv = TRUE)
#   df_ss_f_wrl <- data.frame(ball_rem = 1:120, "s_wrl" = ss_f_wrl$y)
#   data <- full_join(data, df_ss_f_wrl, by = "ball_rem")
#   
#   # Refit with smoothed wrl
#   data_models_wrl <- dlply(data, "ball_rem", function(df)
#     glm(win_loss ~ away + offset(s_rd *rd)  + offset(s_wrl * wrl) + offset(s_rpo * rpo),
#         family = binomial, data = df))
#   
#   data_coeff_wrl <- ldply(data_models_wrl, coef)
#   
#   
#   # Smooth away
#   ss_f_away <- smooth.spline(x = data_coeff_wrl$ball_rem, y = data_coeff_wrl$away, cv = TRUE)
#   df_ss_f_away <- data.frame(ball_rem = 1:120, "s_away" = ss_f_away$y)
#   data <- full_join(data, df_ss_f_away, by = "ball_rem")
#   
#   # Refit with smoothed away
#   data_models_away <- dlply(data, "ball_rem", function(df)
#     glm(win_loss ~ offset(s_away * away) + offset(s_rd *rd)  + offset(s_wrl * wrl) + offset(s_rpo * rpo),
#         family = binomial, data = df))
#   
#   data_coeff_away <- ldply(data_models_away, coef)
#   
#   # Smooth intercept
#   ss_f_intercept <- smooth.spline(x = data_coeff_away$ball_rem, y = data_coeff_away$`(Intercept)`, cv = TRUE)
#   df_ss_f_intercept <- data.frame(ball_rem = 1:120, "s_intercept" = ss_f_intercept$y)
#   data <- full_join(data, df_ss_f_intercept, by = "ball_rem")
#   
# }
# 
# 
# helper_for_LOOCV_first <- function(test_id, df) {
#   train <- df |> filter(id != test_id)
#   test <- df |> filter(id == test_id)
#   coeff <- helper_for_coeff_first(train)
#   coeff <- coeff |> 
#     select(ball_rem, s_intercept, s_away,s_rd,s_wrl,s_rpo) |> 
#     group_by(ball_rem) |> 
#     slice_head(n = 1) |> 
#     ungroup()
#   helper_for_error_first(test_df = test, coeff_df = coeff)
# }