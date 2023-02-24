library(plyr)
library(tidyverse)
source("helpers.R")
library(lemon)
library(patchwork)
library(splines)
library(gam)
library(npreg)
library(zoo)
library(tictoc)
library(glue)

# This function calculates all the coefficients and smoothes them.

# Test

First_data <- read.csv("First.csv")
Second_data <- read.csv("Second.csv")

train <- Second_data |> filter(id != 335982)
test <- Second_data |> filter(id == 335982)

helper_for_coeff <- function(data, vars) {
  num_of_vars <- length(vars)
  new_vars <- (1: num_of_vars) |>  
    map(~glue("s_",vars[.x])) |> 
    unlist()
  vars_with_offset <-(1:num_of_vars) |>
    map(~glue::glue("offset(",new_vars[.x],"*",vars[.x],")")) |> 
    unlist()

  # Initial Coefficients
  fmla = glue("win_loss ~ ",glue_collapse(vars, sep = " + "))
  data_models <- dlply(data, "ball_rem", function(df)
    glm(fmla,
        family = binomial(link = "logit"), data = df))
  data_coeff <- ldply(data_models, coef, .id = NULL)
  
  # Smooth all the coefficients
 
  for (i in 1:num_of_vars) {
      temp <- na.spline(data_coeff[vars[i]], na.rm = FALSE)
      data_coeff[vars[i]] <- temp
      df_spline <- data.frame(x = c(data_coeff$ball_rem), "y" = c(data_coeff[{vars[i]}]))
      colnames(df_spline) <- c("x", "y")
      df_spline <- smooth.spline(x = df_spline$x, y = df_spline$y,  cv = TRUE)
      df_spline <- data.frame("ball_rem" = 1:120, y = df_spline$y)
      colnames(df_spline) <- c("ball_rem", new_vars[[i]])
      data <- full_join(data, df_spline, by = "ball_rem")

      fmla <- glue("win_loss ~ ", glue_collapse(c(vars_with_offset[1:i],
                                                  vars[-(1:i)]), sep = " + "))
      data_models <- dlply(data, "ball_rem", function(df)
        glm(fmla,
            family = binomial(link = "logit"), data = df))
      data_coeff <- ldply(data_models, coef, .id = NULL)
      #fmla |> print()
  }
# Smooth Intercept
ss_f_intercept <- smooth.spline(x = data_coeff$ball_rem, y = data_coeff$`(Intercept)`, cv = TRUE)
df_ss_f_intercept <- data.frame(ball_rem = 1:120, "s_intercept" = ss_f_intercept$y)
data <- full_join(data, df_ss_f_intercept, by = "ball_rem")

# Return data with smooth intercept present 
data
}

# helper_for_error <- function(test_df, coeff_df, vars) {
#   num_of_vars <- length(vars)
#   new_vars <- (1:num_of_vars) |> 
#     map(~ glue("s_",vars[.x])) |> 
#     unlist()
#   temp_vars <- (1:num_of_vars) |> 
#     map(~ glue(new_vars[.x]," * ",vars[.x])) |> 
#     unlist() |> glue_collapse(sep = " + ")
# 
#   df <- left_join(coeff_df, test_df, by = "ball_rem")
#   df |> mutate(prob = 1/(1 + exp(-(s_intercept + s_home * home +
#                                      s_wrl * wrl + s_rpo * rpo)))) |> 
#     mutate(pred = prob > 0.5 ) |> 
#     mutate(accurate = (pred == win_loss)) |> 
#     select(id, ball_rem, prob, accurate)
# }

# helper_for_LOOCV <- function(test_id, df, vars =c("rpo","rd")) {
#   train <- df |> filter(id != test_id)
#   test <- df |> filter(id == test_id)
#   coeff <- helper_for_coeff(train, vars)
#   coeff <- coeff |> 
#     select(c(ball_rem, {{vars}})) |> 
#     group_by(ball_rem) |> 
#     slice_head(n = 1) |> 
#     ungroup()
#   helper_for_error(test_df = test, coeff_df = coeff, vars = vars)
# }

# out <- helper_for_coeff(train, vars =c("rpo","wrl","rd","toss","home","away"))
# out |> str()
# 
# ?na.spline


