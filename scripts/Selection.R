library(tidyverse)
library(bestglm)
source("helpers.R")

First <- read.csv("First.csv")
Second <- read.csv("Second.csv")
first_glm_bic <- First |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_first(.x, Criteria = "BIC"))

second_glm_bic <- Second |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_second(.x, Criteria = "BIC"))

first_glm_aic <- First |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_first(.x, Criteria = "AIC"))

second_glm_aic <- Second |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_second(.x, Criteria = "AIC"))

first_glm_cv <- First |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_first(.x, Criteria = "CV"))

second_glm_cv <- Second |>
  group_by(ball_rem) |>
  group_modify(~ helper_for_bestglm_second(.x, Criteria = "CV"))

# write.csv(first_glm_cv, "first_glm_cv_2.csv")
# write.csv(second_glm_cv, "second_glm_cv_2.csv")


first_glm_aic |> colSums()
second_glm_aic |> colSums()

first_glm_bic |> colSums()
second_glm_bic |> colSums()

first_glm_cv |> colSums()
second_glm_cv |> colSums()

