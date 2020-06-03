predict_lambda <- function(df_row, model) {
  df <- tibble(
    "goals" = c(df_row$score1, df_row$score2),
    "spi" = c(df_row$spi1, df_row$spi2),
    "opp_spi" = c(df_row$spi2, df_row$spi1),
    "importance" = c(df_row$importance1, df_row$importance2),
    "opp_importance" = c(df_row$importance2, df_row$importance1),
    "home" = 1:0
  )
  lambdas <- predict(model, newdata = df, type = "response")
  lambdas <- tibble("lambda1" = lambdas[1], "lambda2" = lambdas[2])
  return(lambdas)
}

match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  score_matrix <- score_matrix/sum(score_matrix)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(tibble("win_prob" = win_prob, "tie_prob" = tie_prob, "loss_prob" = loss_prob))
}

get_predictions <- function(df, hfa_reduction = 0, model) {
  model$coefficients['home'] <- (1 - hfa_reduction) * model$coefficients['home']
  df <- 
    df %>%
    bind_cols(future_map_dfr(1:nrow(df), ~predict_lambda(df[.x,], model))) %>%
    bind_cols(future_map2_dfr(.$lambda1, .$lambda2, match_probs))
  df$hfa_reduction <- hfa_reduction
  return(df)
}

unvig_odds <- function(x) {
 y <- ifelse(x > 0, 100/(100 + x), abs(x)/(100 + abs(x)))
 return(y/sum(y))
}
