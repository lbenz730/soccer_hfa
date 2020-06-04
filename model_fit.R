library(tidyverse)
library(furrr)
library(patchwork)
options("future.fork.enable" = T)
plan(multiprocess)
source("prediction_helpers.R")


model_fit <- function(league_, restart_date) {
  dif <- read_csv("draw_infation_factors.csv") %>%
    filter(league == league_) %>%
    pull(tie_inflation)
  
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  
  pre_covid <- filter(x, date < restart_date, league == league_)
  n <- nrow(pre_covid)
  df <- tibble(
    "goals" = c(pre_covid$proj_score1, pre_covid$proj_score2),
    "spi" = c(pre_covid$spi1, pre_covid$spi2),
    "opp_spi" = c(pre_covid$spi2, pre_covid$spi1),
    "importance" = c(pre_covid$importance1, pre_covid$importance2),
    "opp_importance" = c(pre_covid$importance2, pre_covid$importance1),
    "home" = rep(1:0, each = n)
  ) 
  
  
  ### Fit Model
  #model <- glm(goals ~ ns(spi, 3) + ns(opp_spi, 3) + home, data = df, family = "poisson")
  model <- lm(log(goals) ~ ns(spi, 3) + ns(opp_spi, 3) + home, data = df)
  write_rds(model, paste0(gsub("\\s", "_", tolower(league_)), "/model.rds"))
  
  pre_covid <- get_predictions(pre_covid, 0, model, dif)
  
  p1 <- ggplot(pre_covid, aes(x = prob1, y = win_prob)) +
    geom_point(alpha = 0.2) +
    geom_abline(lty = 2, size = 1, color = "red") +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "FiveThirtyEight Probability",
         y = "Model Estimated Probability", 
         subtitle = "P(Home Team Win)") +
    scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    annotate("label", x = 0.25, y = 0.75, size = 4,
             label = paste0("Correlation: ", round(cor(pre_covid$prob1, pre_covid$win_prob, use = "pairwise.complete.obs"), 2)))
  
  
  p2 <- ggplot(pre_covid, aes(x = prob2, y = loss_prob)) +
    geom_point(alpha = 0.2) +
    geom_abline(lty = 2, size = 1, color = "red") +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "FiveThirtyEight Probability",
         y = "Model Estimated Probability", 
         subtitle = "P(Away Team Win)") +
    scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    annotate("label", x = 0.25, y = 0.75, size = 4,
             label = paste0("Correlation: ", round(cor(pre_covid$prob2, pre_covid$loss_prob, use = "pairwise.complete.obs"), 2)))
  
  
  p3 <- ggplot(pre_covid, aes(x = probtie, y = tie_prob)) +
    geom_point(alpha = 0.2) +
    geom_abline(lty = 2, size = 1, color = "red") +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "FiveThirtyEight Probability",
         y = "Model Estimated Probability", 
         subtitle = "P(Draw)") +
    scale_x_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    scale_y_continuous(limits = c(0,1), labels = function(x) paste0(100 * x, "%")) +
    annotate("label", x = 0.25, y = 0.75, size = 4,
             label = paste0("Correlation: ", round(cor(pre_covid$probtie, pre_covid$tie_prob, use = "pairwise.complete.obs"), 2)))
  
  p1 + p2 + p3 +
    plot_annotation(title = 'Calibration of Model Estimated Probabilities',
                    subtitle = league_,
                    theme = theme(plot.title = element_text(size = 24, hjust = 0.5),
                                  plot.subtitle = element_text(size = 20, hjust = 0.5))) 
  
  
  ggsave(paste0(gsub("\\s", "_", tolower(league_)), "/figures/model_calibration.png"), height = 9/1.2, width = 16/1.2)
}