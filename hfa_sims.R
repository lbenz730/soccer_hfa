library(tidyverse)
library(splines)
library(glue)
library(ggridges)
library(furrr)
options("future.fork.enable" = T)
plan(multiprocess)
source("prediction_helpers.R")

### Expected vs Observed Points Graphic
exp_pts_graphic <- function(league_, restart_date, fill_col) {
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  df_league <- filter(x, league == league_) %>%
    filter(date <= Sys.Date(), date >= restart_date) %>%
    mutate("exp_home_points" = 3 * prob1 + probtie,
           "home_points" = 3 * (score1 > score2) + 1 * (score1 == score2),
           "home_wp" =  1 * (score1 > score2) + 0.5 * (score1 == score2),
           "exp_home_wp" =   1 * prob1 + 0.5 * probtie)
  
  exp_home_pts <- sum(df_league$exp_home_points)
  home_pts <- sum(df_league$home_points)
  
  get_exp_home_points <- function(x) {
    case_when(runif(nrow(df_league)) <= df_league$prob1 ~ 3,
              runif(nrow(df_league)) <= df_league$prob1  + df_league$probtie ~ 1,
              T ~ 0) %>%
      sum()
  }
  
  set.seed(123)
  nsims <- 10000
  df <- tibble("exp_pts" = future_map_dbl(1:nsims, get_exp_home_points))
  
  
  ggplot(df, aes(x = exp_pts)) +
    geom_density(fill = fill_col, alpha = 0.2) +
    geom_vline(xintercept = home_pts, lty = 2, size = 1.2) +
    annotate(geom = "label", x = home_pts, y = 0.075, label = glue("Home Points: {home_pts}\nExpected Home Points: {round(exp_home_pts, 1)}")) +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5)) + 
    labs(x = "# of Points for Home Teams",
         y = "Density",
         title = "Distribution of Expected Home Team Points",
         subtitle = glue("{league_}: {restart_date} to Present"),
         caption = "Based on 10,000 sims using FiveThirtyEight SPI Model\n(10% HFA Reduction Already Factored In)") +
    scale_x_continuous(limits = c(-5,5) + c(min(df$exp_pts), max(df$exp_pts)))
  
  
  ggsave(paste0(gsub("\\s", "_", tolower(league_)), "/figures/exp_pts.png"), height = 9/1.2, width = 16/1.2)
}


### Sims w/ Custom Model and Varying HFA
hfa_reduction_sims <- function(league_, restart_date, fill_col) {
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  post_covid <- filter(x, date >= restart_date, date <= Sys.Date(), league == league_)
  model <- read_rds(paste0(gsub("\\s", "_", tolower(league_)), "/model.rds"))
  
  y <- future_map_dfr(seq(0, 1, 0.05), ~get_predictions(post_covid, .x, model))
  
  get_exp_home_points <- function(df) {
    case_when(runif(nrow(df)) <= df$win_prob ~ 3,
              runif(nrow(df)) <= df$win_prob  + df$tie_prob ~ 1,
              T ~ 0) %>%
      sum()
  }
  
  
  set.seed(123)
  nsims <- 10000
  sim <- function(x) {
    hfa <- seq(0,1,0.05)
    exp_pts <- map_dbl(hfa, ~ get_exp_home_points(filter(y, hfa_reduction == .x)))
    return(tibble("hfa_reduction" = hfa,
                  "exp_pts" = exp_pts))
  }
  
  df_sims <- future_map_dfr(1:nsims, sim)
  
  post_covid <- post_covid %>%
    mutate("exp_home_points" = 3 * prob1 + probtie,
           "home_points" = 3 * (score1 > score2) + 1 * (score1 == score2),
           "home_wp" =  1 * (score1 > score2) + 0.5 * (score1 == score2),
           "exp_home_wp" =   1 * prob1 + 0.5 * probtie)
  home_pts <- sum(post_covid$home_points)
  
  pvals <- 
    group_by(df_sims, hfa_reduction) %>%
    summarise("p_value" = mean(exp_pts <= home_pts),
              "min_pts" = min(exp_pts),
              "mean_pts" = mean(exp_pts))
  
  
  ggplot(df_sims, aes(x = exp_pts, y = as.factor(hfa_reduction))) +
    geom_density_ridges(scale = 1, fill = fill_col, alpha = 0.5) +
    geom_vline(xintercept = home_pts, lty = 2, size = 1.2) +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 18, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "Points Accrued by Home Team",
         y = "Reduction in Home Field Advantage", 
         title = "Distribution of Expected Home Team Points w/ Varying HFA",
         subtitle = glue("{league_}: {restart_date} to Present")) +
    scale_y_discrete(labels = function(x) paste0(100 * as.numeric(x), "%"))
  ggsave(paste0(gsub("\\s", "_", tolower(league_)), "/figures/sims.png"), height = 9/1.2, width = 16/1.2)
}


