library(tidyverse)
library(splines)
library(glue)
library(ggridges)
library(furrr)
library(ggimage)
options("future.fork.enable" = T)
plan(multiprocess)
source("prediction_helpers.R")
source("xg_graphics.R")

### Expected vs Observed Points Graphic
exp_pts_graphic <- function(league_, alias, restart_date, fill_col) {
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  df_league <- filter(x, league == league_) %>%
    filter(date <= Sys.Date(), date >= restart_date, !is.na(score1)) %>%
    mutate("exp_home_points" = 3 * prob1 + probtie,
           "home_points" = 3 * (score1 > score2) + 1 * (score1 == score2),
           "home_wp" =  1 * (score1 > score2) + 0.5 * (score1 == score2),
           "exp_home_wp" =   1 * prob1 + 0.5 * probtie)
  
  exp_home_pts <- sum(df_league$exp_home_points)
  home_pts <- sum(df_league$home_points)
  n_games <- nrow(df_league)
  
  get_exp_home_points <- function(x) {
    p <- runif(nrow(df_league))
    case_when(p <= df_league$prob1 ~ 3,
              p <= df_league$prob1  + df_league$probtie ~ 1,
              T ~ 0) %>%
      sum()
  }
  
  set.seed(123)
  nsims <- 10000
  df <- tibble("exp_pts" = future_map_dbl(1:nsims, get_exp_home_points))
  dens <- density(df$exp_pts)
  
  ggplot(df, aes(x = exp_pts)) +
    geom_density(fill = fill_col, alpha = 0.2) +
    geom_vline(xintercept = home_pts, lty = 2, size = 1.2) +
    annotate(geom = "label", x = home_pts, y = max(dens$y) * 1.1,
             label = glue("Home Points: {home_pts}\nExpected Home Points: {round(exp_home_pts, 1)}\n(# of Games: {n_games})")) +
    geom_image(data = tibble("x" = quantile(dens$x, 0.95), "y" = quantile(dens$y, 0.95), "image" = get_logo(league_)),
               aes(x = x, y = y, image = image), size = 0.2) +
    theme_bw() +
    theme(axis.title = element_text(size = 20, hjust = 0.5),
          plot.title = element_text(size = 28, hjust = 0.5),
          plot.subtitle = element_text(size = 22, hjust = 0.5),
          plot.caption = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    labs(x = "# of Points for Home Teams",
         y = "Density",
         title = "Distribution of Expected Home Team Points",
         subtitle = glue("{alias}: {restart_date} to Present"),
         caption = "Based on 10,000 sims using FiveThirtyEight SPI Model\n(10% HFA Reduction Already Factored In)") +
    scale_x_continuous(limits = c(-5,5) + c(min(df$exp_pts), max(df$exp_pts))) +
    scale_y_continuous(limits = c(0, max(dens$y) * 1.2))
  
  
  
  ggsave(paste0(gsub("\\s", "_", tolower(alias)), "/figures/exp_pts.png"), height = 9/1.2, width = 16/1.2)
}


### Sims w/ Custom Model and Varying HFA
hfa_reduction_sims <- function(league_, alias, restart_date, fill_col) {
  dif <- read_csv("draw_infation_factors.csv") %>%
    filter(league == league_) %>%
    pull(tie_inflation)
  
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  post_covid <- filter(x, date >= restart_date, date <= Sys.Date(), league == league_, !is.na(score1))
  n_games <- nrow(post_covid)
  model <- read_rds(paste0(gsub("\\s", "_", tolower(alias)), "/model.rds"))
  
  y <- future_map_dfr(seq(0, 1, 0.05), ~get_predictions(post_covid, .x, model, dif))
  
  get_exp_home_points <- function(df) {
    p <- runif(nrow(df))
    case_when(p <= df$win_prob ~ 3,
              p <= df$win_prob  + df$tie_prob ~ 1,
              T ~ 0) %>%
      sum()
  }
  
  sim <- function(x) {
    hfa <- seq(0,1,0.05)
    exp_pts <- map_dbl(hfa, ~ get_exp_home_points(filter(y, hfa_reduction == .x)))
    return(tibble("hfa_reduction" = hfa,
                  "exp_pts" = exp_pts))
  }
  
  set.seed(123)
  nsims <- 10000
  
  df_sims <- future_map_dfr(1:nsims, sim) %>%
    mutate("league" = league_)
  
  write_csv(df_sims, paste0(gsub("\\s", "_", tolower(alias)), "/sims/sims.csv"))
  
  post_covid <- post_covid %>%
    mutate("exp_home_points" = 3 * prob1 + probtie,
           "home_points" = 3 * (score1 > score2) + 1 * (score1 == score2),
           "home_wp" =  1 * (score1 > score2) + 0.5 * (score1 == score2),
           "exp_home_wp" =   1 * prob1 + 0.5 * probtie)
  home_pts <- sum(post_covid$home_points)
  
  ecdf <- 
    group_by(df_sims, hfa_reduction) %>%
    summarise("ecdf" = mean(exp_pts <= home_pts),
              "min_pts" = min(exp_pts),
              "q025" = quantile(exp_pts, 0.025),
              "q975" = quantile(exp_pts, 0.975),
              "mean_pts" = mean(exp_pts),
              "median_pts" = median(exp_pts))
  
  write_csv(ecdf, paste0(gsub("\\s", "_", tolower(alias)), "/sims/simulation_ecdf.csv"))
  
  
  ggplot(df_sims, aes(x = exp_pts, y = as.factor(hfa_reduction))) +
    geom_density_ridges(scale = 0.9, fill = fill_col, alpha = 0.5, quantile_lines = T, quantiles = 2) +
    geom_vline(xintercept = home_pts, lty = 2, size = 1.2) +
    theme_bw() +
    annotate("label", x = 0.9 * max(df_sims$exp_pts), y = "0.7",
             label = paste(glue("Home Points: {home_pts}\nExpected Home Points w/ Full HFA: {round(ecdf$mean_pts[1], 1)}"),
                           glue("95% CI: ({round(ecdf$q025[1], 1)}, {round(ecdf$q975[1], 1)})"),
                           glue("Expected Home Points w/ No HFA: {round(ecdf$mean_pts[21], 1)}"),
                           glue("95% CI: ({round(ecdf$q025[21], 1)}, {round(ecdf$q975[21], 1)})"), 
                           glue("# of Games: {n_games}"),
                           sep = "\n")
             
    ) +
    geom_image(data = tibble("x" = 0.95 * max(df_sims$exp_pts), "y" = "0.95", "image" = get_logo(league_)),
               aes(x = x, y = y, image = image), size = 0.1) +
    theme(axis.title = element_text(size = 20, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 28, hjust = 0.5),
          plot.subtitle = element_text(size = 22, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "Points Accrued by Home Teams",
         y = "Reduction in Home Field Advantage", 
         title = "Distribution of Expected Home Team Points w/ Varying HFA",
         subtitle = glue("{alias}: {restart_date} to Present")) +
    scale_y_discrete(labels = function(x) paste0(100 * as.numeric(x), "%"))
  ggsave(paste0(gsub("\\s", "_", tolower(alias)), "/figures/sims.png"), height = 9/1.2, width = 16/1.2)
}


