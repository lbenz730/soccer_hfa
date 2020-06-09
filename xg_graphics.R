### xg graphics
xg_graphics <- function(league_, alias, restart_date) {
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  post_covid <- filter(x, date >= restart_date, date <= Sys.Date(), league == league_)
  n_teams <- filter(x, league == league_, season == 2019) %>%
    pull(team1) %>%
    n_distinct()
  tibble("xg" = c(post_covid$xg1, post_covid$xg2),
         "goals" = c(post_covid$score1, post_covid$score2)) %>%
    mutate("loc" = rep(c("Home", "Away"), each = nrow(.)/2),
           "week" = paste0("Post-COVID Matchday: ", get_matchdays(., n_teams))
    ) %>%
    ggplot(aes(x = xg, y = goals)) +
    geom_point(aes(color = loc), size = 3) +
    facet_wrap(~week) +
    geom_abline() +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "Shot Based xG",
         y = "Goals Scored",
         color = "",
         title = "xG vs. Goals Scored",
         subtitle = glue("{alias}: {restart_date} to Present"))
  ggsave(paste0(gsub("\\s", "_", tolower(alias)), "/figures/shot_based_xg.png"), height = 9/1.2, width = 16/1.2)
  
  tibble("xg" = c(post_covid$nsxg1, post_covid$nsxg2),
         "goals" = c(post_covid$score1, post_covid$score2)) %>%
    mutate("loc" = rep(c("Home", "Away"), each = nrow(.)/2),
           "week" = paste0("Post-COVID Matchday: ", get_matchdays(., n_teams))
    ) %>%
    ggplot(aes(x = xg, y = goals)) +
    geom_point(aes(color = loc), size = 3) +
    facet_wrap(~week) +
    geom_abline() +
    theme_bw() +
    theme(axis.title = element_text(size = 16, hjust = 0.5),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "Non-Shot Based xG",
         y = "Goals Scored",
         color = "",
         title = "xG vs. Goals Scored",
         subtitle = glue("{alias}: {restart_date} to Present"))
  ggsave(paste0(gsub("\\s", "_", tolower(alias)), "/figures/non_shot_based_xg.png"), height = 9/1.2, width = 16/1.2)
  
}

get_matchdays <- function(df, n_teams) {
  weeks <- 1:(ceiling(nrow(df)/n_teams))
  full_weeks <- floor(nrow(df)/n_teams)
  if(0.5 * nrow(df) %% n_teams != 0) {
    extra <- 0.5 * nrow(df) %% n_teams
    rep_vec <- rep(weeks, c(rep(n_teams/2, full_weeks), extra))
  } else {
    rep_vec <- rep(weeks, rep(n_teams/2, full_weeks)) 
  }
  
  
  return(rep(rep_vec, 2))
}

get_logo <- function(league_) {
  read_csv("league_info.csv") %>%
    filter(league == league_) %>%
    pull(logo_url)
}
