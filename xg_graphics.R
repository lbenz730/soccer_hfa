### xg graphics
xg_graphics <- function(league_, restart_date) {
  x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  post_covid <- filter(x, date >= restart_date, date <= Sys.Date(), league == league_)
  tibble("xg" = c(post_covid$xg1, post_covid$xg2),
         "goals" = c(post_covid$score1, post_covid$score2)) %>%
    mutate("loc" = rep(c("Home", "Away"), each = nrow(.)/2),
           "week" = paste0("Matchday: ", get_matchdays(.))
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
         subtitle = glue("{league_}: {restart_date} to Present"))
  ggsave(paste0(gsub("\\s", "_", tolower(league_)), "/figures/shot_based_xg.png"), height = 9/1.2, width = 16/1.2)
  
  tibble("xg" = c(post_covid$nsxg1, post_covid$nsxg2),
         "goals" = c(post_covid$score1, post_covid$score2)) %>%
    mutate("loc" = rep(c("Home", "Away"), each = nrow(.)/2),
           "week" = paste0("Matchday: ", get_matchdays(.))
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
         subtitle = glue("{league_}: {restart_date} to Present"))
  ggsave(paste0(gsub("\\s", "_", tolower(league_)), "/figures/non_shot_based_xg.png"), height = 9/1.2, width = 16/1.2)
  
}

get_matchdays <- function(df) {
  weeks <- 1:(ceiling(nrow(df)/18))
  full_weeks <- floor(nrow(df)/18)
  if(0.5 * nrow(df)%%18 != 0) {
    extra <- 0.5 * nrow(df)%%18
    rep_vec <- rep(weeks, c(rep(9, full_weeks), extra))
  } else {
    rep_vec <- rep(weeks, rep(9, full_weeks)) 
  }
  

  return(rep(25 + rep_vec, 2))
}
