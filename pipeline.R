### pipeline.R
### pipeline for simulation, graphics, and analysis of home field advantage
### in the era of COVID-19 w/out fans
source("hfa_sims.R")
source("xg_graphics.R")
source("model_fit.R")
source("prediction_helpers.R")
source("update_readme.R")

pipeline_refresh <- function(league, alias, restart_date, fill_col, model_refresh = F) {
  if(!dir.exists(gsub("\\s", "_", tolower(alias)))) {
    dir.create(gsub("\\s", "_", tolower(alias)))
    dir.create(paste0(gsub("\\s", "_", tolower(alias)), "/figures"))
    dir.create(paste0(gsub("\\s", "_", tolower(alias)), "/sims"))
  }    
  exp_pts_graphic(league, alias, restart_date, fill_col)
  if(!file.exists(paste0(gsub("\\s", "_", tolower(alias)), "/model.rds")) | model_refresh) {
    model_fit(league, alias, restart_date)
  } 
  hfa_reduction_sims(league, alias, restart_date, fill_col)
  xg_graphics(league, alias, restart_date)
}

league_info <- read_csv("league_info.csv")
x <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
league_info <- 
  league_info %>%
  select(-n_games) %>%
  left_join(filter(x, date >= "2020-05-16", !is.na(score1)) %>%
               group_by(league) %>%
               summarise("n_games" = n()),
             by = "league")

for(i in 1:nrow(league_info)) {
  last_match <- 
    filter(x, league == league_info$league[i]) %>%
    filter(!is.na(score1)) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    pull(date)
  if((is.na(league_info$last_refresh[i]) & last_match >= league_info$restart_date[i]) | 
     (!is.na(league_info$last_refresh[i]) & last_match > league_info$last_refresh[i]) |
     last_match == Sys.Date()) {
    print(glue("Refreshing: {league_info$league[i]}"))
    pipeline_refresh(league_info$league[i], league_info$alias[i], 
                     league_info$restart_date[i], league_info$color[i])
    league_info$last_refresh[i] <- Sys.Date()
  }
}
write_csv(league_info, "league_info.csv")
update_readme()
files <- dir(recursive = T)
map(files[grepl("figures/sims.png", files)], ~file.copy(.x, paste0("simulation_figures/", gsub("/figures/sims.png", "", .x), "_sims.png"), overwrite = T))



       