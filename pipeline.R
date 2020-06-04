### pipeline.R
### pipeline for simulation, graphics, and analysis of home field advantage
### in the era of COVID-19 w/out fans
source("hfa_sims.R")
source("xg_graphics.R")
source("model_fit.R")
source("prediction_helpers.R")


pipeline_refresh <- function(league, restart_date, fill_col, model_refresh = F) {
  if(!dir.exists(gsub("\\s", "_", tolower(league)))) {
    dir.create(gsub("\\s", "_", tolower(league)))
    dir.create(paste0(gsub("\\s", "_", tolower(league)), "/figures"))
  }
  exp_pts_graphic(league, restart_date, fill_col)
  if(!file.exists(paste0(gsub("\\s", "_", tolower(league)), "/model.rds")) | model_refresh) {
    model_fit(league, restart_date)
  } 
  hfa_reduction_sims(league, restart_date, fill_col)
  xg_graphics(league, restart_date)
}


### German Bundesliga
pipeline_refresh("German Bundesliga", "2020-05-16", "red")
  
### 2. German Bundesliga
pipeline_refresh("German 2. Bundesliga", "2020-05-16", "red")

### Danish SAS-Ligaen
pipeline_refresh("Danish SAS-Ligaen", "2020-05-31", "skyblue")

### Austrian T-Mobile Bundesliga
pipeline_refresh("Austrian T-Mobile Bundesliga", "2020-06-02", "pink")

### Portuguese Liga
pipeline_refresh("Portuguese Liga", "2020-06-03", "seagreen")


