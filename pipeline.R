### pipeline.R
### pipeline for simulation, graphics, and analysis of home field advantage
### in the era of COVID-19 w/out fans
source("hfa_sims.R")
source("xg_graphics.R")
source("model_fit.R")
source("prediction_helpers.R")

### German Bundesliga
league <- "German Bundesliga"
restart_date <- "2020-05-16"
fill_col <- "red"

exp_pts_graphic(league, restart_date, fill_col)
if(!file.exists(paste0(gsub("\\s", "_", tolower(league)), "/model.rds"))) {
  model_fit(league, restart_date)
} 
hfa_reduction_sims(league, restart_date, fill_col)
xg_graphics(league, restart_date)
