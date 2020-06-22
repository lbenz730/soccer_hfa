library(tidyverse)
files <- dir(recursive = T)
model_files <- files[str_detect(files, "model.rds")]

league_info <- read_csv("league_info.csv")

hfa_df <- function(alias_) {
  df_league <- filter(league_info, alias == alias_)
  model <- read_rds(paste0(gsub("\\s", "_", tolower(alias_)), "/model.rds"))
  df_league <- 
    df_league %>%
    mutate("hfa" = 2 * model$coefficients['home'],
           "std_error" = sqrt(2) * summary(model)$coefficients['home', 'Std. Error'])
  return(df_league)
}

df_hfa <- map_dfr(league_info$alias, hfa_df)

ggplot(df_hfa, aes(x = hfa, y = fct_reorder(alias, hfa))) +
  geom_point() +
  geom_segment(aes(x = -2 * se + hfa, xend = 2 * se + hfa, yend = alias)) +
  ggimage::geom_image(aes(image = logo_url)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16, hjust = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        legend.position = "bottom")
