update_readme <- function() {
  league_info <- read_csv("league_info.csv") %>%
    filter(!is.na(n_games)) %>%
    mutate("id" = paste0(gsub("\\s", "_", tolower(alias)), "/figures/sims.png")) %>%
    arrange(id)
  
  read_me <- read_lines("README.md")
  read_me <- gsub("Updated 2020-\\d+-\\d+"  , paste0("Updated ", Sys.Date()), read_me)
  files <- dir(recursive = T)
  files <- files[grepl("/figures/sims.png", files)]
  files <- files[order(league_info$n_games, decreasing = T)]
  files <- paste0("  <img src=\"", files, "\" width=\"450\" />")
  ix_start <- which(read_me == "<p float=\"left\">")
  
  write_lines(c(read_me[1:ix_start], files, read_me[length(read_me)]), "README.md")
}