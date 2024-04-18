library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

data <- read.csv("all_video_games(cleaned).csv") |>
  select(
    title = Title,
    release_date = Release.Date,
    publisher = Publisher,
    developer = Developer,
    genre = Genres,
    user_score = User.Score,
    platform_origin = Platforms.Info
  ) |>
  mutate(
    release_date = mdy(release_date),
    year = year(release_date),
    genre = case_when(
      str_detect(genre, "RPG") ~ "RPG",
      str_detect(genre, "Adventure|Platform|Metroid|Point") ~ "Adventure",
      str_detect(genre, "Strategy|RTS|Tactic|MOBA|Defense") ~ "Strategy",
      str_detect(genre, "Racing") ~ "Racing",
      str_detect(genre, "Fight") ~ "Fighter",
      str_detect(genre, "Action|Beat|Arcade|Sandbox|Rougelike") ~ "Action",
      str_detect(genre, "Shoot|FPS|Gun|Artillery") ~ "Shooter",
      str_detect(genre, "Sim|Virtual|Manage|Tycoon|Visual Novel") ~ "Sim",
      str_detect(genre, "Puzzle|Hidden|Trivia|Edutainment") ~ "Puzzle",
      str_detect(genre, "Sport|Ball|ball|Golf|Ski|Wrest|Rugb|Hock|Billi|Tenn|Socc|Bik|Skat|Hunt|Surf|Crick|Bowl|Fish|Athlet|Gambl") ~ "Sport",
      str_detect(genre, "Danc|Party|Rhythm|Exercise") ~ "Party",
      str_detect(genre, "Survival") ~ "Survival",
      TRUE ~ "Other"
    ),
    platform = str_extract_all(platform_origin, "(?<='Platform': ').*?(?=',)"),
    metacritic_score = str_extract_all(platform_origin, "(?<='Platform Metascore': ').*?(?=',)"),
    critic_coverage = str_extract_all(platform_origin, "(?<='Based on ).*?(?= Critic Review)")
  ) |>
  unnest(
    c(platform, metacritic_score, critic_coverage)
  ) |>
  mutate(
    platform_condensed = case_when(
      str_detect(platform, "iOS") ~ "Phone",
      str_detect(platform, "Xbox") ~ "Xbox",
      str_detect(platform, "Playstation|PlayStation|PSP") ~ "Playstation",
      str_detect(platform, "Game|Wii|Nintendo|DS") ~ "Nintendo",
      str_detect(platform, "Dreamcast") ~ "Dreamcast",
      str_detect(platform, "PC") ~ "PC",
      str_detect(platform, "Meta Quest") ~ "Meta VR",
      TRUE ~ "Other"
    ),
    locality = case_when(
      str_detect(platform, "iOS|PSP|DS|Game Boy") ~ "Mobile",
      TRUE ~ "Home Console"
    ),
    exclusivity = case_when(
      duplicated(title) | duplicated(title, fromLast = TRUE) ~ "Multi-Platform",
      TRUE ~ "Exclusive"
    ),
    metacritic_score = as.numeric(metacritic_score),
    metacritic_score_factorized = case_when(
      metacritic_score <= 50 ~ "Poor",
      metacritic_score > 50 & metacritic_score <= 70 ~ "Average",
      metacritic_score > 70 & metacritic_score <= 90 ~ "Good",
      metacritic_score > 90 ~ "Excellent",
      TRUE ~ "Unscored"
    ),
    metacritic_score_factorized = factor(
      metacritic_score_factorized, 
      levels = c("Excellent", "Good", "Average", "Poor", "Unscored")
    ),
    critic_coverage = as.numeric(critic_coverage),
    user_score_factorized = case_when(
      user_score <= 5 ~ "Poor",
      user_score > 5 & user_score <= 7 ~ "Average",
      user_score > 7 & user_score <= 9 ~ "Good",
      user_score > 9 ~ "Excellent",
      TRUE ~ "Unscored"
    ),
    user_score_factorized = factor(
      user_score_factorized, 
      levels = c("Excellent", "Good", "Average", "Poor", "Unscored")
    ),
    user_metacritic_differential = metacritic_score - user_score * 10
  ) |>
  group_by(
    publisher
  ) |>
  mutate(
    total_developer_under_publisher = n_distinct(developer)
  ) |>
  ungroup() |>
  mutate(
    independance = case_when(
      total_developer_under_publisher == 1 ~ "Indie",
      TRUE ~ "Commercial"
    )
  ) |>
  filter(
    genre != "Other",
    year < 2024
  )

saveRDS(data, "video_game_analysis_2024.RDS")
