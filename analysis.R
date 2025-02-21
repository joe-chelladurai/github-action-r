library(tidyverse)
library(rvest)
library(janitor)
library(jsonlite)
url <- "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures"

# Function to retry with exponential backoff
retry_scrape <- function(url, max_attempts = 3, initial_delay = 5) {
  for (attempt in 1:max_attempts) {
    tryCatch({
      cat("Attempt", attempt, "of", max_attempts, "to scrape", url, "\n")
      result <- read_html(url)
      cat("Successfully scraped on attempt", attempt, "\n")
      return(result)
    }, error = function(e) {
      cat("Error on attempt", attempt, ":", conditionMessage(e), "\n")
      if (attempt < max_attempts) {
        delay <- initial_delay * 2^(attempt - 1)
        cat("Waiting", delay, "seconds before retrying...\n")
        Sys.sleep(delay)
      } else {
        stop("Failed all", max_attempts, "attempts to scrape", url)
      }
    })
  }
}

data <- retry_scrape(url)
table <- data |> html_table() |> pluck(1)
table <- clean_names(table)
table <- table |> 
  mutate(full_date = ymd(date)) |> 
  mutate(date2 = ymd(date)) |> 
  mutate(month = month(date2)) |> 
  mutate(date = day(date2)) |> 
  relocate(month, .after = date) |> select(-date2) |> 
  mutate(date_month = paste0(date, "/", month)) 
team_names <- tribble(
  ~team, ~short_name,
  "Manchester Utd", "MUN",
  "Ipswich Town", "IPS",
  "Newcastle Utd", "NEW",
  "Nott'ham Forest", "NOT",
  "Everton", "EVE",
  "Arsenal", "ARS",
  "West Ham", "WHU",
  "Brentford", "BRE",
  "Chelsea", "CHE",
  "Leicester City", "LEI",
  "Fulham", "FUL",
  "Liverpool", "LIV",
  "Southampton", "SOU",
  "Bournemouth", "BOU",
  "Brighton", "BRI",
  "Wolves", "WOL",
  "Aston Villa", "AVL",
  "Crystal Palace", "CRY",
  "Manchester City", "MCI",
  "Tottenham", "TOT"
)
home_games <- table |> 
  select(wk, date_month, home, away, score, full_date) |> 
  separate(score, into = c("home_score", "away_score")) |> 
  mutate(result = case_when(home_score > away_score ~ "W",
                            home_score == away_score ~ "D",
                            home_score < away_score ~ "L")) |> 
  rename(team = home, team_score = home_score) |> 
  rename(opp = away, opp_score = away_score) |> 
  mutate(venue = "H") 
away_games <- table |> 
  select(wk, date_month, home, away, score, full_date) |> 
  separate(score, into = c("home_score", "away_score")) |> 
  mutate(result = case_when(home_score > away_score ~ "L",
                            home_score == away_score ~ "D",
                            home_score < away_score ~ "W")) |> 
  rename(opp = home, opp_score = home_score) |> 
  rename(team = away, team_score = away_score) |> 
  mutate(venue = "A") 
all_games <- bind_rows(home_games, away_games) |> arrange(full_date) |> 
  mutate(team_score = replace_na(team_score, "")) |> 
  mutate(opp_score = replace_na(opp_score, "")) |> 
  mutate(score = paste0(team_score, "-", opp_score)) |> 
  filter(!is.na(wk)) |> 
  left_join(team_names, by = join_by(team)) |> 
  mutate(full_name = team) |>  mutate(team = short_name) |> select(-short_name) |> 
  left_join(team_names, by = c("opp" = "team")) |> mutate(opp = short_name) |> select(-short_name) |> 
  mutate(
    isHome = ifelse(venue == "H", TRUE, FALSE)  
  )
standings_url <- "https://fbref.com/en/comps/9/Premier-League-Stats"
standings_data <- retry_scrape(standings_url)
standings <- standings_data |> html_table() |> pluck(1) |> clean_names()
all_games <- all_games |> left_join(standings, by = c("full_name" = "squad"))
teams_json <- all_games |> 
  group_by(full_name, team, mp, rk, w, d, l, gf, ga, gd, pts, top_team_scorer) |> 
  nest() |> 
  ungroup() |> 
  mutate(name = full_name,
         team = team,
         rk = rk,
         mp = mp, 
         w = w, 
         d = d, 
         l = l,
         gf = gf, 
         ga = ga, 
         gd = gd, 
         pts = pts, 
         top_team_scorer = top_team_scorer, 
         form = map(data, function(team_data) {
           map(1:nrow(team_data), function(i) {
             list(
               date = team_data$date_month[i],
               result = team_data$result[i],
               opponent = team_data$opp[i],
               isHome = team_data$isHome[i],
               score = team_data$score[i]
             )
           })
         })) |> select(name, mp, w, d, l, gf, ga, gd, pts, top_team_scorer, form, rk) |> 
  arrange(rk)
json_output <- toJSON(teams_json, pretty = TRUE, auto_unbox = TRUE)
writeLines(json_output, "./data.json")
