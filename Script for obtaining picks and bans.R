# Script for obtaining picks and bans
library(tidyverse)
library(httr)
library(jsonlite)


# TEST 1
api_base_url <- "https://lol.fandom.com/api.php"

# probbaly going to have get the picks and bans for each tournennamt again, since max is 500
games_table <- read_csv("games_transformed.csv", col_names = TRUE)

view(games_table)

# will be something similar to the way we got all games data
games_table$GameId[1]
games_list <- list()

### UNDER WORK, NEEDS TO BE TESTED
query_param <- list(
  action = "cargoquery",
  limit = '1',
  tables = "PicksAndBansS7=PB",
  fields = "PB.Team1Role1, PB.Team1Role2, PB.Team1Role3, PB.Team1Role4, PB.Team1Role5,
  PB.Team2Role1, PB.Team2Role2, PB.Team2Role3, PB.Team2Role4, PB.Team2Role5,
  PB.Team1Ban1, PB.Team1Ban2",
  where = paste0("PB.GameId=", "'",games_table$GameId[1], "'"),
  format = "json"
)

temp_api_data <- GET(api_base_url, query = query_param)

temp_api_content <- content(temp_api_data)

temp_api_content

content_df <- map_df(temp_api_content$cargoquery, ~as.data.frame(.x), .default = NA)
colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')

games_list[[i]] <- content_df


final_df <- bind_rows(games_list)

view(final_df)




#### so the above is using scoreboards7 plus the games table I have
tournaments_data <- read_csv("Tournaments.csv", col_names = TRUE, locale = readr::locale(encoding = "latin1"))

tournaments_data$Name[3]

dim(tournaments_data)


api_base_url <- "https://lol.fandom.com/api.php"

# will now try and do it with scoreboard games
query_param <- list(
  action = "cargoquery",
  limit = '10',
  tables = "PicksAndBansS7=PB, MatchScheduleGame=MS, ScoreboardGames=SG",
  fields = "SG.Tournament, SG.DateTime_UTC,SG.Team1, SG.Team2, SG.WinTeam, SG.Gamelength, SG.GameId, SG.MatchId, SG.Patch, SG.RiotPlatformGameId,
  Team1Role1, Team1Role2, Team1Role3, Team1Role4, Team1Role5,
  Team2Role1, Team2Role2, Team2Role3, Team2Role4, Team2Role5,
  Team1Ban1, Team1Ban2, Team1Ban3, Team1Ban4, Team1Ban5,
  Team2Ban1, Team2Ban2, Team2Ban3, Team2Ban4, Team2Ban5,
  Team1Pick1, Team1Pick2, Team1Pick3, Team1Pick4, Team1Pick5,
  Team2Pick1, Team2Pick2, Team2Pick3, Team2Pick4, Team2Pick5,
  Team1PicksByRoleOrder, Team2PicksByRoleOrder",
  where = paste0("SG.Tournament=", "'",tournaments_data$Name[3], "'"),
  join_on = "PB.GameId = MS.GameId, MS.GameId = SG.GameId",
  order_by = "SG.DateTime_UTC",
  format = "json")


temp_api_data <- GET(api_base_url, query = query_param)

temp_api_content <- content(temp_api_data)

temp_api_content



# Next: Run for all tournemanents.
