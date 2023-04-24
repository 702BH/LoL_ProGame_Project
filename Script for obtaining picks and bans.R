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
  fields = "PB.Team1Role1",
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
