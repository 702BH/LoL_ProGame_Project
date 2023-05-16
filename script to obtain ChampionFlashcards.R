# script to obtain ChampionFlashcards

setwd("C:/Users/house/Desktop/Getting into Modelling/Scripts")

library(tidyverse)
library(httr)
library(jsonlite)

# Metadata - Leagues
api_base_url <- "https://lol.fandom.com/api.php"

query_param <- list(
  action = "cargoquery",
  limit = 'max',
  tables = "ChampionFlashcards = CF",
  fields = "CF.Champion, CF.ChampionRange, CF.DamageType, CF.CCLevel, CF.BurstLevel,
  CF.SustainedLevel, CF.TankLevel, CF.Goal, CF.Strengths, CF.Weaknesses, CF.Ultimate,
  CF.Mechanic, CF.Classes, CF.Roles",
  format = "json"
)

league_api_data <- GET(api_base_url, query = query_param)


api_content <- content(league_api_data)

api_content$cargoquery
res <- map_depth(api_content$cargoquery, 3, ~ifelse(is.null(.x), NA, .x))

content_df <- map_df(res, ~as.data.frame(.x), .default = NA)
colnames(content_df) <- str_extract(colnames(content_df), '\\b\\w+$')

view(content_df)

write.csv(content_df, "champFlashCards.csv",  row.names = FALSE)
