# processing V4 data - Version 1 - Stats Page
Libraries
library(tidyverse)
library(httr)
library(jsonlite)

#### reference data
item_helper <- read_csv("item_name_id.csv", col_names = TRUE)
summoner_spell_helper <- read_csv("summoner_spell_id_name.csv", col_names = TRUE)
games_table <- read_csv("games_transformed.csv", col_names = TRUE)
champ_names_df <- read_csv("champ_id_name.csv", col_names = TRUE)


### Input data (TO DO)



### TEST DATA


test_v4_stats_1 <- readRDS("Testv4_stats_1.RData")

test_v4_stats_2 <- readRDS("Testv4_stats_2.RData")

test_v4_stats_1$stats_title
test_v4_stats_2$stats_title

### Stats Data
## Participant