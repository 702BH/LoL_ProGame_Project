# processing V4 data - Version 1 - Stats Page
# Libraries
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



function_1_v4 <- function(raw_data){
  
  raw_data_file <- raw_data
  
  # parse json
  json_data <- fromJSON(raw_data$content)
  
  # Participants
  participants <- function_2_v4(json_data)
  
  # add useful relationship information
  participants <- function_3(raw_data_file, json_data, participants)
  
  # replace item ids with item names
  participants <- function_5(participants)
  
  # replace summoner spell ids with names
  participants <- function_6(participants)
  
  # replace champion ID with names
  participants <- function_3_v4(participants)
  
  # convert NA to 0
  participants <- function_7(participants)
  
  # convert logicals
  participants <- function_8(participants)
  
  #  seperate team name and player name
  participants <- function_4_v4(json_data, participants)
  
  return(participants)
  
}

test_v4_func <- function_1_v4(test_v4_stats_1)

test_v4_func$title


# get the participants structure
function_2_v4 <- function(data){
  
  participants <- data$participants
  
  participants <- participants %>%
    select(-c(timeline, participantId)) %>%
    unnest(stats)
  
  return(participants)
  

}


# replace champion id with names
function_3_v4 <- function(data){
  
  data$championId <- sapply(data$championId, function(col) unlist(champ_names_df[match(col, champ_names_df$key), "name"]))
  
  return(data)
}

#  seperate team name and player name
function_4_v4 <- function(json_data, processing_data){
  
  part_identities <- json_data$participantIdentities %>%
    unnest(player) %>%
    select(-profileIcon)
  
  sep_identities <- function_4(part_identities)
  
  identities_join <-left_join(processing_data, sep_identities, by = "participantId")
  
  return(identities_join)
  
}
